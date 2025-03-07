{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
module VCRSpec (spec) where

import Imports

import Test.Hspec
import Test.HUnit.Lang
import Test.Mockery.Directory
import System.Timeout

import Control.Concurrent
import Control.Concurrent.Async
import Network.HTTP.Client qualified as Client
import Network.HTTP.Client.TLS (getGlobalManager)
import Network.HTTP.Types

import WebMock

import VCR

makeRequest :: String -> [Header] -> IO (Client.Response LazyByteString)
makeRequest url headers = do
  manager <- getGlobalManager
  request <- Client.parseUrlThrow url
  Client.httpLbs request { Client.requestHeaders = headers } manager

httpException :: Client.HttpException -> Bool
httpException _ = True

hUnitFailure :: FailureReason -> HUnitFailure -> Bool
hUnitFailure expectd (HUnitFailure _ actual) = expectd == actual

expectedButGot :: Request -> Request -> HUnitFailure -> Bool
expectedButGot expected actual= hUnitFailure (ExpectedButGot Nothing (show expected) (show actual))

infix 1 `shouldReturnStatus`
infix 1 `shouldReturnBody`

shouldReturnStatus :: HasCallStack => String -> Status -> IO ()
shouldReturnStatus url expected = do
  Client.responseStatus <$> makeRequest url [] `shouldReturn` expected

shouldReturnBody :: HasCallStack => String -> LazyByteString -> IO ()
shouldReturnBody url expected = do
  Client.responseBody <$> makeRequest url [] `shouldReturn` expected

authRequest :: Request
authRequest = "http://httpbin.org/status/200" {
  requestHeaders = [(hAuthorization, "Bearer sk-RfAZfajzapKps4anC6ej8rhSnMxf5sLd")]
}

redactedAuthRequest :: Request
redactedAuthRequest = authRequest {
  requestHeaders = [(hAuthorization, "********")]
}

makeAuthRequest :: IO ()
makeAuthRequest = void $ makeRequest authRequest.requestUrl authRequest.requestHeaders

respondWith :: Status -> Request -> IO Response
respondWith responseStatus _ = return $ "" { responseStatus }

spec :: Spec
spec = around_ inTempDirectory do
  describe "withTape" do
    context "when mode is AnyOrder" do
      let tape = "tape.yaml"

      it "records the first request to a resource, replays subsequent requests" do
        mockRequestChain [\ _ -> return ""] do
          withTape tape do
            "http://httpbin.org/status/200" `shouldReturnStatus` status200
            "http://httpbin.org/status/200" `shouldReturnStatus` status200
            "http://httpbin.org/status/200" `shouldReturnStatus` status200
        length <$> loadTape tape.file `shouldReturn` 1

      it "records the requests in the order they were made" do
        mockRequestChain [respondWith status200, respondWith status202, respondWith status201] do
          withTape tape do
            "http://httpbin.org/status/200" `shouldReturnStatus` status200
            "http://httpbin.org/status/202" `shouldReturnStatus` status202
            "http://httpbin.org/status/201" `shouldReturnStatus` status201
        loadTape tape.file `shouldReturn` [
            ("http://httpbin.org/status/200", "" { responseStatus = status200 })
          , ("http://httpbin.org/status/202", "" { responseStatus = status202 })
          , ("http://httpbin.org/status/201", "" { responseStatus = status201 })
          ]

      context "with an existing tape" do
        it "replays existing requests from the tape" do
          mockRequest "http://httpbin.org/status/200" "" do
            withTape tape do
              "http://httpbin.org/status/200" `shouldReturnStatus` status200
          disableRequests do
            withTape tape do
              "http://httpbin.org/status/200" `shouldReturnStatus` status200
          length <$> loadTape tape.file `shouldReturn` 1

        it "records new requests to the tape" do
            mockRequest "http://httpbin.org/status/200" "" do
              withTape tape do
                "http://httpbin.org/status/200" `shouldReturnStatus` status200
            mockRequest "http://httpbin.org/status/201" "" { responseStatus = status201 } do
              withTape tape do
                "http://httpbin.org/status/201" `shouldReturnStatus` status201
            length <$> loadTape tape.file `shouldReturn` 2

      context "with concurrent requests" do
        it "records the requests concurrently, without blocking" do
          maybe (expectationFailure "<<timeout>>") return <=< timeout 100_000 . protectRequestAction $ do
            unsafeMockRequest (\ _ -> threadDelay 10_000 >> return "")
            withTape tape do
              forConcurrently_ [200 .. 299 :: Int] $ \ status -> do
                "http://httpbin.org/status/" <> show status `shouldReturnBody` ""
          length <$> loadTape tape.file `shouldReturn` 100

      context "on exception" do
        it "writes the tape to disk" do
          mockRequest "http://httpbin.org/status/500" "" { responseStatus = status500 } do
            withTape tape (makeRequest "http://httpbin.org/status/500" [])
              `shouldThrow` httpException
          length <$> loadTape tape.file `shouldReturn` 1

      context "with an Authorization header" do
        it "redacts the Authorization header" do
          mockRequest authRequest "" do
            withTape tape makeAuthRequest
          loadTape tape.file `shouldReturn` [(redactedAuthRequest, "")]

    context "when mode is Sequential" do
      let tape = "tape.yaml" { mode = Sequential }

      it "records all requests to a resource" do
        mockRequest "http://httpbin.org/status/200" "" do
          withTape tape do
            "http://httpbin.org/status/200" `shouldReturnStatus` status200
            "http://httpbin.org/status/200" `shouldReturnStatus` status200
            "http://httpbin.org/status/200" `shouldReturnStatus` status200
        length <$> loadTape tape.file `shouldReturn` 3

      context "with an existing tape" do
        it "replays request in order" do
          mockRequestChain [respondWith status200, respondWith status202, respondWith status201] do
            withTape tape do
              "http://httpbin.org/status/200" `shouldReturnStatus` status200
              "http://httpbin.org/status/202" `shouldReturnStatus` status202
              "http://httpbin.org/status/201" `shouldReturnStatus` status201
          disableRequests $ do
            withTape tape do
              "http://httpbin.org/status/200" `shouldReturnStatus` status200
              "http://httpbin.org/status/202" `shouldReturnStatus` status202
              "http://httpbin.org/status/201" `shouldReturnStatus` status201
          loadTape tape.file `shouldReturn` [
              ("http://httpbin.org/status/200", "" { responseStatus = status200 })
            , ("http://httpbin.org/status/202", "" { responseStatus = status202 })
            , ("http://httpbin.org/status/201", "" { responseStatus = status201 })
            ]

        context "with an out of order request" do
          it "fails" do
            mockRequestChain [respondWith status200, respondWith status201, respondWith status202] do
              withTape tape do
                "http://httpbin.org/status/200" `shouldReturnStatus` status200
                "http://httpbin.org/status/201" `shouldReturnStatus` status201
                "http://httpbin.org/status/202" `shouldReturnStatus` status202
            disableRequests $ do
              withTape tape do
                "http://httpbin.org/status/200" `shouldReturnStatus` status200
                "http://httpbin.org/status/202" `shouldReturnStatus` status200
              `shouldThrow`
                  expectedButGot
                    (Request "GET" "http://httpbin.org/status/201" [] "")
                    (Request "GET" "http://httpbin.org/status/202" [] "")

        context "with a missing request" do
          it "fails" do
            mockRequestChain [respondWith status200, respondWith status201, respondWith status202] do
              withTape tape do
                "http://httpbin.org/status/200" `shouldReturnStatus` status200
                "http://httpbin.org/status/201" `shouldReturnStatus` status201
                "http://httpbin.org/status/202" `shouldReturnStatus` status202
            disableRequests $ do
              withTape tape do
                "http://httpbin.org/status/200" `shouldReturnStatus` status200
                "http://httpbin.org/status/201" `shouldReturnStatus` status201
              `shouldThrow` hUnitFailure (Reason "Expected 3 requests, but only received 2!")

        context "with additional requests" do
          it "records additional requests" do
            mockRequestChain [respondWith status200, respondWith status201, respondWith status202] do
              withTape tape do
                "http://httpbin.org/status/200" `shouldReturnStatus` status200
                "http://httpbin.org/status/201" `shouldReturnStatus` status201
              withTape tape do
                "http://httpbin.org/status/200" `shouldReturnStatus` status200
                "http://httpbin.org/status/201" `shouldReturnStatus` status201
                "http://httpbin.org/status/202" `shouldReturnStatus` status202
              loadTape tape.file `shouldReturn` [
                  ("http://httpbin.org/status/200", "" { responseStatus = status200 })
                , ("http://httpbin.org/status/201", "" { responseStatus = status201 })
                , ("http://httpbin.org/status/202", "" { responseStatus = status202 })
                ]

      context "on exception" do
        it "writes the tape to disk" do
          mockRequest "http://httpbin.org/status/500" "" { responseStatus = status500 } do
            withTape tape (makeRequest "http://httpbin.org/status/500" [])
              `shouldThrow` httpException
          length <$> loadTape tape.file `shouldReturn` 1

      context "with an Authorization header" do
        it "redacts the Authorization header" do
          mockRequest authRequest "" do
            withTape tape makeAuthRequest
          loadTape tape.file `shouldReturn` [(redactedAuthRequest, "")]

  describe "playTape" do
    context "when mode is AnyOrder" do
      let tape = "tape.yaml"

      it "replays existing requests from the tape" do
        mockRequest "http://httpbin.org/status/200" "" do
          recordTape tape do
            "http://httpbin.org/status/200" `shouldReturnStatus` status200
        disableRequests do
          playTape tape do
            "http://httpbin.org/status/200" `shouldReturnStatus` status200
        length <$> loadTape tape.file `shouldReturn` 1

      it "does not record any new requests to the tape" do
          mockRequest "http://httpbin.org/status/200" "" do
            recordTape tape do
              "http://httpbin.org/status/200" `shouldReturnStatus` status200
          mockRequest "http://httpbin.org/status/201" "" { responseStatus = status201 } do
            playTape tape do
              "http://httpbin.org/status/201" `shouldReturnStatus` status201
          length <$> loadTape tape.file `shouldReturn` 1
