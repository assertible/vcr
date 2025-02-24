{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
module VCRSpec (spec) where

import Imports

import Test.Hspec
import Test.Mockery.Directory

import Data.ByteString.Lazy qualified as L
import Network.HTTP.Client qualified as Client
import Network.HTTP.Client.TLS (getGlobalManager)
import Network.HTTP.Types

import WebMock

import VCR

makeRequest :: String -> [Header] -> IO (Client.Response L.ByteString)
makeRequest url headers = do
  manager <- getGlobalManager
  request <- Client.parseUrlThrow url
  Client.httpLbs request { Client.requestHeaders = headers } manager

httpException :: Client.HttpException -> Bool
httpException _ = True

infix 1 `shouldReturnStatus`

shouldReturnStatus :: HasCallStack => String -> Status -> IO ()
shouldReturnStatus url expected = do
  Client.responseStatus <$> makeRequest url [] `shouldReturn` expected

tape :: Tape
tape = "tape.yaml"

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

spec :: Spec
spec = around_ inTempDirectory do
  describe "withTape" do
    context "when mode is AnyOrder" do
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

      context "on exception" do
        it "writes the tape to disk" do
          mockRequest "http://httpbin.org/status/500" "" { responseStatus = status500 } do
            withTape tape (makeRequest "http://httpbin.org/status/500" [])
              `shouldThrow` httpException
          length <$> loadTape tape.file `shouldReturn` 1

    context "with an Authorization header" do
      context "when mode is AnyOrder" do
        it "redacts the Authorization header" do
          mockRequest authRequest "" do
            withTape tape do
              makeAuthRequest
          loadTape tape.file `shouldReturn` [(redactedAuthRequest, "")]

      context "when mode is Sequential" do
        it "redacts the Authorization header" do
          mockRequest authRequest "" do
            withTape tape { mode = Sequential } do
              makeAuthRequest
          loadTape tape.file `shouldReturn` [(redactedAuthRequest, "")]

  describe "playTape" do
    context "when mode is AnyOrder" do
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
