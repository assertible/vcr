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
import System.Directory

import Data.ByteString (ByteString)
import Network.HTTP.Client.Internal qualified as Client


import WebMock

import VCR

import           Test.HUnit.Lang

import           Data.Aeson

makeRequest :: String -> [Header] -> IO (Client.Response L.ByteString)
makeRequest url headers = do
  manager <- getGlobalManager
  request <- Client.parseUrlThrow url
  Client.httpLbs request {Client.requestHeaders = headers} manager

httpException :: Client.HttpException -> Bool
httpException _ = True

hUnitFailure :: FailureReason -> HUnitFailure -> Bool
hUnitFailure expectd (HUnitFailure _ actual) = expectd == actual

expectedButGot :: Request -> Request -> HUnitFailure -> Bool
expectedButGot expected actual= hUnitFailure (ExpectedButGot Nothing (show expected) (show actual))

infix 1 `shouldReturnBody`
infix 1 `shouldReturnStatus`

shouldReturnStatus :: HasCallStack => String -> Status -> IO ()
shouldReturnStatus url expected = do
  Client.responseStatus <$> makeRequest url [] `shouldReturn` expected

shouldReturnBody :: HasCallStack => String -> L.ByteString -> IO ()
shouldReturnBody url expected = do
  Client.responseBody <$> makeRequest url [] `shouldReturn` expected

tape :: Tape
tape = "tape.yaml"

data HTTPBin = HTTPBin {
  data_ :: String
} deriving (Eq, Show)

instance FromJSON HTTPBin where
  parseJSON = withObject "HTTPBin" \ o -> HTTPBin <$> o .: "data"

spec :: Spec
spec = around_ inTempDirectory $ do
  describe "withTape" do
    it "xxx requestBody" do
      withTape tape do
        manager <- getGlobalManager
        request <- Client.parseUrlThrow "http://httpbin.org/anything"

        _body :: IO ByteString <- Client.constBodyReader ["xxx Foo xxx\n"]

        let
          body = Client.RequestBodyStreamChunked bar

          bar :: (IO ByteString -> IO ()) -> IO ()
          bar action = action _body

        response <- Client.httpLbs request { Client.method = "POST", Client.requestBody = body } manager

        r :: HTTPBin <- throwDecode $ Client.responseBody response
        r.data_ `shouldBe` "xxx Foo xxx\n"

  describe "withTape" $ do
    context "with an Authorization header" $ do
      let
        headers :: [Header]
        headers = [(hAuthorization, "Bearer sk-RfAZfajzapKps4anC6ej8rhSnMxf5sLd")]

        request :: Request
        request = "http://httpbin.org/status/200" {requestHeaders = headers}

        response :: Response
        response = "" {responseStatus = status200}

      context "when mode is AnyOrder" $ do
        it "redacts the Authorization header" $ do
          mockRequest request response $ do
            withTape tape {mode = AnyOrder} $ do
              void $ makeRequest "http://httpbin.org/status/200" headers
          [Interaction recordedRequest _ ] <- loadTape tape.file
          requestHeaders recordedRequest `shouldBe` [(hAuthorization, "********")]

      context "when mode is Sequential" $ do
        it "redacts the Authorization header" $ do
          mockRequest request response $ do
            withTape tape {mode = Sequential} $ do
              void $ makeRequest "http://httpbin.org/status/200" headers
          [Interaction recordedRequest _ ] <- loadTape tape.file
          requestHeaders recordedRequest `shouldBe` [(hAuthorization, "********")]

    context "on exception" $ do
      it "writes tape" $ do
        mockRequest "http://httpbin.org/status/500" "" {responseStatus = status500} $ do
          withTape tape (makeRequest "http://httpbin.org/status/500" [])
            `shouldThrow` httpException
        doesFileExist tape.file `shouldReturn` True
