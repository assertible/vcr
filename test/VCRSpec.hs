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

import WebMock

import VCR

makeRequest :: String -> [Header] -> IO (Client.Response L.ByteString)
makeRequest url headers = do
  manager <- getGlobalManager
  request <- Client.parseUrlThrow url
  Client.httpLbs request {Client.requestHeaders = headers} manager

httpException :: Client.HttpException -> Bool
httpException _ = True

tape :: Tape
tape = "tape.yaml"

spec :: Spec
spec = around_ inTempDirectory do
  describe "withTape" do
    context "with an Authorization header" do
      let
        headers :: [Header]
        headers = [(hAuthorization, "Bearer sk-RfAZfajzapKps4anC6ej8rhSnMxf5sLd")]

        request :: Request
        request = "http://httpbin.org/status/200" {requestHeaders = headers}

      context "when mode is AnyOrder" do
        it "redacts the Authorization header" do
          mockRequest request "" do
            withTape tape {mode = AnyOrder} do
              void $ makeRequest "http://httpbin.org/status/200" headers
          [(recordedRequest, _)] <- loadTape tape.file
          requestHeaders recordedRequest `shouldBe` [(hAuthorization, "********")]

      context "when mode is Sequential" do
        it "redacts the Authorization header" do
          mockRequest request "" do
            withTape tape {mode = Sequential} do
              void $ makeRequest "http://httpbin.org/status/200" headers
          [(recordedRequest, _)] <- loadTape tape.file
          requestHeaders recordedRequest `shouldBe` [(hAuthorization, "********")]

    context "on exception" do
      it "writes tape" do
        mockRequest "http://httpbin.org/status/500" "" {responseStatus = status500} do
          withTape tape (makeRequest "http://httpbin.org/status/500" [])
            `shouldThrow` httpException
        doesFileExist tape.file `shouldReturn` True
