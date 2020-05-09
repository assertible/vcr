{-# LANGUAGE OverloadedStrings #-}
module VCRSpec (spec) where

import           Test.Hspec
import           Test.Mockery.Directory

import qualified Data.ByteString.Lazy as L
import qualified Network.HTTP.Client as Client
import           Network.HTTP.Client.TLS (getGlobalManager)
import           Network.HTTP.Types
import           System.Directory

import           WebMock

import           VCR

makeRequest :: String -> IO (Client.Response L.ByteString)
makeRequest url = do
    manager <- getGlobalManager
    request <- Client.parseUrlThrow url
    Client.httpLbs request manager

httpException :: Client.HttpException -> Bool
httpException _ = True

spec :: Spec
spec = around_ inTempDirectory $ do
    describe "withTape" $ do
        context "on exception" $ do
            it "writes tape" $ do
                mockRequest "http://httpbin.org/status/500" "" {responseStatus = status500} $ do
                    withTape "tape.yaml" (makeRequest "http://httpbin.org/status/500")
                        `shouldThrow` httpException
                    doesFileExist "tape.yaml" `shouldReturn` True
