{-# LANGUAGE OverloadedStrings #-}
module WebMock.UtilSpec (spec) where

import Imports

import Test.Hspec
import Test.Mockery.Directory

import Data.ByteString.Lazy.Char8 qualified as L
import Data.ByteString.Lazy.Internal (defaultChunkSize)
import Network.HTTP.Client

import WebMock.Util

spec :: Spec
spec = around_ inTempDirectory do
  fdescribe "requestBodyToByteString" do
    describe "with RequestBodyLBS" do
      let body = RequestBodyLBS "foo"

      it "converts it to a ByteString" do
        snd <$> requestBodyToByteString body `shouldReturn` "foo"

      it "returns a copy of the original body" do
        (copy, bytes) <- requestBodyToByteString body
        snd <$> requestBodyToByteString copy `shouldReturn` bytes

    describe "with RequestBodyBS" do
      let body = RequestBodyBS "foo"

      it "converts it to a ByteString" do
        snd <$> requestBodyToByteString body `shouldReturn` "foo"

      it "returns a copy of the original body" do
        (copy, bytes) <- requestBodyToByteString body
        snd <$> requestBodyToByteString copy `shouldReturn` bytes

    describe "with RequestBodyBuilder" do
      let body = RequestBodyBuilder 3 "foo"

      it "converts it to a ByteString" do
        snd <$> requestBodyToByteString body `shouldReturn` "foo"

      it "returns a copy of the original body" do
        (copy, bytes) <- requestBodyToByteString body
        snd <$> requestBodyToByteString copy `shouldReturn` bytes

      context "with wrong length" do
        it "throws an exception" do
          snd <$> requestBodyToByteString (RequestBodyBuilder 5 "foo") `shouldThrow` anyException

    let file = "test.txt"
    let expected = L.fromChunks $ replicate defaultChunkSize $ "foobar"
    before_ (L.writeFile file expected) do
      context "with RequestBodyStream" do
        it "converts it to a ByteString" do
          body@(RequestBodyStream _ _) <- streamFile file
          snd <$> requestBodyToByteString body `shouldReturn` expected

        it "returns a copy of the original body" do
          body@(RequestBodyStream _ _) <- streamFile file
          (copy, bytes) <- requestBodyToByteString body
          snd <$> requestBodyToByteString copy `shouldReturn` bytes

        context "with wrong length" do
          it "throws an exception" do
            RequestBodyStream n body <- streamFile file
            snd <$> requestBodyToByteString (RequestBodyStream (pred n) body) `shouldThrow` anyException

      context "with RequestBodyStreamChunked" do
        it "converts it to a ByteString" do
          (RequestBodyStream _ body) <- streamFile file
          snd <$> requestBodyToByteString (RequestBodyStreamChunked body) `shouldReturn` expected

        it "returns a copy of the original body" do
          (RequestBodyStream _ body) <- streamFile file
          (copy, bytes) <- requestBodyToByteString (RequestBodyStreamChunked body)
          snd <$> requestBodyToByteString copy `shouldReturn` bytes
