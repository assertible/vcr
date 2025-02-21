{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module WebMock.Util (requestBodyToByteString) where

import           Data.ByteString (ByteString)
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as L
import           Data.Int
import           Data.IORef
import           Network.HTTP.Client.Internal

requestBodyToByteString :: RequestBody -> IO L.ByteString
requestBodyToByteString = \ case
  RequestBodyLBS body -> return body
  RequestBodyBS body -> return (L.fromStrict body)
  RequestBodyBuilder n builder -> checkLength n (Builder.toLazyByteString builder)
  RequestBodyStream n stream -> streamToByteString stream >>= checkLength n
  RequestBodyStreamChunked stream -> streamToByteString stream
  RequestBodyIO body -> body >>= requestBodyToByteString

streamToByteString :: GivesPopper () -> IO L.ByteString
streamToByteString givesPopper = do
  ref <- newIORef undefined
  givesPopper (go [] ref)
  readIORef ref
  where
    go :: [ByteString] -> IORef L.ByteString -> Popper -> IO ()
    go xs ref get = get >>= \ case
      "" -> writeIORef ref (L.fromChunks $ reverse xs)
      x -> go (x : xs) ref get

checkLength :: Int64 -> L.ByteString -> IO L.ByteString
checkLength n xs
  | n == len = return xs
  | otherwise = throwHttp $ WrongRequestBodyStreamSize (fromIntegral len) (fromIntegral n)
  where
    len = L.length xs
