{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module WebMock (
  Request (..)
, Response (..)
, disableRequests
, unsafeMockRequest
, mockRequest
, mockRequestChain
, mkRequestActions
, mkRequestAction
, module Network.HTTP.Types

, RequestAction
, withRequestAction
, protectRequestAction

, toSimpleRequest
, fromSimpleResponse
, toSimpleResponse
) where

import           Control.Exception
import           Control.Monad
import qualified Data.ByteString.Lazy as L
import           Data.IORef
import           Data.String
import qualified Data.Text as T
import           Data.Text.Encoding (encodeUtf8)
import           GHC.Stack (HasCallStack)
import           Network.HTTP.Client.Internal (Manager, BodyReader)
import qualified Network.HTTP.Client.Internal as Client
import           Network.HTTP.Types
import           Network.URI (uriToString)
import           Test.HUnit

import           WebMock.Util

data Request = Request {
  requestMethod  :: Method
, requestUrl     :: String
, requestHeaders :: RequestHeaders
, requestBody    :: L.ByteString
} deriving Eq

instance IsString Request where
  fromString url = Request "GET" url [] ""

instance Show Request where
  show Request{..} = unlines [
      "Request {"
    , "  requestMethod = " ++ show requestMethod
    , ", requestUrl = " ++ show requestUrl
    , ", requestHeaders = " ++ show requestHeaders
    , ", requestBody = " ++ show requestBody
    , "}"
    ]

data Response = Response {
  responseStatus  :: Status
, responseHeaders :: ResponseHeaders
, responseBody    :: L.ByteString
} deriving (Eq, Show)

instance IsString Response where
  fromString body = Response status200 [] (L.fromStrict $ encodeUtf8 $ T.pack body)

unsafeMockRequest :: (Request -> IO Response) -> IO ()
unsafeMockRequest f = writeIORef Client.requestAction requestAction
  where
    requestAction request _manager = do
      (,) request <$> (toSimpleRequest request >>= f >>= fromSimpleResponse request)

mockRequest :: HasCallStack => Request -> Response -> IO a -> IO a
mockRequest expectedRequest response action = protectRequestAction $ do
  unsafeMockRequest $ \ request -> do
    request @?= expectedRequest
    return response
  action

disableRequests :: HasCallStack => IO a -> IO a
disableRequests action = do
  let
    requestAction :: RequestAction -> RequestAction
    requestAction _makeRequest clientRequest = do
      request <- toSimpleRequest clientRequest
      unexpectedRequest request
  withRequestAction requestAction action

mockRequestChain :: HasCallStack => [Request -> IO Response] -> IO a -> IO a
mockRequestChain xs action = do
  ref <- newIORef xs

  let
    requestAction :: a -> RequestAction
    requestAction _ clientRequest = do
      request <- toSimpleRequest clientRequest
      readIORef ref >>= \ case
        z:zs -> do
          writeIORef ref zs
          z request >>= fromSimpleResponse clientRequest
        [] -> unexpectedRequest request

    checkLeftover :: IO ()
    checkLeftover = do
      leftover <- length <$> readIORef ref
      when (leftover /= 0) $ do
        let
          total = length xs
          actual = total - leftover
        assertFailure $ "Expected " ++ show total ++ " requests, but only received " ++ show actual ++ "!"

  withRequestAction requestAction action <* checkLeftover

unexpectedRequest :: Request -> IO a
unexpectedRequest request = assertFailure $ "Unexpected HTTP request: " ++ show request

mkRequestActions :: HasCallStack => [(Request, Response)] -> [Request -> IO Response]
mkRequestActions = map (uncurry mkRequestAction)

mkRequestAction :: HasCallStack => Request -> Response -> Request -> IO Response
mkRequestAction expected response actual = do
  actual @?= expected
  return response

type RequestAction = Client.Request -> IO (Client.Response BodyReader)

type ClientRequestAction = Client.Request -> Manager -> IO (Client.Request, Client.Response BodyReader)

withRequestAction :: (RequestAction -> RequestAction) -> IO a -> IO a
withRequestAction action = bracket setup restore . const
  where
    lift :: ClientRequestAction -> ClientRequestAction
    lift f request manager = do
      (,) request <$> makeRequest request
      where
        makeRequest :: RequestAction
        makeRequest = action (fmap snd . flip f manager)

    setup :: IO ClientRequestAction
    setup = atomicModifyIORef Client.requestAction $ \ old -> (lift old, old)

    restore :: ClientRequestAction -> IO ()
    restore = writeIORef Client.requestAction

protectRequestAction :: IO a -> IO a
protectRequestAction = withRequestAction id

toSimpleRequest :: Client.Request -> IO Request
toSimpleRequest r = do
  body <- requestBodyToByteString (Client.requestBody r)
  return $ Request {
    requestMethod = Client.method r
  , requestUrl = uriToString id (Client.getUri r) ""
  , requestHeaders = Client.requestHeaders r
  , requestBody = body
  }

toSimpleResponse :: Client.Response BodyReader -> IO Response
toSimpleResponse r = do
  c <- Client.brConsume (Client.responseBody r)
  Client.responseClose r
  return $ Response {
    responseStatus = Client.responseStatus r
  , responseHeaders = Client.responseHeaders r
  , responseBody = L.fromChunks c
  }

fromSimpleResponse :: Client.Request -> Response -> IO (Client.Response BodyReader)
fromSimpleResponse request Response{..} = do
  body <- Client.constBodyReader (L.toChunks responseBody)
  return $ Client.Response {
    Client.responseStatus = responseStatus
  , Client.responseVersion = http11
  , Client.responseHeaders = responseHeaders
  , Client.responseBody = body
  , Client.responseCookieJar = mempty
  , Client.responseClose' = Client.ResponseClose $ return ()
  , Client.responseOriginalRequest = request { Client.requestBody = mempty }
  , Client.responseEarlyHints = mempty
  }
