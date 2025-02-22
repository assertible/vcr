{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
module VCR (
  Tape(..)
, Mode(..)
, withTape
, recordTape
, playTape
#ifdef TEST
, Interaction(..)
, loadTape
#endif
) where

import Imports

import Control.Exception
import Data.Text (Text)
import Data.Ord
import Data.ByteString.Char8 qualified as B
import Data.ByteString.Lazy.Char8 qualified as L
import Data.CaseInsensitive qualified as CI
import Data.String
import Data.Yaml
import Data.Yaml qualified as Yaml
import Data.Yaml.Pretty qualified as Yaml
import GHC.Stack (HasCallStack)
import System.Directory
import System.FilePath

import WebMock hiding (withRequestAction)
import WebMock qualified

data Tape = Tape {
  file :: FilePath
, mode :: Mode
, redact :: Request -> Request
}

instance IsString Tape where
  fromString file = Tape file AnyOrder redactAuthorization

data Mode = Sequential | AnyOrder
  deriving (Eq, Show)

redactAuthorization :: Request -> Request
redactAuthorization request@Request{..} = request { requestHeaders = map redact requestHeaders }
  where
    redact :: Header -> Header
    redact header@(name, _)
      | name == hAuthorization = (name, "********")
      | otherwise = header

withTape :: HasCallStack => Tape -> IO a -> IO a
withTape (Tape file mode redact) = case mode of
  AnyOrder -> modeAnyOrder redact file
  Sequential -> modeSequential redact file

modeAnyOrder :: (Request -> Request) -> FilePath -> IO a -> IO a
modeAnyOrder redact file action = do
  interactions <- doesFileExist file >>= \ case
    False -> return []
    True -> loadTape file

  let
    save newInteractions = unless (null newInteractions) do
      saveTape file (interactions ++ newInteractions)

  captureInteractions redact save $ mockInteractions redact interactions action

modeSequential :: HasCallStack => (Request -> Request) -> FilePath -> IO a -> IO a
modeSequential redact file action = doesFileExist file >>= \ case
  False -> record redact file action
  True -> do
    interactions <- loadTape file
    playInOrder redact interactions action

recordTape :: Tape -> IO a -> IO a
recordTape (Tape file _ redact) = record redact file

record :: (Request -> Request) -> FilePath -> IO a -> IO a
record redact = captureInteractions redact . saveTape

captureInteractions :: (Request -> Request) -> ([Interaction] -> IO ()) -> IO a -> IO a
captureInteractions redact consumeCaptured action = do

  ref <- newIORef []

  let
    capture :: IO Response -> Request -> IO Response
    capture makeRequest request = do
      response <- makeRequest
      captureInteraction $ Interaction request response
      return response

    captureInteraction :: Interaction -> IO ()
    captureInteraction x = atomicModifyIORef' ref \ xs -> (x : xs, ())

    getCaptured :: IO [Interaction]
    getCaptured = reverse <$> atomicReadIORef ref

  withRequestAction redact capture action `finally` (consumeCaptured =<< getCaptured)

playTape :: HasCallStack => Tape -> IO a -> IO a
playTape (Tape file mode redact) action = do
  interactions <- loadTape file
  case mode of
    AnyOrder -> mockInteractions redact interactions action
    Sequential -> playInOrder redact interactions action

mockInteractions :: (Request -> Request) -> [Interaction] -> IO a -> IO a
mockInteractions redact = go
  where
    go :: [Interaction] -> IO a -> IO a
    go (map fromInteraction -> interactions) = withRequestAction redact \ makeRequest request -> do
      case lookup request interactions of
        Just response -> return response
        Nothing -> makeRequest

    fromInteraction :: Interaction -> (Request, Response)
    fromInteraction (Interaction request response) = (request, response)

playInOrder :: HasCallStack => (Request -> Request) -> [Interaction] -> IO a -> IO a
playInOrder redact = mockRequestChain . map toRequestAction
  where
    toRequestAction :: Interaction -> Request -> IO Response
    toRequestAction (Interaction request response) = mkRequestAction request response . redact

withRequestAction :: (Request -> Request) -> (IO Response -> Request -> IO Response) -> IO a -> IO a
withRequestAction redact requestAction = WebMock.withRequestAction
  \ makeRequest request -> requestAction makeRequest (redact request)

saveTape :: FilePath -> [Interaction] -> IO ()
saveTape file interactions = do
  ensureDirectory file
  B.writeFile file $ Yaml.encodePretty conf interactions
  where
    conf = Yaml.setConfCompare (comparing f) Yaml.defConfig

    f :: Text -> Int
    f name = fromMaybe maxBound (lookup name fieldOrder)

fieldOrder :: [(Text, Int)]
fieldOrder = flip zip [1..] [
    "request"
  , "response"

  , "method"
  , "url"

  , "status"
  , "headers"
  , "body"

  , "code"
  , "message"

  , "name"
  , "value"
  ]

ensureDirectory :: FilePath -> IO ()
ensureDirectory = createDirectoryIfMissing True . takeDirectory

loadTape :: FilePath -> IO [Interaction]
loadTape file = Yaml.decodeFileThrow file

data Interaction = Interaction Request Response

instance ToJSON Interaction where
  toJSON (Interaction request response) = object [
        "request" .= requestToJSON request
      , "response" .= responseToJSON response
      ]
    where
      requestToJSON :: Request -> Value
      requestToJSON Request{..} = object [
          "method" .= B.unpack requestMethod
        , "url" .= requestUrl
        , "headers" .= headersToJSON requestHeaders
        , "body" .= L.unpack requestBody
        ]

      responseToJSON :: Response -> Value
      responseToJSON Response{..} = object [
          "status" .= statusToJSON responseStatus
        , "headers" .= headersToJSON responseHeaders
        , "body" .= L.unpack responseBody
        ]
        where
          statusToJSON :: Status -> Value
          statusToJSON (Status code message) = object [
              "code" .= code
            , "message" .= B.unpack message
            ]

      headersToJSON :: RequestHeaders -> Value
      headersToJSON = toJSON . map headerToJSON
        where
          headerToJSON :: Header -> Value
          headerToJSON (name, value) = object [
              "name" .= B.unpack (CI.original name)
            , "value" .= B.unpack value
            ]

instance FromJSON Interaction where
  parseJSON = withObject "Interaction" \ o -> Interaction
    <$> (o .: "request" >>= requestFromJSON)
    <*> (o .: "response" >>= responseFromJSON)
    where
      requestFromJSON :: Value -> Parser Request
      requestFromJSON = withObject "Request" \ o ->
        Request
        <$> (B.pack <$> o .: "method")
        <*> o .: "url"
        <*> (o .: "headers" >>= headersFromJSON)
        <*> (L.pack <$> o .: "body")

      responseFromJSON :: Value -> Parser Response
      responseFromJSON = withObject "Response" \ o -> Response
        <$> (o .: "status" >>= statusFromJSON)
        <*> (o .: "headers" >>= headersFromJSON)
        <*> (L.pack <$> o .: "body")
        where
          statusFromJSON :: Object -> Parser Status
          statusFromJSON o = Status
            <$> (o .: "code")
            <*> (B.pack <$> o .: "message")

      headersFromJSON :: [Object] -> Parser RequestHeaders
      headersFromJSON = mapM headerFromJSON
        where
          headerFromJSON :: Object -> Parser Header
          headerFromJSON o = (,)
            <$> (CI.mk . B.pack <$> o .: "name")
            <*> (B.pack <$> o .: "value")
