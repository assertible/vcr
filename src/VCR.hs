{-# LANGUAGE CPP #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
module VCR (
  Tape(..)
, Mode(..)
, withTape
, recordTape
, playTape
#ifdef TEST
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
import System.IO.Error
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

data OpenTape mode = OpenTape {
  file :: FilePath
, interactions :: InteractionsFor mode
, redact :: Request -> Request
}

type family InteractionsFor (mode :: Mode)

type instance InteractionsFor AnyOrder = [(Request, Response)]

type instance InteractionsFor Sequential = [(Request, Response)]

redactAuthorization :: Request -> Request
redactAuthorization request@Request{..} = request { requestHeaders = map redact requestHeaders }
  where
    redact :: Header -> Header
    redact header@(name, _)
      | name == hAuthorization = (name, "********")
      | otherwise = header

withTape :: HasCallStack => Tape -> IO a -> IO a
withTape tape action = case tape.mode of
  AnyOrder -> openTape'AnyOrder tape >>= flip modeAnyOrder action
  Sequential -> openTape'Sequential tape >>= flip modeSequential action

modeAnyOrder :: OpenTape AnyOrder -> IO a -> IO a
modeAnyOrder tape action = do
  captureInteractions tape.redact save $ mockInteractions tape.redact tape.interactions action
  where
    save :: [(Request, Response)] -> IO ()
    save newInteractions = unless (null newInteractions) do
      closeTape'AnyOrder tape { interactions = tape.interactions ++ newInteractions }

modeSequential :: HasCallStack => OpenTape Sequential -> IO a -> IO a
modeSequential tape action = case (not . null) tape.interactions of
  False -> record tape.redact tape.file action
  True -> do
    playInOrder tape.redact tape.interactions action

recordTape :: Tape -> IO a -> IO a
recordTape (Tape file _ redact) = record redact file

record :: (Request -> Request) -> FilePath -> IO a -> IO a
record redact = captureInteractions redact . saveTape

captureInteractions :: (Request -> Request) -> ([(Request, Response)] -> IO ()) -> IO a -> IO a
captureInteractions redact consumeCaptured action = do

  ref <- newIORef []

  let
    capture :: IO Response -> Request -> IO Response
    capture makeRequest request = do
      response <- makeRequest
      captureInteraction (request, response)
      return response

    captureInteraction :: (Request, Response) -> IO ()
    captureInteraction x = atomicModifyIORef' ref \ xs -> (x : xs, ())

    getCaptured :: IO [(Request, Response)]
    getCaptured = reverse <$> atomicReadIORef ref

  withRequestAction redact capture action `finally` (consumeCaptured =<< getCaptured)

playTape :: HasCallStack => Tape -> IO a -> IO a
playTape (Tape file mode redact) action = do
  interactions <- loadTape file
  case mode of
    AnyOrder -> mockInteractions redact interactions action
    Sequential -> playInOrder redact interactions action

mockInteractions :: (Request -> Request) -> [(Request, Response)] -> IO a -> IO a
mockInteractions redact = go
  where
    go :: [(Request, Response)] -> IO a -> IO a
    go interactions = withRequestAction redact \ makeRequest request -> do
      case lookup request interactions of
        Just response -> return response
        Nothing -> makeRequest

playInOrder :: HasCallStack => (Request -> Request) -> [(Request, Response)] -> IO a -> IO a
playInOrder redact = mockRequestChain . map toRequestAction
  where
    toRequestAction :: (Request, Response) -> Request -> IO Response
    toRequestAction (request, response) = mkRequestAction request response . redact

withRequestAction :: (Request -> Request) -> (IO Response -> Request -> IO Response) -> IO a -> IO a
withRequestAction redact requestAction = WebMock.withRequestAction
  \ makeRequest request -> requestAction makeRequest (redact request)

openTape'AnyOrder :: Tape -> IO (OpenTape AnyOrder)
openTape'AnyOrder Tape{..} = do
  interactions <- tryLoadTape file
  return OpenTape {
    file
  , interactions
  , redact
  }

closeTape'AnyOrder :: OpenTape AnyOrder -> IO ()
closeTape'AnyOrder tape = saveTape tape.file tape.interactions

openTape'Sequential :: Tape -> IO (OpenTape Sequential)
openTape'Sequential Tape{..} = do
  interactions <- tryLoadTape file
  return OpenTape {
    file
  , interactions
  , redact
  }

saveTape :: FilePath -> [(Request, Response)] -> IO ()
saveTape file interactions = do
  ensureDirectory file
  B.writeFile file $ Yaml.encodePretty conf (toInteractions interactions)
  where
    conf = Yaml.setConfCompare (comparing f) Yaml.defConfig

    f :: Text -> Int
    f name = fromMaybe maxBound (lookup name fieldOrder)

    toInteractions :: [(Request, Response)] -> [Interaction]
    toInteractions = map \ (request, response) -> (Interaction request response)

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

tryLoadTape :: FilePath -> IO [(Request, Response)]
tryLoadTape file = either (const []) id <$> tryJust (guard . isDoesNotExistError) (loadTape file)

loadTape :: FilePath -> IO [(Request, Response)]
loadTape file = B.readFile file >>= fmap fromInteractions . Yaml.decodeThrow
  where
    fromInteractions :: [Interaction] -> [(Request, Response)]
    fromInteractions = map \ (Interaction request response) -> (request, response)

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
