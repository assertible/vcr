{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
module VCR (
  Tape(..)
, Mode(..)
, withTape
, recordTape
, playTape
) where

import           Control.Exception
import           Control.Monad
import           Data.Text (Text)
import           Data.Ord
import           Data.Maybe
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.CaseInsensitive as CI
import           Data.IORef
import           Data.String
import           Data.Yaml
import qualified Data.Yaml as Yaml
import qualified Data.Yaml.Pretty as Yaml
import           GHC.Stack (HasCallStack)
import           System.Directory
import           System.FilePath

import           WebMock

data Tape = Tape {
  tapeFile :: FilePath
, tapeMode :: Mode
} deriving (Eq, Show)

instance IsString Tape where
    fromString file = Tape file AnyOrder

data Mode = Sequential | AnyOrder
    deriving (Eq, Show)

withTape :: HasCallStack => Tape -> IO a -> IO a
withTape (Tape file mode) = case mode of
    AnyOrder -> modeAnyOrder file
    Sequential -> modeSequential file

modeAnyOrder :: FilePath -> IO a -> IO a
modeAnyOrder file action = do
    interactions <- doesFileExist file >>= \ case
        False -> return []
        True -> loadTape file

    let save newInteractions = unless (null newInteractions) $ do
            saveTape file (interactions ++ newInteractions)

    captureInteractions save $ mockInteractions interactions action

modeSequential :: HasCallStack => FilePath -> IO a -> IO a
modeSequential file action = doesFileExist file >>= \ case
    False -> record file action
    True -> do
        interactions <- loadTape file
        playInOrder interactions action

recordTape :: Tape -> IO a -> IO a
recordTape (Tape file _) = record file

record :: FilePath -> IO a -> IO a
record = captureInteractions . saveTape

captureInteractions :: ([Interaction] -> IO ()) -> IO a -> IO a
captureInteractions consumeCaptured action = do

    ref <- newIORef []

    let capture :: RequestAction -> RequestAction
        capture makeRequest request manager = do
            (r, response) <- do
                (req, response) <- makeRequest request manager
                simpleResponse <- toSimpleResponse response
                return (req, simpleResponse)
            captureInteraction =<< Interaction <$> toSimpleRequest request <*> pure response
            (,) r <$> fromSimpleResponse request response

        captureInteraction :: Interaction -> IO ()
        captureInteraction x = atomicModifyIORef ref $ \ xs -> (x : xs, ())

        getCaptured :: IO [Interaction]
        getCaptured = reverse <$> readIORef ref

    withRequestAction capture action `finally` (consumeCaptured =<< getCaptured)

playTape :: HasCallStack => Tape -> IO a -> IO a
playTape (Tape file mode) action = do
    interactions <- loadTape file
    case mode of
        AnyOrder -> mockInteractions interactions action
        Sequential -> playInOrder interactions action

mockInteractions :: [Interaction] -> IO a -> IO a
mockInteractions = go
  where
    go :: [Interaction] -> IO a -> IO a
    go (map fromInteraction -> interactions) = withRequestAction $ \ makeRequest clientRequest manager -> do
        request <- toSimpleRequest clientRequest
        case lookup request interactions of
            Just response -> (,) clientRequest <$> fromSimpleResponse clientRequest response
            Nothing -> makeRequest clientRequest manager

    fromInteraction :: Interaction -> (Request, Response)
    fromInteraction (Interaction request response) = (request, response)

playInOrder :: HasCallStack => [Interaction] -> IO a -> IO a
playInOrder = mockRequestChain . map toRequestAction
  where
    toRequestAction :: Interaction -> Request -> IO Response
    toRequestAction (Interaction request response) = mkRequestAction request response

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
    parseJSON = withObject "Interaction" $ \ o -> Interaction
        <$> (o .: "request" >>= requestFromJSON)
        <*> (o .: "response" >>= responseFromJSON)
      where
        requestFromJSON :: Value -> Parser Request
        requestFromJSON = withObject "Request" $ \ o ->
            Request
            <$> (B.pack <$> o .: "method")
            <*> o .: "url"
            <*> (o .: "headers" >>= headersFromJSON)
            <*> (L.pack <$> o .: "body")

        responseFromJSON :: Value -> Parser Response
        responseFromJSON = withObject "Response" $ \ o -> Response
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
