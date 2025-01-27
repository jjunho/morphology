{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad.IO.Class      (liftIO)
import           Data.Aeson                  (FromJSON, ToJSON)
import           Data.Text                   (Text, pack, unpack)
import           Data.Time                   (defaultTimeLocale, formatTime,
                                              getCurrentTime)
import           GHC.Generics                (Generic)
import           Network.Wai                 (Middleware)
import           Network.Wai.Middleware.Cors (CorsResourcePolicy (..), cors,
                                              simpleCorsResourcePolicy)
import qualified System.Log.FastLogger       as FL
import           Web.Scotty                  (ActionM, ScottyM, get, json,
                                              middleware, param, scotty)

import           NLP.Morphology.PT.Verb      (Citation, Paradigm, TenseTable,
                                              VerbStructure, mkParadigm,
                                              mkTense)

-- Definição do tipo de resposta padrão
data ApiResponse a = ApiResponse
  { status  :: Text
  , message :: Text
  , content :: Maybe a
  } deriving (Generic, Show)

instance ToJSON a => ToJSON (ApiResponse a)

instance FromJSON a => FromJSON (ApiResponse a)

-- Configuração do Middleware CORS
corsMiddleware :: Middleware
corsMiddleware = cors (const $ Just corsPolicy)

corsPolicy :: CorsResourcePolicy
corsPolicy =
  simpleCorsResourcePolicy
    { corsOrigins = Just (["http://localhost:1234"], True)
    , corsMethods = ["GET", "POST", "OPTIONS"]
    , corsRequestHeaders = ["Content-Type"]
    }

-- Inicialização do Logger
initLogger :: IO FL.LoggerSet
initLogger = FL.newStdoutLoggerSet FL.defaultBufSize

flushLogger :: FL.LoggerSet -> IO ()
flushLogger = FL.flushLogStr

logRequest :: FL.LoggerSet -> Text -> Text -> Maybe Text -> IO ()
logRequest logger endpoint citation tense = do
  currentTime <- getCurrentTime
  let isoTime =
        pack $ formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" currentTime
  let tensePart = maybe "" ("/" <>) tense
  let logLine = isoTime <> " - " <> endpoint <> "/" <> citation <> tensePart
  FL.pushLogStrLn logger (FL.toLogStr logLine)
  flushLogger logger

-- Rotas da API
defineRoutes :: FL.LoggerSet -> ScottyM ()
defineRoutes logger = do
  get "/morphology/v1/health" $ do
    liftIO $ logRequest logger "GET /api/v1/health" "" Nothing
    json $ ApiResponse "ok" "API is running" (Nothing :: Maybe Text)
  get "/morphology/v1/paradigm/:citation" $ createParadigmHandler logger
  get "/morphology/v1/tense_paradigm/:citation/:tense"
    $ createTenseParadigmHandler logger

-- Handler para /paradigm
createParadigmHandler :: FL.LoggerSet -> ActionM ()
createParadigmHandler logger = do
  citation <- param "citation"
  liftIO $ logRequest logger "GET /api/v1/paradigm" citation Nothing
  case mkParadigm (unpack citation) of
    Left e ->
      json
        $ ApiResponse
            "error"
            (pack e)
            (Nothing :: Maybe (Paradigm VerbStructure))
    Right p -> json $ ApiResponse "ok" "" (Just p)

-- Handler para /tense_paradigm
createTenseParadigmHandler :: FL.LoggerSet -> ActionM ()
createTenseParadigmHandler logger = do
  citation <- param "citation"
  tense <- param "tense"
  liftIO $ logRequest logger "GET /api/v1/tense_paradigm" citation (Just tense)
  case mkTense (unpack citation) (read $ unpack tense) of
    Left e ->
      json
        $ ApiResponse
            "error"
            (pack e)
            (Nothing :: Maybe (TenseTable VerbStructure))
    Right p -> json $ ApiResponse "ok" "" (Just p)

-- Função principal
main :: IO ()
main = do
  logger <- initLogger
  putStrLn "Starting Morphology API on http://0.0.0.0:3000..."
  scotty 3000 $ do
    middleware corsMiddleware
    defineRoutes logger
