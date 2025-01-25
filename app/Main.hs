{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad.IO.Class      (liftIO)
import           Data.Aeson                  (FromJSON, ToJSON, decode, encode)
import           Data.Text                   (Text, pack)
import qualified System.Log.FastLogger       as FL
import           Web.Scotty

import           NLP.Morphology.PT.Core      (Citation)
import           NLP.Morphology.PT.Verb      (Paradigm, Txt (txt),
                                              VerbStructure, mkParadigm)

import           ApiResponse
import           Handlers
import           Network.Wai                 (Middleware)
import           Network.Wai.Middleware.Cors
import           Network.Wai.Middleware.Cors (CorsResourcePolicy (..), cors,
                                              simpleCorsResourcePolicy)

-- Middleware para CORS
corsMiddleware :: Middleware
corsMiddleware = cors (const $ Just corsPolicy)

-- Configuração do CORS
corsPolicy :: CorsResourcePolicy
corsPolicy =
  simpleCorsResourcePolicy
    { corsOrigins = Just (["http://localhost:1234"], True) -- Permitindo localhost:1234
    , corsMethods = ["GET", "POST", "OPTIONS"] -- Métodos permitidos
    , corsRequestHeaders = ["Content-Type"] -- Cabeçalhos permitidos
    }

-- Initialize Logger
initLogger :: IO FL.LoggerSet
initLogger = FL.newStdoutLoggerSet FL.defaultBufSize

-- Application Main
defineRoutes :: FL.LoggerSet -> ScottyM ()
defineRoutes logger = do
  -- Update routes with /api/v1/ prefix
  get "/api/v1/health" $ do
    json (ApiResponse "ok" "API is running" Nothing :: ApiResponse Text)
  post "/api/v1/paradigm" $ createParadigmHandler logger

main :: IO ()
main = do
  logger <- initLogger
  putStrLn "Starting Morphology API on http://0.0.0.0:3000..."
  scotty 3000 $ do
    middleware corsMiddleware
    defineRoutes logger
  -- API Request Examples
  -- curl -X POST http://localhost:3000/paradigm \
  --   -H "Content-Type: application/json" \
  --   -d '{"citation":"falar"}'
