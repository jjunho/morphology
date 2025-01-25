{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson             (FromJSON, ToJSON, decode, encode)
import           Data.Text              (Text, pack)
import qualified System.Log.FastLogger  as FL
import           Web.Scotty

import           NLP.Morphology.PT.Core (Citation)
import           NLP.Morphology.PT.Verb (Paradigm, Txt (txt), VerbStructure,
                                         mkParadigm)

import           ApiResponse
import           Handlers

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
  scotty 3000 (defineRoutes logger)
  -- API Request Examples
  -- curl -X POST http://localhost:3000/paradigm \
  --   -H "Content-Type: application/json" \
  --   -d '{"citation":"falar"}'
