{-# LANGUAGE OverloadedStrings #-}

module Handlers
  ( createParadigmHandler
  ) where

import           ApiResponse
import           Control.Monad.IO.Class    (liftIO)
import           Data.Text                 (Text, pack, unpack)
import           Network.HTTP.Types.Status (status422)
import qualified System.Log.FastLogger     as FL
import qualified Web.Scotty                as S

import           NLP.Morphology.PT.Verb    (Txt (txt), mkParadigm)

-- Handlers
createParadigmHandler :: FL.LoggerSet -> S.ActionM ()
createParadigmHandler logger = do
  input <- S.jsonData :: S.ActionM ParadigmInput
  let citationText = citation input
  liftIO
    $ FL.pushLogStrLn logger
    $ FL.toLogStr
    $ "Received citation: " ++ unpack citationText
  case mkParadigm (unpack citationText) of
    Left err -> do
      S.status status422 -- Correct usage without 'S.'
      S.json (ApiResponse "error" (pack err) Nothing :: ApiResponse Text)
    Right paradigm ->
      S.json
        (ApiResponse "ok" "Paradigm generated" (Just (pack $ txt paradigm)) :: ApiResponse
           Text)
