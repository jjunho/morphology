{-|

GET /api/v1/health

  Retorna um JSON com o status da API.

  Este endpoint é usado para verificar se a API está operacional.
  Pode ser utilizado em rotinas de monitoramento para determinar se o servidor
  está funcionando corretamente.

  Estrutura da resposta:
    - status: String - Representa o estado atual da resposta da API, como "ok" ou "error".
    - message: String - Mensagem complementar ou informativa sobre o resultado da requisição.
    - content: Object - Dados principais retornados pela API. Neste endpoint, será um objeto vazio ({}).

  Possíveis valores para o campo "status":
    - "ok": A API está funcionando normalmente.
    - "error": Ocorreu um problema na API.

  Exemplo de resposta:
  {
    "status": "ok",
    "message": "API is running",
    "content": {}
  }

GET /api/v1/paradigm/:citation

  Retorna um JSON com o paradigma do verbo para a forma de citação informada.

  Um paradigma verbal é o conjunto de todas as formas conjugadas de um verbo,
  organizado de acordo com os tempos, modos e pessoas gramaticais. Este conceito
  é amplamente utilizado em linguística e no ensino de línguas para mostrar como
  um verbo se flexiona em diferentes contextos.

  Este endpoint é usado para recuperar informações sobre o paradigma verbal
  de um verbo específico. Útil em aplicativos que precisam manipular
  flexões verbais, como conjugadores automáticos ou editores de texto linguístico.

  Estrutura da resposta:
    - status: String - Representa o estado atual da resposta da API, como "ok" ou "error".
    - message: String - Mensagem complementar ou informativa sobre o resultado da requisição.
    - content: Object - Contém os dados principais retornados pela API, neste caso, o paradigma verbal solicitado.

  Parâmetro:
    - :citation: Forma de citação do verbo (por exemplo, "correr", "amar").

  Exemplo prático:
    - Requisição: GET /api/v1/paradigm/correr

  Exemplo de resposta bem-sucedida:
  {
    "status": "ok",
    "message": "",
    "content": { PARADIGMA }
  }

  Exemplo de resposta com erro:
  {
    "status": "error",
    "message": MENSAGEM DE ERRO,
    "content": {}
  }

### GET /api/v1/tense_paradigm/:citation/:tense
  Retorna um JSON com o paradigma do verbo para a forma de citação informada no tempo verbal informado.

  Este endpoint é usado para recuperar informações sobre o paradigma verbal de um verbo específico em um tempo verbal específico.

  Estrutura da resposta:
    - status: String - Representa o estado atual da resposta da API, como "ok" ou "error".
    - message: String - Mensagem complementar ou informativa sobre o resultado da requisição.
    - content: Object - Contém os dados principais retornados pela API, neste caso, o paradigma verbal solicitado.

  Parâmetros:
    - :citation: Forma de citação do verbo (por exemplo, "correr", "amar").
    - :tense: Tempo verbal.

  Possíveis valores para o campo "tense":
    - "IPRS": Presente do Indicativo
    - "IPRF": Pretérito Perfeito do Indicativo
    - "IIPF": Pretérito Imperfeito do Indicativo
    - "IPPF": Pretérito Mais-que-perfeito do Indicativo
    - "IFUT": Futuro do Indicativo
    - "COND": Condicional Presente
    - "SPRS": Presente do Subjuntivo
    - "SPIF": Pretérito Imperfeito do Subjuntivo
    - "SFUT": Futuro do Subjuntivo
    - "IMPA": Imperativo Afirmativo
    - "IMPN": Imperativo Negativo
    - "INFP": Infinitivo Pessoal
    - "INF": Infinitivo
    - "GER": Gerúndio
    - "PPP": Particípio Passado Passivo

  Exemplo prático:
    - Requisição: GET /api/v1/tense_paradigm/correr/IPRS

  Exemplo de resposta bem-sucedida:
  {
    "status": "ok",
    "message": "",
    "content": { PARADIGMA }
  }

  Exemplo de resposta com erro:
  {
    "status": "error",
    "message": MENSAGEM DE ERRO,
    "content": {}
  }
-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad.IO.Class      (liftIO)
import           Data.Aeson                  (FromJSON, ToJSON)
import           Data.Text                   (Text, pack, unpack)
import           Data.Time                   (defaultTimeLocale, formatTime,
                                              getCurrentTime, utcToZonedTime)
import           Data.Time.Clock.POSIX       (posixSecondsToUTCTime)
import           GHC.Generics                (Generic)
import           Network.Wai                 (Middleware)
import           Network.Wai.Middleware.Cors (CorsResourcePolicy (..), cors,
                                              simpleCorsResourcePolicy)
import qualified System.Log.FastLogger       as FL
import           Web.Scotty                  (ActionM, ScottyM, get, json,
                                              middleware, param, scotty)

import           NLP.Morphology.PT.Verb      (Citation, Paradigm, TenseTable,
                                              Txt (txt), VerbStructure,
                                              mkParadigm, mkTense)

-- Define ApiResponse type
data ApiResponse a = ApiResponse
  { status  :: Text
  , message :: Text
  , content :: Maybe a
  } deriving (Generic, Show)

instance ToJSON a => ToJSON (ApiResponse a)

instance FromJSON a => FromJSON (ApiResponse a)

-- Middleware para CORS
corsMiddleware :: Middleware
corsMiddleware = cors (const $ Just corsPolicy)

-- Configuração do CORS
corsPolicy :: CorsResourcePolicy
corsPolicy =
  simpleCorsResourcePolicy
    { corsOrigins = Just (["http://localhost:1234"], True) -- Permitindo localhost : 1234
    , corsMethods = ["GET", "POST", "OPTIONS"] -- Métodos permitidos
    , corsRequestHeaders = ["Content-Type"] -- Cabeçalhos permitidos
    }

-- Initialize Logger
initLogger :: IO FL.LoggerSet
initLogger = FL.newStdoutLoggerSet FL.defaultBufSize

-- Flush Logger
flushLogger :: FL.LoggerSet -> IO ()
flushLogger = FL.flushLogStr

-- Define loggerLine function
loggerLine :: Text -> Text -> Text -> IO Text
loggerLine url citation tense = do
  currentTime <- getCurrentTime
  let isoTime =
        pack $ formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" currentTime
  let logLine =
        case url of
          "GET /api/v1/health" -> isoTime <> " - " <> url <> "/"
          "GET /api/v1/paradigm" -> isoTime <> " - " <> url <> "/" <> citation
          "GET /api/v1/tense_paradigm" ->
            isoTime <> " - " <> url <> "/" <> citation <> "/" <> tense
          _ -> isoTime <> " - " <> url
  return logLine

-- Application Main
defineRoutes :: FL.LoggerSet -> ScottyM ()
defineRoutes logger = do
  get "/morphology/v1/health" $ do
    logLine <- liftIO $ loggerLine "GET /api/v1/health" "" ""
    liftIO $ FL.pushLogStrLn logger (FL.toLogStr logLine)
    liftIO $ flushLogger logger
    json (ApiResponse "ok" "API is running" Nothing :: ApiResponse Text)
  get "/morphology/v1/paradigm/:citation" $ createParadigmHandler logger
  get "/morphology/v1/tense_paradigm/:citation/:tense"
    $ createTenseParadigmHandler logger

createParadigmHandler :: FL.LoggerSet -> ActionM ()
createParadigmHandler logger = do
  citation <- param "citation"
  logLine <- liftIO $ loggerLine "GET /api/v1/paradigm" citation ""
  liftIO $ FL.pushLogStrLn logger (FL.toLogStr logLine)
  liftIO $ flushLogger logger
  let paradigm = mkParadigm (unpack citation)
  case paradigm of
    Left e -> json (ApiResponse "error" (pack e) Nothing :: ApiResponse Text)
    Right p ->
      json
        (ApiResponse "ok" "" (Just p) :: ApiResponse (Paradigm VerbStructure))

createTenseParadigmHandler :: FL.LoggerSet -> ActionM ()
createTenseParadigmHandler logger = do
  citation <- param "citation" :: ActionM Text
  tense <- param "tense" :: ActionM Text
  logLine <- liftIO $ loggerLine "GET /api/v1/tense_paradigm" citation tense
  liftIO $ FL.pushLogStrLn logger (FL.toLogStr logLine)
  liftIO $ flushLogger logger
  let tenseTable = mkTense (unpack citation) (read $ unpack tense)
  case tenseTable of
    Left e -> json (ApiResponse "error" (pack e) Nothing :: ApiResponse Text)
    Right p ->
      json
        (ApiResponse "ok" "" (Just p) :: ApiResponse (TenseTable VerbStructure))

main :: IO ()
main = do
  logger <- initLogger
  putStrLn "Starting Morphology API on http://0.0.0.0:3000..."
  scotty 3000 $ do
    middleware corsMiddleware
    defineRoutes logger
