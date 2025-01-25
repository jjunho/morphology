{-# LANGUAGE DeriveGeneric #-}

module ApiResponse
  ( ApiResponse(..)
  , ApiError(..)
  , ParadigmInput(..)
  ) where

import           Data.Aeson   (FromJSON, ToJSON)
import           Data.Text    (Text)
import           GHC.Generics (Generic)

-- API Response Types
data ApiResponse a = ApiResponse
  { status  :: Text
  , message :: Text
  , data'   :: Maybe a
  } deriving (Show, Generic)

instance ToJSON a => ToJSON (ApiResponse a)

instance FromJSON a => FromJSON (ApiResponse a)

-- Error Handling
data ApiError = ApiError
  { error :: Text
  } deriving (Show, Generic)

instance ToJSON ApiError

instance FromJSON ApiError

-- JSON Input for Paradigm Creation
data ParadigmInput = ParadigmInput
  { citation :: Text
  } deriving (Show, Generic)

instance FromJSON ParadigmInput
