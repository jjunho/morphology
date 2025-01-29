{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor  #-}
{-# LANGUAGE DeriveGeneric  #-}

module NLP.Morphology.PT.Verb.Paradigm
  ( TenseTable(..)
  , Paradigm(..)
  ) where

import           Data.Aeson             (FromJSON, ToJSON)
import           GHC.Generics           (Generic)
import           NLP.Morphology.PT.Core (Citation, MoodTense)

data TenseTable a = TenseTable
  { tenseTable :: MoodTense
  , tenseForms :: [a]
  } deriving (Show, Eq, Ord, Functor, Generic, ToJSON, FromJSON)

data Paradigm a = Paradigm
  { citationForm :: Citation
  , tenseTables  :: [TenseTable a]
  } deriving (Show, Eq, Ord, Functor, Generic, ToJSON, FromJSON)
