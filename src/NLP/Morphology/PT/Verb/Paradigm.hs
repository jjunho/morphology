{-# LANGUAGE DeriveFunctor #-}

module NLP.Morphology.PT.Verb.Paradigm
  ( TenseTable(..)
  , Paradigm(..)
  ) where

import           NLP.Morphology.PT.Core (Citation, MoodTense)

data TenseTable a = TenseTable
  { tenseTable :: MoodTense
  , tenseForms :: [a]
  } deriving (Show, Eq, Ord, Functor)

data Paradigm a = Paradigm
  { citation    :: Citation
  , tenseTables :: [TenseTable a]
  } deriving (Show, Eq, Ord, Functor)
