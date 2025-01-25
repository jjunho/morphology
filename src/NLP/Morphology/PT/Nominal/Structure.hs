{-# LANGUAGE DeriveFunctor #-}

module NLP.Morphology.PT.Nominal.Structure
  ( NominalStructure(..)
  ) where

import           NLP.Morphology.PT.Core

data NominalStructure a = NounStructure
  { root          :: Root
  , thematicVowel :: ThematicVowel
  , number        :: Number
  } deriving (Show, Eq, Ord, Functor)
