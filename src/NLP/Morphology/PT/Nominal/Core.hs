module NLP.Morphology.PT.Nominal.Core
  ( Nominal(..)
  ) where

import           NLP.Morphology.PT.Core

data Nominal = N
  { root          :: Root
  , thematicVowel :: ThematicVowel
  , number        :: Number
  }
