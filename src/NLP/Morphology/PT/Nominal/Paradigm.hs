{-# OPTIONS_GHC -Wno-partial-fields #-}

module NLP.Morphology.PT.Nominal.Paradigm
  ( NominalParadigm(..)
  , NomForm(..)
  ) where

import           NLP.Morphology.PT.Core

data NominalParadigm
  = NounParadigm
      { citation :: Citation
      , forms    :: [NomForm]
      }
  | AdjectiveParadigm
      { citation    :: Citation
      , forms       :: [NomForm]
      , comparative :: [NomForm]
      }
  | PersonalPronounParadigm
      { citation :: Citation
      , forms    :: [NomForm]
      }
  | DemonstrativePronounParadigm
      { citation :: Citation
      , forms    :: [NomForm]
      }
  deriving (Show, Eq, Ord)

data NomForm
  = NomForm
      { root          :: Root
      , thematicVowel :: ThematicVowel
      , number        :: Number
      , gender        :: Gender
      }
  | PersonalForm
      { root          :: Root
      , thematicVowel :: ThematicVowel
      , number        :: Number
      , person        :: Person
      , kase          :: Case
      }
  deriving (Show, Eq, Ord)
