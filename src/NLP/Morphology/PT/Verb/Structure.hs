{-# OPTIONS_GHC -Wno-partial-fields #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module NLP.Morphology.PT.Verb.Structure
  ( VerbStructure(..)
  , VerbParameters(..)
  ) where

import           Data.Aeson             (FromJSON, ToJSON)
import           GHC.Generics           (Generic)
import           NLP.Morphology.PT.Core (Aspect, Citation, Gender, Mood,
                                         MoodTense, Number, Person,
                                         PersonNumber, Polarity, Root, Tense,
                                         ThematicVowel, Voice)

data VerbParameters = VParams
  { citation :: Citation
  , voice    :: Voice
  , mood     :: Mood
  , aspect   :: Aspect
  , tense    :: Tense
  , person   :: Person
  , number   :: Number
  , gender   :: Gender
  , polarity :: Polarity
  } deriving (Show, Eq, Ord, Generic, FromJSON, ToJSON)

data VerbStructure
  = VS0
  | VS3
      { vs3Root          :: Root
      , vs3ThematicVowel :: ThematicVowel
      , vs3MoodTense     :: MoodTense
      }
  | VS4
      { vs4Root          :: Root
      , vs4ThematicVowel :: ThematicVowel
      , vs4MoodTense     :: MoodTense
      , vs4PersonNumber  :: PersonNumber
      }
  | VS5
      { vs5Root          :: Root
      , vs5ThematicVowel :: ThematicVowel
      , vs5MoodTense     :: MoodTense
      , vs5Gender        :: Gender
      , vs5Number        :: Number
      }
  | VS34
      { vs34VS3 :: VerbStructure
      , vs34VS4 :: VerbStructure
      }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)
