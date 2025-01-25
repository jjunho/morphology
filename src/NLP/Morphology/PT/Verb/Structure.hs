{-# OPTIONS_GHC -Wno-partial-fields #-}

module NLP.Morphology.PT.Verb.Structure
  ( VerbStructure(..)
  , root
  , thematicVowel
  , moodTense
  , personNumber
  , gender
  , number
  ) where

import           NLP.Morphology.PT.Core (Gender, MoodTense, Number,
                                         PersonNumber, Root, ThematicVowel)

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
  deriving (Show, Eq, Ord)

root :: VerbStructure -> Maybe Root
root (VS3 r _ _)     = Just r
root (VS4 r _ _ _)   = Just r
root (VS5 r _ _ _ _) = Just r
root _               = Nothing

thematicVowel :: VerbStructure -> Maybe ThematicVowel
thematicVowel (VS3 _ tv _)     = Just tv
thematicVowel (VS4 _ tv _ _)   = Just tv
thematicVowel (VS5 _ tv _ _ _) = Just tv
thematicVowel _                = Nothing

moodTense :: VerbStructure -> Maybe MoodTense
moodTense (VS3 _ _ mt)     = Just mt
moodTense (VS4 _ _ mt _)   = Just mt
moodTense (VS5 _ _ mt _ _) = Just mt
moodTense _                = Nothing

personNumber :: VerbStructure -> Maybe PersonNumber
personNumber (VS4 _ _ _ pn) = Just pn
personNumber _              = Nothing

gender :: VerbStructure -> Maybe Gender
gender (VS5 _ _ _ g _) = Just g
gender _               = Nothing

number :: VerbStructure -> Maybe Number
number (VS5 _ _ _ _ n) = Just n
number _               = Nothing
