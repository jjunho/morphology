{-# LANGUAGE FlexibleInstances #-}

module NLP.Morphology.PT.Txt
  ( Txt(..)
  ) where

import           Data.List                        (intercalate)
import           NLP.Morphology.PT.Core
import           NLP.Morphology.PT.Verb.Core
import           NLP.Morphology.PT.Verb.Paradigm
import           NLP.Morphology.PT.Verb.Structure

class Txt a where
  txt :: a -> String

instance Txt Char where
  txt = pure

instance Txt String where
  txt = id

instance Txt Root where
  txt (Root r) = r

instance Txt ThematicVowel where
  txt tv =
    case tv of
      A' -> "A"
      E' -> "E"
      I' -> "I"
      O' -> "O"
      U' -> "U"
      Z' -> "∅"

instance Txt MoodTense where
  txt = show

instance Txt PersonNumber where
  txt = show

instance Txt Gender where
  txt = show

instance Txt Number where
  txt = show

instance Txt VerbStructure where
  txt VS0               = "×"
  txt (VS3 r tv mt)     = intercalate "-" [txt r, txt tv, txt mt]
  txt (VS4 r tv mt pn)  = intercalate "-" [txt r, txt tv, txt mt, txt pn]
  txt (VS5 r tv mt g n) = intercalate "-" [txt r, txt tv, txt mt, txt g, txt n]
  txt (VS34 v3 v4)      = intercalate "+" [txt v3, txt v4]

instance Txt Morpheme where
  txt m =
    case m of
      X        -> "×"
      Z        -> "∅"
      Morph m' -> m'
      _        -> show m

instance Txt Morphemes where
  txt (M0)        = "×"
  txt (M3 ms)     = intercalate "-" $ map txt ms
  txt (M4 ms)     = intercalate "-" $ map txt ms
  txt (M5 ms)     = intercalate "-" $ map txt ms
  txt (M34 m1 m2) = intercalate "+" [txt m1, txt m2]

instance Txt a => Txt (Either String a) where
  txt (Left e)  = "Erro: " <> e
  txt (Right a) = txt a

instance Txt a => Txt (TenseTable a) where
  txt (TenseTable _ tt) = intercalate "\n" $ fmap txt tt

instance Txt a => Txt (Paradigm a) where
  txt (Paradigm _ tts) = intercalate "\n\n" $ fmap txt tts
