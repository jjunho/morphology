{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module NLP.Morphology.PT.Verb
  ( Citation
  , Root(..)
  , ThematicVowel(..)
  , VerbStructure(..)
  , MoodTense(..)
  , PersonNumber(..)
  , Number(..)
  , Gender(..)
  , Morph(..)
  , Morphs(..)
  , TenseTable(..)
  , Paradigm(..)
  , Txt(..)
  , Parameter(..)
  , Voice(..)
  , Mood(..)
  , Aspect(..)
  , Tense(..)
  , Polarity(..)
  , Person(..)
  , Case(..)
  , VerbParameters(..)
  , -- Ensure this is exported
    suppletive
  , deep
  , shallow
  , orth
  , mkVerb
  , mkVerbParameters
  , mkVS3
  , mkVS4
  , mkVS5
  , mkCitation
  , mkRoot
  , mkThematicVowel
  , mkTheme
  , mkTense
  , mkParadigm
  ) where

import           Data.Char                        (toUpper)
import           NLP.Morphology.PT.Core
import           NLP.Morphology.PT.Txt
import           NLP.Morphology.PT.Verb.Core
import           NLP.Morphology.PT.Verb.Paradigm
import           NLP.Morphology.PT.Verb.Structure

mkCitation :: Citation -> Either String String
mkCitation c
  | length c' < 2 = Left $ "Citation form must have at least 2 characters" <> c'
  | last c' /= 'R' = Left $ "Citation form must end with 'R'" <> c'
  | otherwise = Right c'
  where
    c' = map toUpper c

mkRoot :: Citation -> Either String Root
mkRoot c = do
  c' <- mkCitation c
  return $ Root $ init $ init c'

mkThematicVowel :: Citation -> Either String ThematicVowel
mkThematicVowel c =
  mkCitation c >>= \c' ->
    case last (init c') of
      'A' -> Right A'
      'E' -> Right E'
      'I' -> Right I'
      'O' -> Right O'
      'Ô' -> Right O'
      'U' -> Right U'
      _   -> Right Z'

mkTheme :: Citation -> Either String (Root, ThematicVowel)
mkTheme c = do
  let c' = map toUpper c
  r <- mkRoot c'
  tv <- mkThematicVowel c'
  return (r, tv)

mkVS3 :: Citation -> MoodTense -> Either String VerbStructure
mkVS3 c mt
  | not $ mt `elem` [INF, GER] = Left "Invalid MoodTense: must be INF or GER"
  | otherwise = do
    (r, tv) <- mkTheme c
    return $ VS3 r tv mt

mkVS4 :: Citation -> MoodTense -> PersonNumber -> Either String VerbStructure
mkVS4 c mt pn
  | not $ mt `elem` [IPRS .. INFP] =
    Left "Invalid MoodTense: cannot be INF, GER, PPP"
  | otherwise = do
    (r, tv) <- mkTheme c
    return $ VS4 r tv mt pn

mkVS5 ::
     Citation -> MoodTense -> Gender -> Number -> Either String VerbStructure
mkVS5 c mt g n
  | mt /= PPP = Left "Invalid MoodTense: must be PPP"
  | otherwise = do
    (r, tv) <- mkTheme c
    return $ VS5 r tv mt g n

mkTense :: Citation -> MoodTense -> Either String (TenseTable VerbStructure)
mkTense c mt = do
  (r, tv) <- mkTheme c
  case mt of
    INF -> return $ TenseTable mt [VS3 r tv mt]
    GER -> return $ TenseTable mt [VS3 r tv mt]
    PPP ->
      return $ TenseTable mt [VS5 r tv mt g n | g <- [MSC, FEM], n <- [SG, PL]]
    _ -> return $ TenseTable mt [VS4 r tv mt pn | pn <- [P1 .. P6]]

mkParadigm :: Citation -> Either String (Paradigm VerbStructure)
mkParadigm c = do
  tenses <- mapM (mkTense c) [IPRS .. PPP]
  return $ Paradigm c tenses

mkVerbParameters ::
     Citation
  -> Voice
  -> Mood
  -> Aspect
  -> Tense
  -> Person
  -> Number
  -> Gender
  -> Polarity
  -> Either String VerbParameters
mkVerbParameters c v m a t p n g po = do
  c' <- mkCitation c
  return $ VParams c' v m a t p n g po

mkVerb :: Citation -> Either String VerbParameters
mkVerb c =
  mkVerbParameters
    c
    Active
    Indicative
    Imperfect
    Present
    Third
    SG
    MSC
    Affirmative

infix 1 %%

class Parameter a where
  (%%) :: VerbParameters -> a -> VerbParameters

instance Parameter Voice where
  (%%) vp v = vp {voice = v}

instance Parameter Mood where
  (%%) vp m = vp {mood = m}

instance Parameter Aspect where
  (%%) vp a = vp {aspect = a}

instance Parameter Tense where
  (%%) vp t = vp {tense = t}

instance Parameter Person where
  (%%) vp p = vp {person = p}

instance Parameter Number where
  (%%) vp n = vp {number = n}

instance Parameter Gender where
  (%%) vp g = vp {gender = g}

instance Parameter Polarity where
  (%%) vp po = vp {polarity = po}
