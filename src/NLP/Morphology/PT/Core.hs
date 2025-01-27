{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module NLP.Morphology.PT.Core
  ( Citation
  , Root(..)
  , ThematicVowel(..)
  , MoodTense(..)
  , PersonNumber(..)
  , Gender(..)
  , Number(..)
  , Morpheme(..)
  , Case(..)
  , Person(..)
  , Mood(..)
  , Aspect(..)
  , Tense(..)
  , Polarity(..)
  , Voice(..)
  ) where

import           Data.Aeson   (FromJSON, ToJSON)
import           GHC.Generics (Generic)

type Citation = String

data Root =
  Root String
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON, Read)

data ThematicVowel
  = A'
  | E'
  | I'
  | O'
  | U'
  | Z'
  deriving (Show, Eq, Ord, Enum, Bounded, Generic, ToJSON, FromJSON, Read)

data Voice
  = Active
  | Passive
  deriving (Show, Eq, Ord, Enum, Bounded, Generic, ToJSON, FromJSON, Read)

data Mood
  = Indicative
  | Subjunctive
  | Conditional
  | Imperative
  | Infinitive
  | Gerund
  | Participle
  deriving (Show, Eq, Ord, Enum, Bounded, Generic, ToJSON, FromJSON, Read)

data Aspect
  = Imperfect
  | Perfect
  deriving (Show, Eq, Ord, Enum, Bounded, Generic, ToJSON, FromJSON, Read)

data Tense
  = Present
  | Past
  | Future
  deriving (Show, Eq, Ord, Enum, Bounded, Generic, ToJSON, FromJSON, Read)

data Polarity
  = Affirmative
  | Negative
  deriving (Show, Eq, Ord, Enum, Bounded, Generic, ToJSON, FromJSON, Read)

data MoodTense
  = IPRS
  | IPRF
  | IIPF
  | IPPF
  | IFUT
  | COND
  | SPRS
  | SIPF
  | SFUT
  | IMPA
  | IMPN
  | INFP
  | INF
  | GER
  | PPP
  deriving (Show, Eq, Ord, Enum, Bounded, Generic, ToJSON, FromJSON, Read)

data PersonNumber
  = P1
  | P2
  | P3
  | P4
  | P5
  | P6
  deriving (Show, Eq, Ord, Enum, Bounded, Generic, ToJSON, FromJSON, Read)

data Person
  = FST
  | SND
  | TRD
  deriving (Show, Eq, Ord, Enum, Bounded, Generic, ToJSON, FromJSON, Read)

data Case
  = NOM
  | ACC
  | DAT
  | GEN
  | OBL
  deriving (Show, Eq, Ord, Enum, Bounded, Generic, ToJSON, FromJSON, Read)

data Gender
  = MSC
  | FEM
  deriving (Show, Eq, Ord, Enum, Bounded, Generic, ToJSON, FromJSON, Read)

data Number
  = SG
  | PL
  deriving (Show, Eq, Ord, Enum, Bounded, Generic, ToJSON, FromJSON, Read)

data Morpheme
  = A
  | E
  | I
  | O
  | U
  | S
  | MOS
  | DES
  | IS
  | M
  | STE
  | STES
  | VA
  | VE
  | RA
  | RE
  | SE
  | R
  | NDO
  | D
  | X -- ^ Null morpheme (inexistent form)
  | Z -- ^ Zero morpheme
  | Morph String
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON, Read)
