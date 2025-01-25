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
  ) where

type Citation = String

data Root =
  Root String
  deriving (Show, Eq, Ord)

data ThematicVowel
  = A'
  | E'
  | I'
  | O'
  | U'
  | Z'
  deriving (Show, Eq, Ord, Enum, Bounded, Read)

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
  deriving (Show, Eq, Ord, Enum, Bounded, Read)

data PersonNumber
  = P1
  | P2
  | P3
  | P4
  | P5
  | P6
  deriving (Show, Eq, Ord, Enum, Bounded, Read)

data Person
  = FST
  | SND
  | TRD
  deriving (Show, Eq, Ord, Enum, Bounded, Read)

data Case
  = NOM
  | ACC
  | DAT
  | GEN
  | OBL
  deriving (Show, Eq, Ord, Enum, Bounded, Read)

data Gender
  = MSC
  | FEM
  deriving (Show, Eq, Ord, Enum, Bounded, Read)

data Number
  = SG
  | PL
  deriving (Show, Eq, Ord, Enum, Bounded, Read)

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
  deriving (Show, Eq, Ord)
