{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module NLP.Morphology.PT.Verb.Core
  ( Morph(..)
  , Morphs(..)
  , Morphemes(..)
  , suppletive
  , deep
  , shallow
  , orth
  , mkIFUT
  , mkCOND
  ) where

import           Data.Aeson                       (FromJSON, ToJSON)
import           GHC.Generics                     (Generic)
import           NLP.Morphology.PT.Core           (Gender (..), MoodTense (..),
                                                   Morpheme (..), Number (..),
                                                   PersonNumber (..), Root (..),
                                                   ThematicVowel (..))
import           NLP.Morphology.PT.Verb.Structure (VerbStructure (..))

class Morph a where
  morph :: a -> Morpheme

instance Morph Root where
  morph (Root r) = Morph r

instance Morph ThematicVowel where
  morph tv =
    case tv of
      A' -> A
      E' -> E
      I' -> I
      O' -> O
      U' -> U
      Z' -> Z

instance Morph MoodTense where
  morph mt =
    case mt of
      IIPF -> VA
      SIPF -> SE
      SFUT -> R
      INFP -> R
      INF  -> R
      GER  -> NDO
      PPP  -> D
      _    -> Z

instance Morph PersonNumber where
  morph pn =
    case pn of
      P2 -> S
      P4 -> MOS
      P5 -> DES
      P6 -> M
      _  -> Z

instance Morph Gender where
  morph g =
    case g of
      MSC -> O
      FEM -> A

instance Morph Number where
  morph n =
    case n of
      SG -> Z
      PL -> S

data Morphemes
  = M0
  | M3 [Morpheme]
  | M4 [Morpheme]
  | M5 [Morpheme]
  | M34 Morphemes Morphemes
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

class Morphs a where
  morphs :: a -> Morphemes

instance Morphs VerbStructure where
  morphs vs =
    case vs of
      VS0             -> M0
      VS3 r tv mt     -> M3 [morph r, morph tv, morph mt]
      VS4 r tv mt pn  -> M4 [morph r, morph tv, morph mt, morph pn]
      VS5 r tv mt g n -> M5 [morph r, morph tv, morph mt, morph g, morph n]
      VS34 v3 v4      -> M34 (morphs v3) (morphs v4)

suppletive :: VerbStructure -> VerbStructure
suppletive vs =
  case vs of
    VS4 r tv IFUT pn    -> mkIFUT r tv pn
    VS4 r tv COND pn    -> mkCOND r tv pn
    VS4 r tv IPRF P6    -> VS4 r tv IPPF P6
    VS4 _ _ IMPN P1     -> VS0
    VS4 r tv IMPN pn    -> VS4 r tv SPRS pn
    VS4 _ _ IMPA P1     -> VS0
    VS4 r tv IMPA pn@P3 -> VS4 r tv SPRS pn
    VS4 r tv IMPA pn@P4 -> VS4 r tv SPRS pn
    VS4 r tv IMPA pn@P6 -> VS4 r tv SPRS pn
    VS4 r tv mt pn      -> VS4 r tv mt pn
    _                   -> vs

deep :: VerbStructure -> Morphemes
deep vs =
  case suppletive vs of
    VS0                 -> M0
    VS4 r _ mt@IPRS P1  -> M4 [morph r, Z, morph mt, O]
    VS4 r A' mt@SPRS pn -> M4 [morph r, E, morph mt, morph pn]
    VS4 r _ mt@SPRS pn  -> M4 [morph r, A, morph mt, morph pn]
    VS4 r tv mt pn      -> M4 [morph r, morph tv, morph mt, morph pn]
    VS34 v3 v4          -> M34 (morphs v3) (morphs_haver_deep v4)
    _                   -> morphs vs

shallow :: VerbStructure -> Morphemes
shallow vs =
  case suppletive vs of
    VS0              -> M0
    VS4 r A' IIPF P5 -> M4 [morph r, A, VE, IS]
    VS4 r A' IIPF pn -> M4 [morph r, A, VA, morph pn]
    VS4 r _ IIPF P5  -> M4 [morph r, I, E, IS]
    VS4 r _ IIPF pn  -> M4 [morph r, I, A, morph pn]
    VS4 r tv IPPF P5 -> M4 [morph r, morph tv, RE, IS]
    VS4 r tv mt P5   -> M4 [morph r, morph tv, morph mt, IS]
    VS4 r tv mt pn   -> M4 [morph r, morph tv, morph mt, morph pn]
    VS34 v3 v4       -> M34 (morphs v3) (morphs_haver_shallow v4)
    _                -> deep vs

orth :: VerbStructure -> String
orth = error "Not implemented yet. Intended for orthographic transformation."

mkIFUT :: Root -> ThematicVowel -> PersonNumber -> VerbStructure
mkIFUT r tv pn =
  let inf = VS3 r tv INF
      haver = VS4 (Root "HAV") E' IPRS pn
   in VS34 inf haver

mkCOND :: Root -> ThematicVowel -> PersonNumber -> VerbStructure
mkCOND r tv pn =
  let inf = VS3 r tv INF
      haver = VS4 (Root "HAV") E' IIPF pn
   in VS34 inf haver

morphs_haver_deep :: VerbStructure -> Morphemes
morphs_haver_deep vs =
  case vs of
    VS4 _ _ mt pn ->
      case (mt, pn) of
        (IPRS, P1) -> M4 [Morph "H", A, Z, I]
        (IPRS, P2) -> M4 [Morph "H", A, Z, S]
        (IPRS, P3) -> M4 [Morph "H", A, Z, Z]
        (IPRS, P4) -> M4 [Morph "HAV", E, Z, MOS]
        (IPRS, P5) -> M4 [Morph "HAV", E, Z, DES]
        (IPRS, P6) -> M4 [Morph "H", A, Z, M]
        (IIPF, P1) -> M4 [Morph "H", I, A, Z]
        (IIPF, P2) -> M4 [Morph "H", I, A, S]
        (IIPF, P3) -> M4 [Morph "H", I, A, Z]
        (IIPF, P4) -> M4 [Morph "H", I, A, MOS]
        (IIPF, P5) -> M4 [Morph "H", I, A, DES]
        (IIPF, P6) -> M4 [Morph "H", I, A, M]
        _          -> error "Should not happen"
    _ -> error "Should not happen"

morphs_haver_shallow :: VerbStructure -> Morphemes
morphs_haver_shallow vs =
  case vs of
    VS4 _ _ mt pn ->
      case (mt, pn) of
        (IPRS, P1) -> M4 [Morph "∅", E, Z, I]
        (IPRS, P2) -> M4 [Morph "∅", A, Z, S]
        (IPRS, P3) -> M4 [Morph "∅", A, Z, Z]
        (IPRS, P4) -> M4 [Morph "∅", E, Z, MOS]
        (IPRS, P5) -> M4 [Morph "∅", E, Z, IS]
        (IPRS, P6) -> M4 [Morph "∅", A, Z, M]
        (IIPF, P1) -> M4 [Morph "∅", I, A, Z]
        (IIPF, P2) -> M4 [Morph "∅", I, A, S]
        (IIPF, P3) -> M4 [Morph "∅", I, A, Z]
        (IIPF, P4) -> M4 [Morph "∅", I, A, MOS]
        (IIPF, P5) -> M4 [Morph "∅", I, E, IS]
        (IIPF, P6) -> M4 [Morph "∅", I, A, M]
        _          -> error "Should not happen"
    _ -> error "Should not happen"
