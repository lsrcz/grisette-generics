{-# LANGUAGE ConstraintKinds #-}

module Grisette.Unified.BaseConstraint
  ( BasicGrisetteType,
    ConSymConversion,
    SafeIntegral,
  )
where

import Control.DeepSeq (NFData)
import Data.Hashable (Hashable)
import Grisette
  ( AllSyms,
    EvaluateSym,
    ExtractSymbolics,
    GPretty,
    Mergeable,
    SEq,
    SOrd,
    SafeDivision,
    SafeLinearArith,
    SubstituteSym,
    ToCon,
    ToSym,
  )
import Language.Haskell.TH.Syntax (Lift)

type BasicGrisetteType t =
  ( AllSyms t,
    Eq t,
    EvaluateSym t,
    ExtractSymbolics t,
    GPretty t,
    Hashable t,
    Lift t,
    Mergeable t,
    NFData t,
    SEq t,
    Show t,
    SOrd t,
    SubstituteSym t
  )

type ConSymConversion conType symType t =
  ( ToCon t conType,
    ToSym conType t,
    ToCon symType t,
    ToSym t symType
  )

type SafeIntegral e v m = (SafeDivision e v m, SafeLinearArith e v m)
