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
  ( EvaluateSym,
    ExtractSymbolics,
    GPretty,
    Mergeable,
    SafeDivision,
    SafeLinearArith,
    SubstituteSym,
    ToCon,
    ToSym,
  )
import Language.Haskell.TH.Syntax (Lift)

type BasicGrisetteType t =
  ( Show t,
    Eq t,
    NFData t,
    Lift t,
    Hashable t,
    EvaluateSym t,
    ExtractSymbolics t,
    SubstituteSym t,
    GPretty t,
    Mergeable t
  )

type ConSymConversion conType symType t =
  ( ToCon t conType,
    ToSym conType t,
    ToCon symType t,
    ToSym t symType
  )

type SafeIntegral e v m = (SafeDivision e v m, SafeLinearArith e v m)
