{-# LANGUAGE ConstraintKinds #-}

module Grisette.Generics.BaseConstraint
  ( BasicGrisetteType,
    ConSymConversion,
  )
where

import Control.DeepSeq (NFData)
import Grisette
  ( EvaluateSym,
    ExtractSymbolics,
    GPretty,
    Mergeable,
    SubstituteSym,
    ToCon,
    ToSym,
  )
import Grisette.Generics.Class.ITEOp (ITEOp)
import Grisette.Generics.Class.SEq (SEq)
import Grisette.Generics.Class.SOrd (SOrd)
import Language.Haskell.TH.Syntax (Lift)

type BasicGrisetteType bool t =
  ( Show t,
    Eq t,
    NFData t,
    Lift t,
    EvaluateSym t,
    ExtractSymbolics t,
    SubstituteSym t,
    GPretty t,
    Mergeable t,
    SEq bool t,
    SOrd bool t,
    ITEOp bool t
  )

type ConSymConversion conType symType bool t =
  ( ToCon t conType,
    ToSym conType t,
    ToCon symType t,
    ToSym t symType
  )
