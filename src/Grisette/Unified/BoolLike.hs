{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Grisette.Unified.BoolLike (BoolLike) where

import Grisette
  ( LogicalOp,
    SymBool,
  )
import Grisette.Unified.BaseConstraint
  ( BasicGrisetteType,
    ConSymConversion,
  )
import Grisette.Unified.Class.SimpleMergeable (SimpleMergeable)

type BoolLike bool =
  ( BasicGrisetteType bool bool,
    ConSymConversion Bool SymBool bool bool,
    SimpleMergeable bool bool,
    LogicalOp bool
  )
