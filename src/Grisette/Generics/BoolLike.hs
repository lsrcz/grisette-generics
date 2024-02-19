{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Grisette.Generics.BoolLike (BoolLike) where

import Grisette
  ( LogicalOp,
    SymBool,
  )
import Grisette.Generics.BaseConstraint
  ( BasicGrisetteType,
    ConSymConversion,
  )
import Grisette.Generics.Class.SimpleMergeable (SimpleMergeable)

type BoolLike bool =
  ( BasicGrisetteType bool bool,
    ConSymConversion Bool SymBool bool bool,
    SimpleMergeable bool bool,
    LogicalOp bool
  )
