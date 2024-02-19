{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Grisette.Generics.IntegerLike
  ( IntegerLike,
    SafeIntegerLike,
  )
where

import Control.Exception (ArithException)
import Grisette.Generics.BaseConstraint (ConSymConversion)
import Grisette.Generics.Class.SimpleMergeable (SimpleMergeable)
import Grisette.Generics.IntegralLike (NumLike, SafeIntegralLike)
import Grisette.IR.SymPrim (SymInteger)

type IntegerLike bool int =
  ( NumLike bool int,
    SimpleMergeable bool int,
    ConSymConversion Integer SymInteger bool int
  )

type SafeIntegerLike bool int m =
  ( IntegerLike bool int,
    SafeIntegralLike ArithException bool int m
  )
