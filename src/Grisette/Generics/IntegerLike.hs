{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Grisette.Generics.IntegerLike
  ( IntegerLike,
    SafeIntegerLike,
  )
where

import Control.Exception (ArithException)
import Grisette (SymInteger)
import Grisette.Generics.BaseConstraint (ConSymConversion)
import Grisette.Generics.Class.SimpleMergeable (SimpleMergeable)
import Grisette.Generics.IntegralLike (NumLike, SafeIntegralLike)

type IntegerLike bool int =
  ( NumLike bool int,
    SimpleMergeable bool int,
    ConSymConversion Integer SymInteger bool int
  )

type SafeIntegerLike bool int m =
  ( IntegerLike bool int,
    SafeIntegralLike ArithException bool int m
  )
