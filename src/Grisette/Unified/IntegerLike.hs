{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Grisette.Unified.IntegerLike
  ( IntegerLike,
    SafeIntegerLike,
  )
where

import Control.Exception (ArithException)
import Grisette (SymInteger)
import Grisette.Unified.BaseConstraint (ConSymConversion)
import Grisette.Unified.Class.SimpleMergeable (SimpleMergeable)
import Grisette.Unified.IntegralLike (NumLike, SafeIntegralLike)

type IntegerLike bool int =
  ( NumLike bool int,
    SimpleMergeable bool int,
    ConSymConversion Integer SymInteger bool int
  )

type SafeIntegerLike bool int m =
  ( IntegerLike bool int,
    SafeIntegralLike ArithException bool int m
  )
