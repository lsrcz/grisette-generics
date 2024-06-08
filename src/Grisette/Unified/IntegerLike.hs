{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Grisette.Unified.IntegerLike
  ( IntegerLike,
    SafeIntegerLike,
    UnifyInteger (..),
    SafeUnifyInteger,
  )
where

import Control.Exception (ArithException)
import Grisette (SymBool, SymInteger)
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

class
  (IntegerLike bool (UniInteger bool), int ~ UniInteger bool) =>
  UnifyInteger bool int
  where
  type UniInteger bool

instance UnifyInteger Bool Integer where
  type UniInteger Bool = Integer

instance UnifyInteger SymBool SymInteger where
  type UniInteger SymBool = SymInteger

class
  (SafeIntegerLike bool int m, UnifyInteger bool int) =>
  SafeUnifyInteger bool int m

instance
  (SafeIntegerLike bool int m, UnifyInteger bool int) =>
  SafeUnifyInteger bool int m
