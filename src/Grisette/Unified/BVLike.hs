{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Grisette.Unified.BVLike
  ( BVLike,
    BVPair,
    SafeBVLike,
    SafeBVPair,
    SomeBVLike,
    SafeSomeBVLike,
    SomeIntNLike,
    SafeSomeIntNLike,
    SomeWordNLike,
    SafeSomeWordNLike,
    SomeWordNSomeIntNLikePair,
    SafeSomeWordNSomeIntNLikePair,
    UnifySomeWordNSomeIntN (..),
    SafeUnifySomeWordNSomeIntN,
  )
where

import Control.Exception (ArithException)
import Control.Monad.Except (ExceptT)
import Data.Bits (Bits, FiniteBits)
import Data.Kind (Constraint)
import Grisette
  ( BV,
    BitwidthMismatch,
    SafeSymRotate,
    SafeSymShift,
    SignConversion,
    SomeIntN,
    SomeSymIntN,
    SomeSymWordN,
    SomeWordN,
    SymBool,
    SymRotate,
    SymShift,
  )
import Grisette.Unified.BaseConstraint
  ( ConSymConversion,
  )
import Grisette.Unified.IntegralLike (NumLike, SafeIntegralLike)

type BVLike bool bv =
  ( NumLike bool bv,
    Bits bv,
    FiniteBits bv,
    SymShift bv,
    SymRotate bv
  ) ::
    Constraint

type BVPair bool word int =
  ( BVLike bool word,
    BVLike bool int,
    SignConversion word int
  )

type SafeBVLike e bool bv m =
  ( BVLike bool bv,
    SafeIntegralLike e bool bv m,
    SafeSymShift e bv (ExceptT e m),
    SafeSymRotate e bv (ExceptT e m)
  ) ::
    Constraint

type SafeBVPair e bool unsigned signed m =
  ( SafeBVLike e bool unsigned m,
    SafeBVLike e bool signed m,
    SignConversion unsigned signed
  )

type SomeBVLike bool bv =
  (BVLike bool bv, BV bv) ::
    Constraint

type SafeSomeBVLike bool bv m =
  ( SafeBVLike (Either BitwidthMismatch ArithException) bool bv m,
    SomeBVLike bool bv
  ) ::
    Constraint

type SomeIntNLike bool bv =
  ( SomeBVLike bool bv,
    ConSymConversion SomeIntN SomeSymIntN bool bv
  )

type SafeSomeIntNLike bool bv m =
  ( SafeSomeBVLike bool bv m,
    SomeIntNLike bool bv
  )

type SomeWordNLike bool bv =
  ( SomeBVLike bool bv,
    ConSymConversion SomeWordN SomeSymWordN bool bv
  )

type SafeSomeWordNLike bool bv m =
  ( SafeSomeBVLike bool bv m,
    SomeWordNLike bool bv
  )

type SomeWordNSomeIntNLikePair bool word int =
  ( SomeIntNLike bool int,
    SomeWordNLike bool word,
    SignConversion word int
  )

type SafeSomeWordNSomeIntNLikePair bool word int m =
  ( SafeSomeWordNLike bool word m,
    SafeSomeIntNLike bool int m,
    SomeWordNSomeIntNLikePair bool word int
  )

class
  ( SomeWordNSomeIntNLikePair bool word int,
    word ~ SomeUniWordN bool,
    int ~ SomeUniIntN bool
  ) =>
  UnifySomeWordNSomeIntN bool word int
  where
  type SomeUniWordN bool
  type SomeUniIntN bool

instance UnifySomeWordNSomeIntN Bool SomeWordN SomeIntN where
  type SomeUniWordN Bool = SomeWordN
  type SomeUniIntN Bool = SomeIntN

instance UnifySomeWordNSomeIntN SymBool SomeSymWordN SomeSymIntN where
  type SomeUniWordN SymBool = SomeSymWordN
  type SomeUniIntN SymBool = SomeSymIntN

class
  ( SafeSomeWordNSomeIntNLikePair bool wordn intn m,
    UnifySomeWordNSomeIntN bool wordn intn
  ) =>
  SafeUnifySomeWordNSomeIntN bool wordn intn m

instance
  ( SafeSomeWordNSomeIntNLikePair bool wordn intn m,
    UnifySomeWordNSomeIntN bool wordn intn
  ) =>
  SafeUnifySomeWordNSomeIntN bool wordn intn m
