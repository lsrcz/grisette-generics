{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}

module Grisette.Generics.BVLike
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
    SomeIntNSomeWordNLikePair,
    SafeSomeIntNSomeWordNLikePair,
  )
where

import Control.Exception (ArithException)
import Data.Bits (Bits, FiniteBits)
import Data.Kind (Constraint)
import Grisette
  ( BV,
    SignConversion,
    SomeIntN,
    SomeSymIntN,
  )
import Grisette.Core.Data.BV (BitwidthMismatch)
import Grisette.Core.Data.Class.SafeSymRotate (SafeSymRotate)
import Grisette.Core.Data.Class.SafeSymShift (SafeSymShift)
import Grisette.Core.Data.Class.SymRotate (SymRotate)
import Grisette.Core.Data.Class.SymShift (SymShift)
import Grisette.Core.Data.SomeBV (SomeSymWordN, SomeWordN)
import Grisette.Generics.BaseConstraint
  ( ConSymConversion,
  )
import Grisette.Generics.IntegralLike (NumLike, SafeIntegralLike)

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
    SafeSymShift e bv m,
    SafeSymRotate e bv m
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

type SomeIntNSomeWordNLikePair bool word int =
  ( SomeIntNLike bool int,
    SomeWordNLike bool word,
    SignConversion word int
  )

type SafeSomeIntNSomeWordNLikePair bool word int m =
  ( SafeSomeWordNLike bool word m,
    SafeSomeIntNLike bool int m,
    SomeIntNSomeWordNLikePair bool word int
  )
