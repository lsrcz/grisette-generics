{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Grisette.Unified.SizedBVLike
  ( SizedBVLike,
    SafeSizedBVLike,
    IntNLike,
    SafeIntNLike,
    WordNLike,
    SafeWordNLike,
    WordNIntNLikePair,
    SafeWordNIntNLikePair,
    UnifyWordNIntN (..),
    SafeUnifyWordNIntN,
  )
where

import Control.DeepSeq (NFData)
import Control.Exception (ArithException)
import Control.Monad.Except (ExceptT)
import Data.Bits (Bits, FiniteBits)
import Data.Kind (Constraint, Type)
import GHC.TypeLits (KnownNat, Nat, type (<=))
import Grisette
  ( EvaluateSym,
    ExtractSymbolics,
    GPretty,
    IntN,
    Mergeable,
    SafeDivision,
    SafeLinearArith,
    SafeSymRotate,
    SafeSymShift,
    SignConversion,
    SizedBV,
    SomeBV,
    SubstituteSym,
    SymBool,
    SymIntN,
    SymRotate,
    SymShift,
    SymWordN,
    ToCon,
    ToSym,
    WordN,
  )
import Grisette.Unified.BVLike
  ( SafeSomeBVLike,
    SafeSomeIntNLike,
    SafeSomeWordNLike,
    SafeUnifySomeWordNSomeIntN,
    SomeBVLike,
    SomeIntNLike,
    SomeWordNLike,
    SomeWordNSomeIntNLikePair,
    UnifySomeWordNSomeIntN,
  )
import Grisette.Unified.Class.Branching (Branching)
import Grisette.Unified.Class.ITEOp (ITEOp)
import Grisette.Unified.Class.SEq (SEq)
import Grisette.Unified.Class.SOrd (SOrd)
import Grisette.Unified.Class.SimpleMergeable (SimpleMergeable)
import Language.Haskell.TH.Syntax (Lift)

type SizedBVLike bool bv =
  ( forall n. (KnownNat n, 1 <= n) => Show (bv n),
    forall n. (KnownNat n, 1 <= n) => Eq (bv n),
    forall n. (KnownNat n, 1 <= n) => NFData (bv n),
    forall n. (KnownNat n, 1 <= n) => Lift (bv n),
    forall n. (KnownNat n, 1 <= n) => EvaluateSym (bv n),
    forall n. (KnownNat n, 1 <= n) => ExtractSymbolics (bv n),
    forall n. (KnownNat n, 1 <= n) => SubstituteSym (bv n),
    forall n. (KnownNat n, 1 <= n) => GPretty (bv n),
    forall n. (KnownNat n, 1 <= n) => Mergeable (bv n),
    forall n. (KnownNat n, 1 <= n) => SEq bool (bv n),
    forall n. (KnownNat n, 1 <= n) => SOrd bool (bv n),
    forall n. (KnownNat n, 1 <= n) => ITEOp bool (bv n),
    forall n. (KnownNat n, 1 <= n) => SimpleMergeable bool (bv n),
    forall n. (KnownNat n, 1 <= n) => Num (bv n),
    forall n. (KnownNat n, 1 <= n) => Bits (bv n),
    forall n. (KnownNat n, 1 <= n) => FiniteBits (bv n),
    forall n. (KnownNat n, 1 <= n) => SymShift (bv n),
    forall n. (KnownNat n, 1 <= n) => SymRotate (bv n),
    SizedBV bv,
    SomeBVLike bool (SomeBV bv)
  ) ::
    Constraint

type SafeSizedBVLike bool bv m =
  ( SizedBVLike bool bv,
    Monad m,
    Branching bool m,
    forall n.
    (KnownNat n, 1 <= n) =>
    SafeDivision ArithException (bv n) (ExceptT ArithException m),
    forall n.
    (KnownNat n, 1 <= n) =>
    SafeLinearArith ArithException (bv n) (ExceptT ArithException m),
    forall n.
    (KnownNat n, 1 <= n) =>
    SafeSymShift ArithException (bv n) (ExceptT ArithException m),
    forall n.
    (KnownNat n, 1 <= n) =>
    SafeSymRotate ArithException (bv n) (ExceptT ArithException m),
    SafeSomeBVLike bool (SomeBV bv) m
  ) ::
    Constraint

type IntNLike bool bv =
  ( SizedBVLike bool bv,
    forall n. (KnownNat n, 1 <= n) => ToCon (SymIntN n) (bv n),
    forall n. (KnownNat n, 1 <= n) => ToCon (bv n) (IntN n),
    forall n. (KnownNat n, 1 <= n) => ToSym (bv n) (SymIntN n),
    forall n. (KnownNat n, 1 <= n) => ToSym (IntN n) (bv n),
    SomeIntNLike bool (SomeBV bv)
  ) ::
    Constraint

type SafeIntNLike bool bv m =
  ( IntNLike bool bv,
    SafeSomeIntNLike bool (SomeBV bv) m,
    SafeSizedBVLike bool bv m
  ) ::
    Constraint

type WordNLike bool bv =
  ( SizedBVLike bool bv,
    forall n. (KnownNat n, 1 <= n) => ToCon (SymWordN n) (bv n),
    forall n. (KnownNat n, 1 <= n) => ToCon (bv n) (WordN n),
    forall n. (KnownNat n, 1 <= n) => ToSym (bv n) (SymWordN n),
    forall n. (KnownNat n, 1 <= n) => ToSym (WordN n) (bv n),
    SomeWordNLike bool (SomeBV bv)
  ) ::
    Constraint

type SafeWordNLike bool bv m =
  ( WordNLike bool bv,
    SafeSomeWordNLike bool (SomeBV bv) m,
    SafeSizedBVLike bool bv m
  ) ::
    Constraint

type WordNIntNLikePair bool word int =
  ( IntNLike bool int,
    WordNLike bool word,
    forall n. (KnownNat n, 1 <= n) => SignConversion (word n) (int n),
    SomeWordNSomeIntNLikePair bool (SomeBV word) (SomeBV int)
  ) ::
    Constraint

type SafeWordNIntNLikePair bool word int m =
  ( SafeIntNLike bool int m,
    SafeWordNLike bool word m,
    WordNIntNLikePair bool word int
  ) ::
    Constraint

class
  ( WordNIntNLikePair bool wordn intn,
    UnifySomeWordNSomeIntN bool (SomeBV wordn) (SomeBV intn),
    wordn ~ UniWordN bool,
    intn ~ UniIntN bool
  ) =>
  UnifyWordNIntN bool wordn intn
  where
  type UniWordN bool :: Nat -> Type
  type UniIntN bool :: Nat -> Type

instance UnifyWordNIntN Bool WordN IntN where
  type UniWordN Bool = WordN
  type UniIntN Bool = IntN

instance UnifyWordNIntN SymBool SymWordN SymIntN where
  type UniWordN SymBool = SymWordN
  type UniIntN SymBool = SymIntN

class
  ( SafeWordNIntNLikePair bool wordn intn m,
    SafeUnifySomeWordNSomeIntN bool (SomeBV wordn) (SomeBV intn) m,
    UnifyWordNIntN bool wordn intn
  ) =>
  SafeUnifyWordNIntN bool wordn intn m

instance
  ( SafeWordNIntNLikePair bool wordn intn m,
    SafeUnifySomeWordNSomeIntN bool (SomeBV wordn) (SomeBV intn) m,
    UnifyWordNIntN bool wordn intn
  ) =>
  SafeUnifyWordNIntN bool wordn intn m
