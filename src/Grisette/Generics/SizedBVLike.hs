{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Grisette.Generics.SizedBVLike
  ( SizedBVLike,
    SafeSizedBVLike,
    IntNLike,
    SafeIntNLike,
    WordNLike,
    SafeWordNLike,
    WordNIntNLikePair,
    SafeWordNIntNLikePair,
  )
where

import Control.DeepSeq (NFData)
import Control.Exception (ArithException)
import Data.Bits (Bits, FiniteBits)
import Data.Kind (Constraint)
import GHC.TypeLits (KnownNat, type (<=))
import Grisette
  ( EvaluateSym,
    ExtractSymbolics,
    GPretty,
    IntN,
    Mergeable,
    SafeDivision,
    SafeLinearArith,
    SignConversion,
    SizedBV,
    SubstituteSym,
    SymIntN,
    SymWordN,
    ToCon,
    ToSym,
    WordN,
  )
import Grisette.Core.Data.Class.SafeSymRotate (SafeSymRotate)
import Grisette.Core.Data.Class.SafeSymShift (SafeSymShift)
import Grisette.Core.Data.Class.SymRotate (SymRotate)
import Grisette.Core.Data.Class.SymShift (SymShift)
import Grisette.Generics.Class.ITEOp (ITEOp)
import Grisette.Generics.Class.MonadBranching (MonadBranching)
import Grisette.Generics.Class.SEq (SEq)
import Grisette.Generics.Class.SOrd (SOrd)
import Grisette.Generics.Class.SimpleMergeable (SimpleMergeable)
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
    SizedBV bv
  ) ::
    Constraint

type SafeSizedBVLike bool bv m =
  ( SizedBVLike bool bv,
    MonadBranching bool m,
    forall n. (KnownNat n, 1 <= n) => SafeDivision ArithException (bv n) m,
    forall n. (KnownNat n, 1 <= n) => SafeLinearArith ArithException (bv n) m,
    forall n. (KnownNat n, 1 <= n) => SafeSymShift ArithException (bv n) m,
    forall n. (KnownNat n, 1 <= n) => SafeSymRotate ArithException (bv n) m
  ) ::
    Constraint

type IntNLike bool bv =
  ( SizedBVLike bool bv,
    forall n. (KnownNat n, 1 <= n) => ToCon (SymIntN n) (bv n),
    forall n. (KnownNat n, 1 <= n) => ToCon (bv n) (IntN n),
    forall n. (KnownNat n, 1 <= n) => ToSym (bv n) (SymIntN n),
    forall n. (KnownNat n, 1 <= n) => ToSym (IntN n) (bv n)
  ) ::
    Constraint

type SafeIntNLike bool bv m =
  ( IntNLike bool bv,
    SafeSizedBVLike bool bv m
  ) ::
    Constraint

type WordNLike bool bv =
  ( SizedBVLike bool bv,
    forall n. (KnownNat n, 1 <= n) => ToCon (SymWordN n) (bv n),
    forall n. (KnownNat n, 1 <= n) => ToCon (bv n) (WordN n),
    forall n. (KnownNat n, 1 <= n) => ToSym (bv n) (SymWordN n),
    forall n. (KnownNat n, 1 <= n) => ToSym (WordN n) (bv n)
  ) ::
    Constraint

type SafeWordNLike bool bv m =
  ( WordNLike bool bv,
    SafeSizedBVLike bool bv m
  ) ::
    Constraint

type WordNIntNLikePair bool word int =
  ( IntNLike bool int,
    WordNLike bool word,
    forall n. (KnownNat n, 1 <= n) => SignConversion (word n) (int n)
  ) ::
    Constraint

type SafeWordNIntNLikePair bool word int m =
  ( SafeIntNLike bool int m,
    SafeWordNLike bool word m,
    WordNIntNLikePair bool word int
  ) ::
    Constraint
