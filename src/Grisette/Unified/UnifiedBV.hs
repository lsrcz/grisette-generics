{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Grisette.Unified.UnifiedBV
  ( UnifiedBV (..),
    UnifiedBV0,
    GetSomeWordN,
    GetSomeIntN,
    SafeUnifiedBV0,
  )
where

import Control.DeepSeq (NFData)
import Control.Exception (ArithException)
import Control.Monad.Except (ExceptT)
import Data.Bits (Bits, FiniteBits)
import Data.Hashable (Hashable)
import Data.Kind (Constraint, Type)
import GHC.TypeNats (KnownNat, Nat, type (<=))
import Grisette
  ( BV,
    BitwidthMismatch,
    EvaluateSym,
    ExtractSymbolics,
    GPretty,
    IntN,
    Mergeable,
    MonadTryMerge,
    MonadUnion,
    SEq,
    SOrd,
    SafeDivision,
    SafeLinearArith,
    SafeSymRotate,
    SafeSymShift,
    SignConversion,
    SizedBV,
    SomeBV,
    SomeIntN,
    SomeSymIntN,
    SomeSymWordN,
    SomeWordN,
    SubstituteSym,
    SymIntN,
    SymRotate,
    SymShift,
    SymWordN,
    ToCon,
    ToSym,
    WordN, AllSyms,
  )
import Grisette.Unified.BaseConstraint (BasicGrisetteType, ConSymConversion)
import Grisette.Unified.Class.UnifiedITEOp (UnifiedITEOp)
import Grisette.Unified.Class.UnifiedSEq (UnifiedSEq)
import Grisette.Unified.Class.UnifiedSOrd (UnifiedSOrd)
import Grisette.Unified.Class.UnifiedSimpleMergeable (UnifiedSimpleMergeable)
import Grisette.Unified.EvaluationMode (EvaluationMode (Con, Sym))
import Language.Haskell.TH.Syntax (Lift)

type SomeBVPair mode word int =
  ( BasicGrisetteType word,
    BasicGrisetteType int,
    Num word,
    Num int,
    Bits word,
    Bits int,
    FiniteBits word,
    FiniteBits int,
    SymShift word,
    SymShift int,
    SymRotate word,
    SymRotate int,
    BV word,
    BV int,
    ConSymConversion SomeWordN SomeSymWordN word,
    ConSymConversion SomeIntN SomeSymIntN int,
    UnifiedSEq mode word,
    UnifiedSEq mode int,
    UnifiedSOrd mode word,
    UnifiedSOrd mode int,
    UnifiedITEOp mode word,
    UnifiedITEOp mode int,
    SignConversion word int
  ) ::
    Constraint

class
  ( forall n. (KnownNat n, 1 <= n) => AllSyms (wordn n),
    forall n. (KnownNat n, 1 <= n) => Bits (wordn n),
    forall n. (KnownNat n, 1 <= n) => Eq (wordn n),
    forall n. (KnownNat n, 1 <= n) => EvaluateSym (wordn n),
    forall n. (KnownNat n, 1 <= n) => ExtractSymbolics (wordn n),
    forall n. (KnownNat n, 1 <= n) => FiniteBits (wordn n),
    forall n. (KnownNat n, 1 <= n) => GPretty (wordn n),
    forall n. (KnownNat n, 1 <= n) => Hashable (wordn n),
    forall n. (KnownNat n, 1 <= n) => Lift (wordn n),
    forall n. (KnownNat n, 1 <= n) => Mergeable (wordn n),
    forall n. (KnownNat n, 1 <= n) => NFData (wordn n),
    forall n. (KnownNat n, 1 <= n) => Num (wordn n),
    forall n. (KnownNat n, 1 <= n) => SEq (wordn n),
    forall n. (KnownNat n, 1 <= n) => Show (wordn n),
    forall n. (KnownNat n, 1 <= n) => SOrd (wordn n),
    forall n. (KnownNat n, 1 <= n) => SubstituteSym (wordn n),
    forall n. (KnownNat n, 1 <= n) => SymShift (wordn n),
    forall n. (KnownNat n, 1 <= n) => SymRotate (wordn n),
    forall n. (KnownNat n, 1 <= n) => ToCon (SymWordN n) (wordn n),
    forall n. (KnownNat n, 1 <= n) => ToCon (wordn n) (WordN n),
    forall n. (KnownNat n, 1 <= n) => ToSym (wordn n) (SymWordN n),
    forall n. (KnownNat n, 1 <= n) => ToSym (WordN n) (wordn n),
    forall n. (KnownNat n, 1 <= n) => UnifiedITEOp mode (wordn n),
    forall n. (KnownNat n, 1 <= n) => UnifiedSEq mode (wordn n),
    forall n. (KnownNat n, 1 <= n) => UnifiedSimpleMergeable mode (wordn n),
    forall n. (KnownNat n, 1 <= n) => UnifiedSOrd mode (wordn n),
    forall n. (KnownNat n, 1 <= n) => AllSyms (intn n),
    forall n. (KnownNat n, 1 <= n) => Bits (intn n),
    forall n. (KnownNat n, 1 <= n) => Eq (intn n),
    forall n. (KnownNat n, 1 <= n) => EvaluateSym (intn n),
    forall n. (KnownNat n, 1 <= n) => ExtractSymbolics (intn n),
    forall n. (KnownNat n, 1 <= n) => FiniteBits (intn n),
    forall n. (KnownNat n, 1 <= n) => GPretty (intn n),
    forall n. (KnownNat n, 1 <= n) => Hashable (intn n),
    forall n. (KnownNat n, 1 <= n) => Lift (intn n),
    forall n. (KnownNat n, 1 <= n) => Mergeable (intn n),
    forall n. (KnownNat n, 1 <= n) => NFData (intn n),
    forall n. (KnownNat n, 1 <= n) => Num (intn n),
    forall n. (KnownNat n, 1 <= n) => SEq (intn n),
    forall n. (KnownNat n, 1 <= n) => Show (intn n),
    forall n. (KnownNat n, 1 <= n) => SOrd (intn n),
    forall n. (KnownNat n, 1 <= n) => SubstituteSym (intn n),
    forall n. (KnownNat n, 1 <= n) => SymRotate (intn n),
    forall n. (KnownNat n, 1 <= n) => SymShift (intn n),
    forall n. (KnownNat n, 1 <= n) => ToCon (SymIntN n) (intn n),
    forall n. (KnownNat n, 1 <= n) => ToCon (intn n) (IntN n),
    forall n. (KnownNat n, 1 <= n) => ToSym (intn n) (SymIntN n),
    forall n. (KnownNat n, 1 <= n) => ToSym (IntN n) (intn n),
    forall n. (KnownNat n, 1 <= n) => UnifiedITEOp mode (intn n),
    forall n. (KnownNat n, 1 <= n) => UnifiedSEq mode (intn n),
    forall n. (KnownNat n, 1 <= n) => UnifiedSimpleMergeable mode (intn n),
    forall n. (KnownNat n, 1 <= n) => UnifiedSOrd mode (intn n),
    SizedBV wordn,
    SizedBV intn,
    forall n. (KnownNat n, 1 <= n) => SignConversion (wordn n) (intn n),
    SomeBVPair mode (SomeBV wordn) (SomeBV intn),
    wordn ~ GetWordN mode,
    intn ~ GetIntN mode
  ) =>
  UnifiedBV (mode :: EvaluationMode) wordn intn
  where
  type GetWordN mode = (w :: Nat -> Type) | w -> mode
  type GetIntN mode = (i :: Nat -> Type) | i -> mode

instance UnifiedBV 'Con WordN IntN where
  type GetWordN 'Con = WordN
  type GetIntN 'Con = IntN

instance UnifiedBV 'Sym SymWordN SymIntN where
  type GetWordN 'Sym = SymWordN
  type GetIntN 'Sym = SymIntN

type family GetSomeWordN mode = sw | sw -> mode where
  GetSomeWordN mode = SomeBV (GetWordN mode)

type family GetSomeIntN mode = sw | sw -> mode where
  GetSomeIntN mode = SomeBV (GetIntN mode)

class
  (UnifiedBV mode (GetWordN mode) (GetIntN mode)) =>
  UnifiedBV0 mode

instance UnifiedBV0 'Con

instance UnifiedBV0 'Sym

type SafeSomeBVPair word int m =
  ( SafeDivision
      (Either BitwidthMismatch ArithException)
      word
      (ExceptT (Either BitwidthMismatch ArithException) m),
    SafeLinearArith
      (Either BitwidthMismatch ArithException)
      word
      (ExceptT (Either BitwidthMismatch ArithException) m),
    SafeSymShift
      (Either BitwidthMismatch ArithException)
      word
      (ExceptT (Either BitwidthMismatch ArithException) m),
    SafeSymRotate
      (Either BitwidthMismatch ArithException)
      word
      (ExceptT (Either BitwidthMismatch ArithException) m),
    SafeDivision
      (Either BitwidthMismatch ArithException)
      int
      (ExceptT (Either BitwidthMismatch ArithException) m),
    SafeLinearArith
      (Either BitwidthMismatch ArithException)
      int
      (ExceptT (Either BitwidthMismatch ArithException) m),
    SafeSymShift
      (Either BitwidthMismatch ArithException)
      int
      (ExceptT (Either BitwidthMismatch ArithException) m),
    SafeSymRotate
      (Either BitwidthMismatch ArithException)
      int
      (ExceptT (Either BitwidthMismatch ArithException) m)
  ) ::
    Constraint

class
  ( UnifiedBV mode wordn intn,
    forall n.
    (1 <= n, KnownNat n) =>
    SafeDivision ArithException (wordn n) (ExceptT ArithException m),
    forall n.
    (1 <= n, KnownNat n) =>
    SafeLinearArith ArithException (wordn n) (ExceptT ArithException m),
    forall n.
    (1 <= n, KnownNat n) =>
    SafeSymShift ArithException (wordn n) (ExceptT ArithException m),
    forall n.
    (1 <= n, KnownNat n) =>
    SafeSymRotate ArithException (wordn n) (ExceptT ArithException m),
    forall n.
    (1 <= n, KnownNat n) =>
    SafeDivision ArithException (intn n) (ExceptT ArithException m),
    forall n.
    (1 <= n, KnownNat n) =>
    SafeLinearArith ArithException (intn n) (ExceptT ArithException m),
    forall n.
    (1 <= n, KnownNat n) =>
    SafeSymShift ArithException (intn n) (ExceptT ArithException m),
    forall n.
    (1 <= n, KnownNat n) =>
    SafeSymRotate ArithException (intn n) (ExceptT ArithException m),
    SafeSomeBVPair (SomeBV wordn) (SomeBV intn) m
  ) =>
  SafeUnifiedBV (mode :: EvaluationMode) wordn intn (m :: Type -> Type)

instance (MonadTryMerge m) => SafeUnifiedBV 'Con WordN IntN m

instance (MonadUnion m) => SafeUnifiedBV 'Sym SymWordN SymIntN m

class
  (UnifiedBV0 mode, SafeUnifiedBV mode (GetWordN mode) (GetIntN mode) m) =>
  SafeUnifiedBV0 mode m

instance (MonadTryMerge m) => SafeUnifiedBV0 'Con m

instance (MonadUnion m) => SafeUnifiedBV0 'Sym m
