{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Grisette.Unified.UnifiedData (UnifyData (..), UnifyData1) where

import Grisette
  ( AllSyms,
    EvaluateSym,
    ExtractSymbolics,
    GPretty,
    ITEOp,
    Mergeable,
    SEq,
    SOrd,
    SubstituteSym,
    SymBool,
    ToCon,
    UnionM,
    mrgReturn, ToSym, LogicalOp,
  )
import Grisette.Unified.Class.Branching (Branching, liftUnionM)

class
  ( u ~ UniData bool v,
    Mergeable u,
    (Show v) => Show u,
    (AllSyms v) => AllSyms u,
    (EvaluateSym v) => EvaluateSym u,
    (ExtractSymbolics v) => ExtractSymbolics u,
    (GPretty v) => GPretty u,
    (ITEOp v) => ITEOp u,
    (Grisette.SEq v) => Grisette.SEq u,
    (Grisette.SOrd v) => Grisette.SOrd u,
    (SubstituteSym v) => SubstituteSym u,
    (LogicalOp v) => LogicalOp u,
    (Eq v) => Eq u,
    (Num v) => Num u,
    forall b. (ToCon v b) => ToCon u b,
    forall a. (ToSym a v) => ToSym a u
  ) =>
  UnifyData bool v u
  where
  type UniData bool v
  wrapData :: (Mergeable v) => v -> UniData bool v
  extractData ::
    (Monad m, Branching bool m, Mergeable v) =>
    UniData bool v ->
    m v

instance (Mergeable v) => UnifyData Bool v v where
  type UniData Bool v = v
  wrapData = id
  extractData = mrgReturn

instance (Mergeable v) => UnifyData SymBool v (UnionM v) where
  type UniData SymBool v = UnionM v
  wrapData = mrgReturn
  extractData = liftUnionM

class (UnifyData bool v (UniData bool v)) => UnifyData1 bool v

instance (UnifyData bool v (UniData bool v)) => UnifyData1 bool v
