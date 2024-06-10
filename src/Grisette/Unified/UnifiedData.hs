{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Grisette.Unified.UnifiedData (UnifiedData (..), UnifiedData0) where

import Control.DeepSeq (NFData)
import Data.Hashable (Hashable)
import Grisette
  ( AllSyms,
    EvaluateSym,
    ExtractSymbolics,
    GPretty,
    ITEOp,
    LogicalOp,
    Mergeable,
    SEq,
    SOrd,
    SubstituteSym,
    ToCon,
    ToSym,
    UnionM,
    liftUnionM,
    mrgReturn,
  )
import Grisette.Unified.Class.UnifiedBranching (UnifiedBranching)
import Grisette.Unified.EvaluationMode (EvaluationMode (Con, Sym))
import Language.Haskell.TH.Syntax (Lift)

class
  ( u ~ GetData mode v,
    Mergeable u,
    (Show v) => Show u,
    (Hashable v) => Hashable u,
    (NFData v) => NFData u,
    (Lift v) => Lift u,
    (AllSyms v) => AllSyms u,
    (EvaluateSym v) => EvaluateSym u,
    (ExtractSymbolics v) => ExtractSymbolics u,
    (GPretty v) => GPretty u,
    (ITEOp v) => ITEOp u,
    (SEq v) => SEq u,
    (SOrd v) => SOrd u,
    (SubstituteSym v) => SubstituteSym u,
    (LogicalOp v) => LogicalOp u,
    (Eq v) => Eq u,
    (Num v) => Num u,
    forall b. (ToCon v b) => ToCon u b,
    forall a. (ToSym a v) => ToSym a u
  ) =>
  UnifiedData (mode :: EvaluationMode) v u
    | u mode -> v,
      u v -> mode
  where
  type GetData mode v
  wrapData :: v -> u
  extractData :: (Monad m, UnifiedBranching mode m) => u -> m v

instance (Mergeable v) => UnifiedData 'Con v v where
  type GetData 'Con v = v
  wrapData = id
  extractData = mrgReturn

instance (Mergeable v) => UnifiedData 'Sym v (UnionM v) where
  type GetData 'Sym v = UnionM v
  wrapData = mrgReturn
  extractData = liftUnionM

class (UnifiedData bool v (GetData bool v)) => UnifiedData0 bool v

instance (UnifiedData bool v (GetData bool v)) => UnifiedData0 bool v
