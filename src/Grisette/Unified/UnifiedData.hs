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
    (AllSyms v) => AllSyms u,
    (Eq v) => Eq u,
    (EvaluateSym v) => EvaluateSym u,
    (ExtractSymbolics v) => ExtractSymbolics u,
    (ITEOp v) => ITEOp u,
    (GPretty v) => GPretty u,
    (Hashable v) => Hashable u,
    (Lift v) => Lift u,
    (LogicalOp v) => LogicalOp u,
    (NFData v) => NFData u,
    (Num v) => Num u,
    (SEq v) => SEq u,
    (Show v) => Show u,
    (SOrd v) => SOrd u,
    (SubstituteSym v) => SubstituteSym u,
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
