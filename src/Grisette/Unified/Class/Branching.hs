{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}

module Grisette.Unified.Class.Branching
  ( Branching (..),
    liftUnionM,
  )
where

import Grisette
  ( Mergeable,
    SymBool,
    TryMerge,
    UnionM,
    UnionMergeable1,
    tryMerge,
    pattern If,
    pattern Single,
  )
import qualified Grisette

class (TryMerge m) => Branching bool m where
  mrgIf :: (Mergeable a) => bool -> m a -> m a -> m a

instance (TryMerge m) => Branching Bool m where
  mrgIf True t _ = tryMerge t
  mrgIf False _ e = tryMerge e

instance (UnionMergeable1 m) => Branching SymBool m where
  mrgIf = Grisette.mrgIf

liftUnionM :: (Monad m, Branching SymBool m, Mergeable a) => UnionM a -> m a
liftUnionM u = case u of
  Single a -> pure a
  If a l r -> mrgIf a (liftUnionM l) (liftUnionM r)
  _ -> error "liftUnionM: impossible"
