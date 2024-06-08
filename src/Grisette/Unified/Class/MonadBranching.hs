{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}

module Grisette.Unified.Class.MonadBranching
  ( MonadBranching (..),
    liftUnionM,
  )
where

import Grisette
  ( Mergeable,
    MonadTryMerge,
    MonadUnion,
    SymBool,
    UnionM,
    tryMerge,
    pattern If,
    pattern Single,
  )
import qualified Grisette

class (MonadTryMerge m) => MonadBranching bool m where
  mrgIf :: (Mergeable a) => bool -> m a -> m a -> m a

instance (MonadTryMerge m) => MonadBranching Bool m where
  mrgIf True t _ = tryMerge t
  mrgIf False _ e = tryMerge e

instance (MonadUnion m) => MonadBranching SymBool m where
  mrgIf = Grisette.mrgIf

liftUnionM :: (MonadBranching SymBool m, Mergeable a) => UnionM a -> m a
liftUnionM u = case u of
  Single a -> pure a
  If a l r -> mrgIf a (liftUnionM l) (liftUnionM r)
  _ -> error "liftUnionM: impossible"
