{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Grisette.Unified.UnifiedData (UnifyData (..), UnifyData1) where

import Grisette (Mergeable, SymBool, UnionM, mrgReturn)
import Grisette.Unified.Class.Branching (Branching, liftUnionM)

class (u ~ UniData bool v, Mergeable u) => UnifyData bool v u where
  type UniData bool v
  wrapData :: (Mergeable v) => v -> UniData bool v
  extractData ::
    (Monad m, Branching bool m, Mergeable v) =>
    UniData bool v ->
    m v

instance Mergeable v => UnifyData Bool v v where
  type UniData Bool v = v
  wrapData = id
  extractData = mrgReturn

instance Mergeable v => UnifyData SymBool v (UnionM v) where
  type UniData SymBool v = UnionM v
  wrapData = mrgReturn
  extractData = liftUnionM

class (UnifyData bool v (UniData bool v)) => UnifyData1 bool v

instance (UnifyData bool v (UniData bool v)) => UnifyData1 bool v
