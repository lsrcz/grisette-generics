{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Grisette.Unified.Class.SimpleMergeable (SimpleMergeable (..)) where

import qualified Grisette

class SimpleMergeable bool a where
  mrgIte :: bool -> a -> a -> a

instance SimpleMergeable Bool a where
  mrgIte True a _ = a
  mrgIte False _ a = a

instance
  (Grisette.SimpleMergeable a) =>
  SimpleMergeable Grisette.SymBool a
  where
  mrgIte = Grisette.mrgIte
