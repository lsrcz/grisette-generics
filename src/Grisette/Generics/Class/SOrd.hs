{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Grisette.Generics.Class.SOrd (SOrd (..)) where

import qualified Grisette
import Grisette.Generics.Class.MonadBranching (MonadBranching, liftUnionM)

class SOrd bool a where
  (.<=) :: a -> a -> bool
  (.>=) :: a -> a -> bool
  (.>) :: a -> a -> bool
  (.<) :: a -> a -> bool
  symCompare ::
    (MonadBranching bool ctx) => proxy bool -> a -> a -> ctx Ordering

instance (Ord a) => SOrd Bool a where
  (.<=) = (<=)
  (.>=) = (>=)
  (.>) = (>)
  (.<) = (<)
  symCompare _ x y = pure (compare x y)

instance (Grisette.SOrd a) => SOrd Grisette.SymBool a where
  (.<=) = (Grisette..<=)
  (.>=) = (Grisette..>=)
  (.>) = (Grisette..>)
  (.<) = (Grisette..<)
  symCompare _ x y = liftUnionM $ Grisette.symCompare x y
