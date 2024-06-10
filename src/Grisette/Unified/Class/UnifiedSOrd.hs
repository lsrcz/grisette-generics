{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Grisette.Unified.Class.UnifiedSOrd (UnifiedSOrd (..)) where

import Data.Type.Bool (If)
import Grisette (SOrd, liftUnionM)
import qualified Grisette
import Grisette.Unified.Class.UnifiedBranching (UnifiedBranching)
import Grisette.Unified.EvaluationMode (EvaluationMode (Con, Sym), IsConMode)
import Grisette.Unified.UnifiedBool (UnifiedBool (GetBool))

class
  (If (IsConMode mode) (Ord a) (SOrd a)) =>
  UnifiedSOrd mode a
  where
  (.<=) :: a -> a -> GetBool mode
  (.>=) :: a -> a -> GetBool mode
  (.>) :: a -> a -> GetBool mode
  (.<) :: a -> a -> GetBool mode
  symCompare ::
    (Monad ctx, UnifiedBranching mode ctx) => a -> a -> ctx Ordering

instance (Ord a) => UnifiedSOrd 'Con a where
  (.<=) = (<=)
  (.>=) = (>=)
  (.>) = (>)
  (.<) = (<)
  symCompare x y = pure (compare x y)

instance (SOrd a) => UnifiedSOrd 'Sym a where
  (.<=) = (Grisette..<=)
  (.>=) = (Grisette..>=)
  (.>) = (Grisette..>)
  (.<) = (Grisette..<)
  symCompare x y = liftUnionM $ Grisette.symCompare x y
