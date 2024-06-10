{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Grisette.Unified.Class.UnifiedSEq (UnifiedSEq (..)) where

import Data.Type.Bool (If)
import Grisette (SEq)
import qualified Grisette
import Grisette.Unified.EvaluationMode (EvaluationMode (Con, Sym), IsConMode)
import Grisette.Unified.UnifiedBool (UnifiedBool (GetBool))

class
  (If (IsConMode mode) (Eq a) (SEq a)) =>
  UnifiedSEq mode a
  where
  (.==) :: a -> a -> GetBool mode
  (./=) :: a -> a -> GetBool mode

instance (Eq a) => UnifiedSEq 'Con a where
  (.==) = (==)
  (./=) = (/=)

instance (SEq a) => UnifiedSEq 'Sym a where
  (.==) = (Grisette..==)
  (./=) = (Grisette../=)
