{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Grisette.Generics.Class.SEq (SEq (..)) where

import qualified Grisette

class SEq bool a where
  (.==) :: a -> a -> bool
  (./=) :: a -> a -> bool

instance (Eq a) => SEq Bool a where
  (.==) = (==)
  (./=) = (/=)

instance (Grisette.SEq a) => SEq Grisette.SymBool a where
  (.==) = (Grisette..==)
  (./=) = (Grisette../=)
