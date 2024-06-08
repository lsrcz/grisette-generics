{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImpredicativeTypes #-}

module Grisette.Unified.Lib.Data.Foldable
  ( symAnd,
    symOr,
    symAny,
    symAll,
    symElem,
    symNotElem,
  )
where

import Data.Foldable (Foldable (foldl'))
import Grisette (LogicalOp (symNot, (.&&), (.||)), ToSym (toSym))
import Grisette.Unified.BoolLike (BoolLike)
import Grisette.Unified.Class.SEq (SEq ((.==)))

symElem :: (Foldable t, BoolLike bool, SEq bool a) => a -> t a -> bool
symElem x = symAny (.== x)
{-# INLINE symElem #-}

symAnd :: (Foldable t, BoolLike bool) => t bool -> bool
symAnd = foldl' (.&&) (toSym True)
{-# INLINE symAnd #-}

symOr :: (Foldable t, BoolLike bool) => t bool -> bool
symOr = foldl' (.||) (toSym False)
{-# INLINE symOr #-}

symAny :: (Foldable t, BoolLike bool) => (a -> bool) -> t a -> bool
symAny f = foldl' (\acc v -> acc .|| f v) (toSym False)
{-# INLINE symAny #-}

symAll :: (Foldable t, BoolLike bool) => (a -> bool) -> t a -> bool
symAll f = foldl' (\acc v -> acc .&& f v) (toSym True)
{-# INLINE symAll #-}

symNotElem :: (Foldable t, BoolLike bool, SEq bool a) => a -> t a -> bool
symNotElem x = symNot . symElem x
{-# INLINE symNotElem #-}
