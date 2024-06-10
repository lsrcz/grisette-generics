{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE MonoLocalBinds #-}

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
import Grisette.Unified.Class.UnifiedSEq (UnifiedSEq ((.==)))
import Grisette.Unified.IsMode (IsMode)
import Grisette.Unified.UnifiedBool (GetBool)

symElem ::
  (Foldable t, IsMode mode, UnifiedSEq mode a) => a -> t a -> GetBool mode
symElem x = symAny (.== x)
{-# INLINE symElem #-}

symAnd :: (Foldable t, IsMode mode) => t (GetBool mode) -> GetBool mode
symAnd = foldl' (.&&) (toSym True)
{-# INLINE symAnd #-}

symOr :: (Foldable t, IsMode mode) => t (GetBool mode) -> GetBool mode
symOr = foldl' (.||) (toSym False)
{-# INLINE symOr #-}

symAny ::
  (Foldable t, IsMode mode) => (a -> GetBool mode) -> t a -> GetBool mode
symAny f = foldl' (\acc v -> acc .|| f v) (toSym False)
{-# INLINE symAny #-}

symAll ::
  (Foldable t, IsMode mode) => (a -> GetBool mode) -> t a -> GetBool mode
symAll f = foldl' (\acc v -> acc .&& f v) (toSym True)
{-# INLINE symAll #-}

symNotElem ::
  (Foldable t, IsMode mode, UnifiedSEq mode a) => a -> t a -> GetBool mode
symNotElem x = symNot . symElem x
{-# INLINE symNotElem #-}