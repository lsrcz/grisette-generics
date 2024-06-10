{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Grisette.Unified.Class.UnifiedBranching (UnifiedBranching (..)) where

import Data.Kind (Constraint)
import Data.Type.Bool (If)
import Grisette
  ( Mergeable,
    TryMerge,
    UnionMergeable1,
    tryMerge,
  )
import qualified Grisette
import Grisette.Unified.EvaluationMode (EvaluationMode (Con, Sym), IsConMode)
import Grisette.Unified.UnifiedBool (UnifiedBool (GetBool))

class
  ( TryMerge m,
    If (IsConMode mode) (() :: Constraint) (UnionMergeable1 m)
  ) =>
  UnifiedBranching (mode :: EvaluationMode) m
  where
  mrgIf :: (Mergeable a) => GetBool mode -> m a -> m a -> m a

instance (TryMerge m) => UnifiedBranching 'Con m where
  mrgIf True t _ = tryMerge t
  mrgIf False _ e = tryMerge e

instance (UnionMergeable1 m) => UnifiedBranching 'Sym m where
  mrgIf = Grisette.mrgIf
