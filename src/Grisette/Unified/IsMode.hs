{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MonoLocalBinds #-}

module Grisette.Unified.IsMode (IsMode, MonadWithMode) where

import Data.Kind (Constraint)
import Grisette (Mergeable)
import Grisette.Unified.Class.UnifiedBranching (UnifiedBranching)
import Grisette.Unified.UnifiedBV
  ( SafeUnifiedBV0,
    UnifiedBV0,
  )
import Grisette.Unified.UnifiedBool (UnifiedBool (GetBool))
import Grisette.Unified.UnifiedConstraint (UnifiedPrimitive)
import Grisette.Unified.UnifiedData (UnifiedData0)
import Grisette.Unified.UnifiedInteger (SafeUnifiedInteger, UnifiedInteger)

type IsMode mode =
  ( UnifiedBool mode,
    UnifiedPrimitive mode (GetBool mode),
    UnifiedInteger mode,
    UnifiedBV0 mode,
    forall v. (Mergeable v) => UnifiedData0 mode v :: Constraint
  )

type MonadWithMode mode m =
  ( IsMode mode,
    Monad m,
    UnifiedBranching mode m,
    SafeUnifiedInteger mode m,
    SafeUnifiedBV0 mode m
  )
