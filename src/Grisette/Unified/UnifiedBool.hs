{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Grisette.Unified.UnifiedBool (UnifiedBool (..)) where

import Grisette (LogicalOp, SymBool)
import Grisette.Unified.BaseConstraint (BasicGrisetteType, ConSymConversion)
import Grisette.Unified.EvaluationMode (EvaluationMode (Con, Sym))

class
  ( BasicGrisetteType (GetBool mode),
    ConSymConversion Bool SymBool (GetBool mode),
    LogicalOp (GetBool mode)
  ) =>
  UnifiedBool (mode :: EvaluationMode)
  where
  type GetBool mode = bool | bool -> mode

instance UnifiedBool 'Con where
  type GetBool 'Con = Bool

instance UnifiedBool 'Sym where
  type GetBool 'Sym = SymBool
