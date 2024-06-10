{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Grisette.Unified.Class.UnifiedITEOp (UnifiedITEOp (..)) where

import Data.Kind (Constraint)
import Data.Type.Bool (If)
import Grisette (ITEOp)
import qualified Grisette
import Grisette.Unified.EvaluationMode (EvaluationMode (Con, Sym), IsConMode)
import Grisette.Unified.UnifiedBool (UnifiedBool (GetBool))

class
  (If (IsConMode mode) (() :: Constraint) (ITEOp v)) =>
  UnifiedITEOp mode v
  where
  symIte :: GetBool mode -> v -> v -> v

instance UnifiedITEOp 'Con a where
  symIte True t _ = t
  symIte False _ e = e

instance (ITEOp a) => UnifiedITEOp 'Sym a where
  symIte = Grisette.symIte
