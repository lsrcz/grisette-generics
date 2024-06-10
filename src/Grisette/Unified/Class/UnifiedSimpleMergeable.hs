{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Grisette.Unified.Class.UnifiedSimpleMergeable
  ( UnifiedSimpleMergeable (..),
  )
where

import Data.Kind (Constraint)
import Data.Type.Bool (If)
import Grisette (SimpleMergeable)
import qualified Grisette
import Grisette.Unified.EvaluationMode (EvaluationMode (Con, Sym), IsConMode)
import Grisette.Unified.UnifiedBool (UnifiedBool (GetBool))

class
  (If (IsConMode mode) (() :: Constraint) (SimpleMergeable a)) =>
  UnifiedSimpleMergeable mode a
  where
  mrgIte :: GetBool mode -> a -> a -> a

instance UnifiedSimpleMergeable 'Con a where
  mrgIte True a _ = a
  mrgIte False _ a = a

instance
  (SimpleMergeable a) =>
  UnifiedSimpleMergeable 'Sym a
  where
  mrgIte = Grisette.mrgIte
