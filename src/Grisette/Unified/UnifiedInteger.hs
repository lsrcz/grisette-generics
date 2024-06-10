{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Grisette.Unified.UnifiedInteger
  ( UnifiedInteger (..),
    SafeUnifiedInteger,
  )
where

import Control.Exception (ArithException)
import Control.Monad.Except (ExceptT)
import Data.Kind (Type)
import Grisette (SymInteger)
import Grisette.Unified.BaseConstraint
  ( BasicGrisetteType,
    ConSymConversion,
    SafeIntegral,
  )
import Grisette.Unified.EvaluationMode (EvaluationMode (Con, Sym))
import Grisette.Unified.UnifiedConstraint (UnifiedPrimitive)

class
  ( BasicGrisetteType (GetInteger mode),
    ConSymConversion Integer SymInteger (GetInteger mode),
    Num (GetInteger mode),
    UnifiedPrimitive mode (GetInteger mode)
  ) =>
  UnifiedInteger (mode :: EvaluationMode)
  where
  type GetInteger mode = bool | bool -> mode

instance UnifiedInteger 'Con where
  type GetInteger 'Con = Integer

instance UnifiedInteger 'Sym where
  type GetInteger 'Sym = SymInteger

class
  ( UnifiedInteger mode,
    SafeIntegral ArithException (GetInteger mode) (ExceptT ArithException m)
  ) =>
  SafeUnifiedInteger (mode :: EvaluationMode) (m :: Type -> Type)

instance
  ( UnifiedInteger mode,
    SafeIntegral ArithException (GetInteger mode) (ExceptT ArithException m)
  ) =>
  SafeUnifiedInteger mode m
