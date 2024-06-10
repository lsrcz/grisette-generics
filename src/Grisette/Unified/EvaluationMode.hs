{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Grisette.Unified.EvaluationMode (EvaluationMode (..), IsConMode) where

data EvaluationMode = Con | Sym

type family IsConMode (mode :: EvaluationMode) = (r :: Bool) | r -> mode where
  IsConMode 'Con = 'True
  IsConMode 'Sym = 'False
