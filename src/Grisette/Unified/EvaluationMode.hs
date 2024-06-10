{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Grisette.Unified.EvaluationMode (EvaluationMode (..), IsConMode) where

import Language.Haskell.TH.Syntax (Lift)

data EvaluationMode = Con | Sym deriving (Lift)

type family IsConMode (mode :: EvaluationMode) = (r :: Bool) | r -> mode where
  IsConMode 'Con = 'True
  IsConMode 'Sym = 'False
