{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Deriving (T (..), T1 (..)) where

import Control.DeepSeq (NFData)
import Data.Hashable (Hashable)
import GHC.Generics (Generic)
import Grisette (Default (Default), Mergeable, SEq, SOrd)
import Grisette.Unified.EvaluationMode (EvaluationMode (Con, Sym))
import Grisette.Unified.TH
  ( deriveAnyclass,
    deriveAnyclassWithMode,
    deriveNewtype,
    deriveStock,
    deriveStockWithMode,
    deriveViaDefault,
  )
import Grisette.Unified.UnifiedBV (UnifiedBV (GetWordN))
import Grisette.Unified.UnifiedBool (UnifiedBool (GetBool))
import Grisette.Unified.UnifiedData (UnifiedData (GetData))
import Language.Haskell.TH.Syntax (Lift)

data T mode a n = T (GetBool mode) (GetWordN mode n) (GetData mode (T mode a n))
  deriving (Generic)

deriveViaDefault ''T [''Mergeable, ''SEq, ''SOrd]
deriveStock ''T [''Show, ''Eq, ''Lift]
deriveAnyclass ''T [''NFData]
deriveAnyclassWithMode Sym ''T [''Hashable]
deriveStockWithMode Con ''T [''Ord]

newtype T1 mode = T1 (GetBool mode)
  deriving (Generic)

deriveNewtype ''T1 [''Mergeable, ''SEq, ''SOrd, ''Show]