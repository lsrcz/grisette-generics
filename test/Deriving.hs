{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Deriving (T (..), T1 (..)) where

import Control.DeepSeq (NFData)
import GHC.Generics (Generic)
import Grisette (Default (Default), Mergeable, SEq, SOrd)
import Grisette.Unified.TH
  ( deriveAnyclassWithMode,
    deriveNewtypeWithMode,
    deriveStockWithMode,
    deriveViaDefaultWithMode,
  )
import Grisette.Unified.UnifiedBV (UnifiedBV (GetWordN))
import Grisette.Unified.UnifiedBool (UnifiedBool (GetBool))
import Grisette.Unified.UnifiedData (UnifiedData (GetData))
import Language.Haskell.TH.Syntax (Lift)

data T mode a n = T (GetBool mode) (GetWordN mode n) (GetData mode (T mode a n))
  deriving (Generic)

deriveViaDefaultWithMode ''T [''Mergeable, ''SEq, ''SOrd]
deriveStockWithMode ''T [''Show, ''Eq, ''Lift]
deriveAnyclassWithMode ''T [''NFData]

newtype T1 mode = T1 (GetBool mode)
  deriving (Generic)

deriveNewtypeWithMode ''T1 [''Mergeable, ''SEq, ''SOrd, ''Show]