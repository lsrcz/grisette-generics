{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Grisette.Unified.IntegralLike
  ( NumLike,
    SafeIntegralLike,
  )
where

import Control.Monad.Except (ExceptT)
import Grisette (SafeDivision, SafeLinearArith)
import Grisette.Unified.BaseConstraint (BasicGrisetteType)
import Grisette.Unified.Class.Branching (Branching)

type NumLike bool int =
  ( BasicGrisetteType bool int,
    Num int
  )

type SafeIntegralLike e bool int m =
  ( NumLike bool int,
    Monad m,
    Branching bool m,
    SafeDivision e int (ExceptT e m),
    SafeLinearArith e int (ExceptT e m)
  )
