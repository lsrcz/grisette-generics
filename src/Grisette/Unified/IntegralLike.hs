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
import Grisette.Unified.BoolLike (BoolLike)
import Grisette.Unified.Class.MonadBranching (MonadBranching)

type NumLike bool int =
  ( BasicGrisetteType bool int,
    BoolLike bool,
    Num int
  )

type SafeIntegralLike e bool int m =
  ( NumLike bool int,
    MonadBranching bool m,
    SafeDivision e int (ExceptT e m),
    SafeLinearArith e int (ExceptT e m)
  )
