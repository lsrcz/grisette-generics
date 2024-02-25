{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Grisette.Generics.IntegralLike
  ( NumLike,
    SafeIntegralLike,
  )
where

import Control.Monad.Except (ExceptT)
import Grisette.Core.Data.Class.SafeDivision (SafeDivision)
import Grisette.Core.Data.Class.SafeLinearArith (SafeLinearArith)
import Grisette.Generics.BaseConstraint (BasicGrisetteType)
import Grisette.Generics.BoolLike (BoolLike)
import Grisette.Generics.Class.MonadBranching (MonadBranching)

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
