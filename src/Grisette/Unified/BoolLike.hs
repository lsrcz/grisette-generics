{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MonoLocalBinds #-}

module Grisette.Unified.BoolLike (BoolLike, MonadBranching) where

import Data.Kind (Constraint)
import Grisette
  ( LogicalOp,
    Mergeable,
    SymBool,
  )
import Grisette.Unified.BVLike
  ( SafeUnifySomeWordNSomeIntN,
    UnifySomeWordNSomeIntN (SomeUniIntN, SomeUniWordN),
  )
import Grisette.Unified.BaseConstraint
  ( BasicGrisetteType,
    ConSymConversion,
  )
import Grisette.Unified.Class.Branching (Branching)
import Grisette.Unified.Class.SimpleMergeable (SimpleMergeable)
import Grisette.Unified.IntegerLike
  ( SafeUnifyInteger,
    UnifyInteger (UniInteger),
  )
import Grisette.Unified.SizedBVLike
  ( SafeUnifyWordNIntN,
    UnifyWordNIntN (UniIntN, UniWordN),
  )
import Grisette.Unified.UnifiedData (UnifyData1)

type BoolLike bool =
  ( BasicGrisetteType bool bool,
    ConSymConversion Bool SymBool bool bool,
    SimpleMergeable bool bool,
    LogicalOp bool,
    UnifyInteger bool (UniInteger bool),
    UnifyWordNIntN bool (UniWordN bool) (UniIntN bool),
    UnifySomeWordNSomeIntN bool (SomeUniWordN bool) (SomeUniIntN bool),
    forall v. (Mergeable v) => UnifyData1 bool v :: Constraint
  )

type MonadBranching bool m =
  ( BoolLike bool,
    Monad m,
    Branching bool m,
    SafeUnifyInteger bool (UniInteger bool) m,
    SafeUnifyWordNIntN bool (UniWordN bool) (UniIntN bool) m,
    SafeUnifySomeWordNSomeIntN bool (SomeUniWordN bool) (SomeUniIntN bool) m
  )
