{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}

module Grisette.Unified.BoolLike (BoolLike, MonadBranching) where

import Grisette
  ( LogicalOp,
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

type BoolLike bool =
  ( BasicGrisetteType bool bool,
    ConSymConversion Bool SymBool bool bool,
    SimpleMergeable bool bool,
    LogicalOp bool,
    UnifyInteger bool (UniInteger bool),
    UnifyWordNIntN bool (UniWordN bool) (UniIntN bool),
    UnifySomeWordNSomeIntN bool (SomeUniWordN bool) (SomeUniIntN bool)
  )

type MonadBranching bool m =
  ( BoolLike bool,
    Monad m,
    Branching bool m,
    SafeUnifyInteger bool (UniInteger bool) m,
    SafeUnifyWordNIntN bool (UniWordN bool) (UniIntN bool) m,
    SafeUnifySomeWordNSomeIntN bool (SomeUniWordN bool) (SomeUniIntN bool) m
  )
