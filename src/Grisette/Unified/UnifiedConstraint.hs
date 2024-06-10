{-# LANGUAGE ConstraintKinds #-}

module Grisette.Unified.UnifiedConstraint (UnifiedPrimitive) where

import Grisette.Unified.Class.UnifiedITEOp (UnifiedITEOp)
import Grisette.Unified.Class.UnifiedSEq (UnifiedSEq)
import Grisette.Unified.Class.UnifiedSOrd (UnifiedSOrd)
import Grisette.Unified.Class.UnifiedSimpleMergeable (UnifiedSimpleMergeable)

type UnifiedPrimitive mode t =
  ( UnifiedITEOp mode t,
    UnifiedSEq mode t,
    UnifiedSOrd mode t,
    UnifiedSimpleMergeable mode t
  )
