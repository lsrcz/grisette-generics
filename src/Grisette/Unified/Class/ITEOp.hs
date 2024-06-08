{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Grisette.Unified.Class.ITEOp (ITEOp (..)) where

import qualified Grisette

class ITEOp bool v where
  symIte :: bool -> v -> v -> v

instance ITEOp Bool a where
  symIte True t _ = t
  symIte False _ e = e

instance (Grisette.ITEOp a) => ITEOp Grisette.SymBool a where
  symIte = Grisette.symIte
