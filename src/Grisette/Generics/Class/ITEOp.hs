{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Grisette.Generics.Class.ITEOp (ITEOp (..)) where

import GHC.TypeNats (KnownNat, type (<=))
import Grisette (Default (Default))
import qualified Grisette

class ITEOp bool v where
  symIte :: bool -> v -> v -> v

instance ITEOp Bool a where
  symIte True t _ = t
  symIte False _ e = e

instance (Grisette.ITEOp a) => ITEOp Grisette.SymBool (Default a) where
  symIte cond (Default l) (Default r) = Default (Grisette.symIte cond l r)

#define DERIVE_ITEOP_INSTANCE(type) \
  deriving via (Default (type)) instance ITEOp Grisette.SymBool (type);

#define DERIVE_ITEOP_INSTANCE_CONSTRAINED(constraint, type) \
  deriving via (Default (type)) \
    instance constraint => ITEOp Grisette.SymBool (type);

#if 1
DERIVE_ITEOP_INSTANCE(Grisette.SymBool)
DERIVE_ITEOP_INSTANCE(Grisette.SymInteger)
DERIVE_ITEOP_INSTANCE(Grisette.SomeSymIntN)
DERIVE_ITEOP_INSTANCE(Grisette.SomeSymWordN)
DERIVE_ITEOP_INSTANCE_CONSTRAINED((Grisette.ITEOp a, Grisette.Mergeable a), Grisette.UnionM a)
DERIVE_ITEOP_INSTANCE_CONSTRAINED((KnownNat n, 1 <= n), Grisette.SymIntN n)
DERIVE_ITEOP_INSTANCE_CONSTRAINED((KnownNat n, 1 <= n), Grisette.SymWordN n)
DERIVE_ITEOP_INSTANCE_CONSTRAINED(
  ( Grisette.SupportedPrim ca,
    Grisette.SupportedPrim cb,
    Grisette.LinkedRep ca sa,
    Grisette.LinkedRep cb sb
  ), sa Grisette.-~> sb)
DERIVE_ITEOP_INSTANCE_CONSTRAINED(
  ( Grisette.SupportedPrim ca,
    Grisette.SupportedPrim cb,
    Grisette.LinkedRep ca sa,
    Grisette.LinkedRep cb sb
  ), sa Grisette.=~> sb)
#endif
