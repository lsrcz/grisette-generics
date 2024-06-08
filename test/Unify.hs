{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Unify (unifyTest) where

import Control.Exception (ArithException)
import Control.Monad.Error.Class (MonadError)
import Control.Monad.Except (ExceptT)
import GHC.Generics (Generic)
import GHC.TypeLits (KnownNat, type (<=))
import Grisette
  ( Default (Default),
    Mergeable,
    SafeDivision (safeDiv),
    SignConversion (toSigned, toUnsigned),
    SymBool,
    UnionM,
    mrgReturn,
  )
import Grisette.Lib.Control.Monad.Except (mrgModifyError)
import Grisette.Unified.BoolLike (BoolLike, MonadBranching)
import Grisette.Unified.SizedBVLike (UnifyWordNIntN (UniIntN, UniWordN))

data T bool n = T (UniIntN bool n) (UniWordN bool n)
  deriving (Generic)

deriving via
  (Default (T bool n))
  instance
    (BoolLike bool, 1 <= n, KnownNat n) =>
    (Mergeable (T bool n))

testMonadBranching ::
  forall bool n m.
  ( MonadBranching bool m,
    KnownNat n,
    1 <= n,
    MonadError String m
  ) =>
  T bool n ->
  m (T bool n)
testMonadBranching (T i w) = do
  r <-
    mrgModifyError (\(_ :: ArithException) -> "err") $
      safeDiv @ArithException i (toSigned w)
  mrgReturn $ T r (toUnsigned r)

unifyTest :: ()
unifyTest =
  foldl1
    seq
    [ (testMonadBranching (T 1 2) :: Either String (T Bool 3)) `seq` (),
      (testMonadBranching (T 1 2) :: ExceptT String UnionM (T SymBool 3)) `seq`
        ()
    ]
