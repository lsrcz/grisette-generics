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
    IntN,
    Mergeable,
    SafeDivision (safeDiv),
    SignConversion (toSigned, toUnsigned),
    SymBool,
    SymIntN,
    UnionM,
    mrgReturn,
  )
import Grisette.Lib.Control.Monad.Except (mrgModifyError)
import Grisette.Unified.BoolLike (BoolLike, MonadBranching)
import Grisette.Unified.SizedBVLike (UnifyWordNIntN (UniIntN, UniWordN))
import Grisette.Unified.UnifiedData (UnifyData (UniData, extractData, wrapData))

data T bool n
  = T (UniIntN bool n) (UniWordN bool n) (UniData bool (T bool n))
  | Nil
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
  m (UniIntN bool n)
testMonadBranching (T i w t) = do
  tl :: T bool n <- extractData @bool t
  v <- testMonadBranching tl
  r <-
    mrgModifyError (\(_ :: ArithException) -> "err") $
      safeDiv @ArithException i (toSigned w)
  mrgReturn $ v + r
testMonadBranching Nil = mrgReturn 0

testConstruction ::
  forall bool n m.
  (MonadBranching bool m, 1 <= n, KnownNat n) =>
  T bool n ->
  m (T bool n)
testConstruction (T i w t) = do
  tl :: T bool n <- extractData @bool t
  ctl <- testConstruction tl
  mrgReturn $ T (toSigned w) (toUnsigned i) (wrapData @bool ctl)
testConstruction Nil = mrgReturn Nil

unifyTest :: ()
unifyTest =
  foldl1
    seq
    [ ( testMonadBranching (T 1 2 Nil :: T Bool 3) ::
          Either String (IntN 3)
      )
        `seq` (),
      ( testMonadBranching (T 1 2 (mrgReturn Nil) :: T SymBool 3) ::
          ExceptT String UnionM (SymIntN 3)
      )
        `seq` (),
      ( testConstruction (T 1 2 Nil :: T Bool 3) :: Either String (T Bool 3)
      )
        `seq` (),
      (testConstruction (T 1 2 Nil) :: ExceptT String UnionM (T Bool 3)) `seq`
        (),
      ( testConstruction (T 1 2 (mrgReturn Nil)) ::
          ExceptT String UnionM (T SymBool 3)
      )
        `seq` ()
    ]
