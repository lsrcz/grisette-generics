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
    SymIntN,
    UnionM,
    mrgReturn,
  )
import Grisette.Lib.Control.Monad.Except (mrgModifyError)
import Grisette.Unified.EvaluationMode (EvaluationMode (Con, Sym))
import Grisette.Unified.IsMode (IsMode, MonadWithMode)
import Grisette.Unified.UnifiedBV (UnifiedBV (GetIntN, GetWordN))
import Grisette.Unified.UnifiedData
  ( UnifiedData (GetData, extractData, wrapData),
  )

data T mode n
  = T (GetIntN mode n) (GetWordN mode n) (GetData mode (T mode n))
  | Nil
  deriving (Generic)

deriving via
  (Default (T mode n))
  instance
    (IsMode mode, 1 <= n, KnownNat n) =>
    (Mergeable (T mode n))

testMonadWithMode ::
  forall bool n m.
  ( MonadWithMode bool m,
    KnownNat n,
    1 <= n,
    MonadError String m
  ) =>
  T bool n ->
  m (GetIntN bool n)
testMonadWithMode (T i w t) = do
  tl :: T bool n <- extractData @bool t
  v <- testMonadWithMode tl
  r <-
    mrgModifyError (\(_ :: ArithException) -> "err") $
      safeDiv @ArithException i (toSigned w)
  mrgReturn $ v + r
testMonadWithMode Nil = mrgReturn 0

testConstruction ::
  forall bool n m.
  (MonadWithMode bool m, 1 <= n, KnownNat n) =>
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
    [ ( testMonadWithMode (T 1 2 Nil :: T 'Con 3) ::
          Either String (IntN 3)
      )
        `seq` (),
      ( testMonadWithMode (T 1 2 (mrgReturn Nil) :: T 'Sym 3) ::
          ExceptT String UnionM (SymIntN 3)
      )
        `seq` (),
      ( testConstruction (T 1 2 Nil :: T 'Con 3) :: Either String (T 'Con 3)
      )
        `seq` (),
      (testConstruction (T 1 2 Nil) :: ExceptT String UnionM (T 'Con 3)) `seq`
        (),
      ( testConstruction (T 1 2 (mrgReturn Nil)) ::
          ExceptT String UnionM (T 'Sym 3)
      )
        `seq` ()
    ]
