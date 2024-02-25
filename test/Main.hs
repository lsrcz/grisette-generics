{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Main (main) where

import Control.Exception (ArithException)
import Control.Monad.Identity (Identity)
import Grisette
  ( IntN,
    SomeIntN,
    SomeSymIntN,
    SomeSymWordN,
    SomeWordN,
    SymBool,
    SymIntN,
    SymInteger,
    SymWordN,
    UnionM,
    WordN,
  )
import Grisette.Core.Data.BV (BitwidthMismatch)
import Grisette.Generics.BVLike
  ( BVLike,
    BVPair,
    SafeBVLike,
    SafeBVPair,
    SafeSomeBVLike,
    SafeSomeIntNLike,
    SafeSomeIntNSomeWordNLikePair,
    SafeSomeWordNLike,
    SomeBVLike,
    SomeIntNLike,
    SomeIntNSomeWordNLikePair,
    SomeWordNLike,
  )
import Grisette.Generics.BaseConstraint (BasicGrisetteType, ConSymConversion)
import Grisette.Generics.BoolLike (BoolLike)
import Grisette.Generics.IntegerLike (IntegerLike, SafeIntegerLike)
import Grisette.Generics.IntegralLike (NumLike, SafeIntegralLike)
import Grisette.Generics.SizedBVLike
  ( IntNLike,
    SafeIntNLike,
    SafeSizedBVLike,
    SafeWordNIntNLikePair,
    SafeWordNLike,
    SizedBVLike,
    WordNLike,
  )

boolConstraints ::
  forall bool.
  ( BasicGrisetteType bool bool,
    ConSymConversion Bool SymBool bool bool,
    BoolLike bool
  ) =>
  ()
boolConstraints = ()

boolConstraintsInstantiate :: ()
boolConstraintsInstantiate = boolConstraints @Bool

symBoolConstraintsInstantiate :: ()
symBoolConstraintsInstantiate = boolConstraints @SymBool

sizedBVConstraints ::
  forall bool word int m.
  ( SizedBVLike bool word,
    SizedBVLike bool int,
    SafeSizedBVLike bool word m,
    SafeSizedBVLike bool int m,
    WordNLike bool word,
    IntNLike bool int,
    SafeWordNLike bool word m,
    SafeIntNLike bool int m,
    SafeWordNIntNLikePair bool word int m,
    BasicGrisetteType bool (word 1),
    BasicGrisetteType bool (int 1),
    ConSymConversion (IntN 1) (SymIntN 1) bool (int 1),
    ConSymConversion (WordN 1) (SymWordN 1) bool (word 1),
    NumLike bool (word 1),
    NumLike bool (int 1),
    SafeIntegralLike ArithException bool (word 1) m,
    SafeIntegralLike ArithException bool (int 1) m,
    BVLike bool (word 1),
    BVLike bool (int 1),
    BVPair bool (word 1) (int 1),
    SafeBVLike ArithException bool (word 1) m,
    SafeBVLike ArithException bool (int 1) m,
    SafeBVPair ArithException bool (word 1) (int 1) m
  ) =>
  ()
sizedBVConstraints = ()

sizedIntNWordNConstraintsInstantiate :: ()
sizedIntNWordNConstraintsInstantiate =
  sizedBVConstraints @Bool @WordN @IntN @Identity

sizedSymIntNSymWordNConstraintsInstantiate :: ()
sizedSymIntNSymWordNConstraintsInstantiate =
  sizedBVConstraints @SymBool @SymWordN @SymIntN @UnionM

someBVConstraints ::
  forall bool word int m.
  ( BasicGrisetteType bool word,
    BasicGrisetteType bool int,
    ConSymConversion SomeIntN SomeSymIntN bool int,
    ConSymConversion SomeWordN SomeSymWordN bool word,
    NumLike bool word,
    NumLike bool int,
    SafeIntegralLike (Either BitwidthMismatch ArithException) bool word m,
    SafeIntegralLike (Either BitwidthMismatch ArithException) bool int m,
    BVLike bool word,
    BVLike bool int,
    BVPair bool word int,
    SafeBVLike (Either BitwidthMismatch ArithException) bool int m,
    SafeBVLike (Either BitwidthMismatch ArithException) bool word m,
    SafeBVPair (Either BitwidthMismatch ArithException) bool word int m,
    SomeBVLike bool word,
    SomeBVLike bool int,
    SafeSomeBVLike bool word m,
    SafeSomeBVLike bool int m,
    SomeIntNLike bool int,
    SafeSomeIntNLike bool int m,
    SomeWordNLike bool word,
    SafeSomeWordNLike bool word m,
    SomeIntNSomeWordNLikePair bool word int,
    SafeSomeIntNSomeWordNLikePair bool word int m
  ) =>
  ()
someBVConstraints = ()

someIntNWordNConstraintsInstantiate :: ()
someIntNWordNConstraintsInstantiate =
  someBVConstraints @Bool @SomeWordN @SomeIntN @Identity

someSymIntNSymWordNConstraintsInstantiate :: ()
someSymIntNSymWordNConstraintsInstantiate =
  someBVConstraints @SymBool @SomeSymWordN @SomeSymIntN @UnionM

integerConstraints ::
  forall bool integer m.
  ( BasicGrisetteType bool integer,
    ConSymConversion Integer SymInteger bool integer,
    NumLike bool integer,
    SafeIntegralLike ArithException bool integer m,
    IntegerLike bool integer,
    SafeIntegerLike bool integer m
  ) =>
  ()
integerConstraints = ()

integerConstraintsInstantiate :: ()
integerConstraintsInstantiate =
  integerConstraints @Bool @Integer @Identity

symIntegerConstraintsInstantiate :: ()
symIntegerConstraintsInstantiate =
  integerConstraints @SymBool @SymInteger @UnionM

-- We just test whether the code compiles for now to ensure that we don't
-- accidentally add imcompatible constraints to the types.
main :: IO ()
main =
  return $
    foldl1
      seq
      [ boolConstraintsInstantiate,
        symBoolConstraintsInstantiate,
        sizedIntNWordNConstraintsInstantiate,
        sizedSymIntNSymWordNConstraintsInstantiate,
        someIntNWordNConstraintsInstantiate,
        someSymIntNSymWordNConstraintsInstantiate,
        integerConstraintsInstantiate,
        symIntegerConstraintsInstantiate
      ]
