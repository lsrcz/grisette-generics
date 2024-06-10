{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Main (main) where

import Unify (unifyTest)

-- We just test whether the code compiles for now to ensure that we don't
-- accidentally add imcompatible constraints to the types.
main :: IO ()
main =
  return $
    foldl1
      seq
      [ unifyTest
      ]
