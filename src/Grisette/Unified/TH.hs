{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Grisette.Unified.TH
  ( deriveNewtypeWithMode,
    deriveAnyclassWithMode,
    deriveStockWithMode,
    deriveViaDefaultWithMode,
  )
where

import Data.List (singleton)
import GHC.TypeNats (KnownNat, Nat, type (<=))
import Grisette (Default, Mergeable)
import Grisette.Unified.EvaluationMode (EvaluationMode)
import Grisette.Unified.IsMode (IsMode)
import Language.Haskell.TH (Ppr (ppr))
import Language.Haskell.TH.Syntax
  ( Dec (DataD, NewtypeD, StandaloneDerivD),
    DerivStrategy
      ( AnyclassStrategy,
        NewtypeStrategy,
        StockStrategy,
        ViaStrategy
      ),
    Info (TyConI),
    Name,
    Q,
    TyVarBndr (KindedTV, PlainTV),
    Type (AppT, ConT, StarT, VarT),
    reify,
    reportWarning,
  )

bndrName :: TyVarBndr flag -> Name
bndrName (PlainTV n _) = n
bndrName (KindedTV n _ _) = n

data Strategy = Stock | Newtype | Via | Anyclass

deriveWithMode :: Strategy -> Name -> Name -> Q [Dec]
deriveWithMode strategy name cls = do
  d <- reify name
  case d of
    TyConI (DataD _ _ bndrs _ _ _) -> genDeriveClause bndrs
    TyConI (NewtypeD _ _ bndrs _ _ _) -> genDeriveClause bndrs
    _ -> fail "Currently only non-GADTs data or newtype are supported."
  where
    genDeriveClause bndrs = do
      bndrConstraints <- concat <$> traverse genBndrConstraint bndrs
      deriveStrategy <- getStrategy bndrs
      dataType <- dataTypeWithTVars bndrs
      return
        [ StandaloneDerivD
            (Just deriveStrategy)
            bndrConstraints
            (AppT (ConT cls) dataType)
        ]
    getStrategy bndrs =
      case strategy of
        Stock -> return StockStrategy
        Newtype -> return NewtypeStrategy
        Via -> ViaStrategy <$> [t|Default $(dataTypeWithTVars bndrs)|]
        Anyclass -> return AnyclassStrategy
    dataTypeWithTVars = dataTypeWithTVars' . reverse
    dataTypeWithTVars' [] = return $ ConT name
    dataTypeWithTVars' (bndr : bndrs) =
      [t|$(dataTypeWithTVars' bndrs) $(return $ VarT (bndrName bndr))|]
    genBndrConstraint :: TyVarBndr flag -> Q [Type]
    genBndrConstraint (PlainTV n _) = do
      let tv = return $ VarT n
      reportWarning $ "Cannot infer the kind for " ++ show n ++ " assuming *"
      singleton <$> [t|($(return $ ConT cls) $tv, Mergeable $tv)|]
    genBndrConstraint (KindedTV n _ StarT) = do
      let tv = return $ VarT n
      sequence [[t|$(return $ ConT cls) $tv|], [t|Mergeable $tv|]]
    genBndrConstraint (KindedTV n _ kind@(ConT nm)) = do
      reifiedKind <- reify nm
      mode <- reify ''EvaluationMode
      nat <- reify ''Nat
      if
        | reifiedKind == mode -> singleton <$> [t|(IsMode $(return $ VarT n))|]
        | reifiedKind == nat ->
            sequence
              [[t|KnownNat $(return $ VarT n)|], [t|1 <= $(return $ VarT n)|]]
        | otherwise ->
            fail $
              "Unsupported kind in type arguments, currently only *, Nat, "
                <> "and EvaluationMode are supported, but got "
                <> show (ppr kind)
    genBndrConstraint (KindedTV _ _ kind) = do
      fail $
        "Unsupported kind in type arguments, currently only *, Nat, "
          <> "and EvaluationMode are supported, but got "
          <> show (ppr kind)

deriveInstancesWithMode :: Strategy -> Name -> [Name] -> Q [Dec]
deriveInstancesWithMode strategy name =
  fmap concat <$> traverse (deriveWithMode strategy name)

deriveNewtypeWithMode :: Name -> [Name] -> Q [Dec]
deriveNewtypeWithMode = deriveInstancesWithMode Newtype

deriveAnyclassWithMode :: Name -> [Name] -> Q [Dec]
deriveAnyclassWithMode = deriveInstancesWithMode Anyclass

deriveStockWithMode :: Name -> [Name] -> Q [Dec]
deriveStockWithMode = deriveInstancesWithMode Stock

deriveViaDefaultWithMode :: Name -> [Name] -> Q [Dec]
deriveViaDefaultWithMode = deriveInstancesWithMode Via
