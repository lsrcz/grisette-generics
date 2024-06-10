{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Grisette.Unified.TH
  ( deriveNewtype,
    deriveAnyclass,
    deriveStock,
    deriveViaDefault,
    deriveNewtypeWithMode,
    deriveAnyclassWithMode,
    deriveStockWithMode,
    deriveViaDefaultWithMode,
  )
where

import Data.List (singleton)
import Data.Maybe (isJust, isNothing)
import GHC.TypeNats (KnownNat, Nat, type (<=))
import Grisette (Default, Mergeable)
import Grisette.Unified.EvaluationMode (EvaluationMode (Con, Sym))
import Grisette.Unified.IsMode (IsMode)
import Language.Haskell.TH (Ppr (ppr), Type (PromotedT))
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

deriveWithMode :: Maybe EvaluationMode -> Strategy -> Name -> Name -> Q [Dec]
deriveWithMode evmode strategy name cls = do
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
    dataTypeWithTVars' (bndr : bndrs)
      | binderIsMode bndr && isJust evmode =
          case evmode of
            Just Con ->
              [t|$(dataTypeWithTVars' bndrs) $(return $ PromotedT 'Con)|]
            Just Sym ->
              [t|$(dataTypeWithTVars' bndrs) $(return $ PromotedT 'Sym)|]
            Nothing -> fail "EvaluationMode is not provided"
    dataTypeWithTVars' (bndr : bndrs) =
      [t|$(dataTypeWithTVars' bndrs) $(return $ VarT (bndrName bndr))|]

    binderIsMode :: TyVarBndr flag -> Bool
    binderIsMode (KindedTV _ _ kind) = kind == ConT ''EvaluationMode
    binderIsMode _ = False

    genBndrConstraint :: TyVarBndr flag -> Q [Type]
    genBndrConstraint (PlainTV n _) = do
      let tv = return $ VarT n
      reportWarning $ "Cannot infer the kind for " ++ show n ++ " assuming *"
      singleton <$> [t|($(return $ ConT cls) $tv, Mergeable $tv)|]
    genBndrConstraint (KindedTV n _ StarT) = do
      let tv = return $ VarT n
      sequence [[t|$(return $ ConT cls) $tv|], [t|Mergeable $tv|]]
    genBndrConstraint (KindedTV n _ kind@(ConT nm)) = do
      if
        | nm == ''EvaluationMode && isNothing evmode ->
            singleton <$> [t|(IsMode $(return $ VarT n))|]
        | nm == ''EvaluationMode -> return []
        | nm == ''Nat ->
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

deriveInstances :: Strategy -> Name -> [Name] -> Q [Dec]
deriveInstances strategy name =
  fmap concat <$> traverse (deriveWithMode Nothing strategy name)

deriveInstancesWithMode ::
  Strategy -> EvaluationMode -> Name -> [Name] -> Q [Dec]
deriveInstancesWithMode strategy mode name =
  fmap concat <$> traverse (deriveWithMode (Just mode) strategy name)

deriveNewtype :: Name -> [Name] -> Q [Dec]
deriveNewtype = deriveInstances Newtype

deriveAnyclass :: Name -> [Name] -> Q [Dec]
deriveAnyclass = deriveInstances Anyclass

deriveStock :: Name -> [Name] -> Q [Dec]
deriveStock = deriveInstances Stock

deriveViaDefault :: Name -> [Name] -> Q [Dec]
deriveViaDefault = deriveInstances Via

deriveNewtypeWithMode :: EvaluationMode -> Name -> [Name] -> Q [Dec]
deriveNewtypeWithMode = deriveInstancesWithMode Newtype

deriveAnyclassWithMode :: EvaluationMode -> Name -> [Name] -> Q [Dec]
deriveAnyclassWithMode = deriveInstancesWithMode Anyclass

deriveStockWithMode :: EvaluationMode -> Name -> [Name] -> Q [Dec]
deriveStockWithMode = deriveInstancesWithMode Stock

deriveViaDefaultWithMode :: EvaluationMode -> Name -> [Name] -> Q [Dec]
deriveViaDefaultWithMode = deriveInstancesWithMode Via
