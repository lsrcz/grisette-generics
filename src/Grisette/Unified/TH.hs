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
    deriveConversions,
    deriveConversionsWithMode,
  )
where

import Control.Monad (when, zipWithM)
import Data.List (singleton)
import Data.Maybe (isJust, isNothing)
import GHC.TypeNats (KnownNat, Nat, type (<=))
import Grisette (Default, Mergeable)
import Grisette.Unified.EvaluationMode (EvaluationMode (Con, Sym))
import Grisette.Unified.IsMode (IsMode)
import Language.Haskell.TH (Ppr (ppr), Quote (newName), Type (PromotedT))
import Language.Haskell.TH.Syntax
  ( Dec (DataD, NewtypeD, StandaloneDerivD),
    DerivStrategy
      ( AnyclassStrategy,
        NewtypeStrategy,
        StockStrategy,
        ViaStrategy
      ),
    Info (TyConI),
    Kind,
    Name,
    Q,
    TyVarBndr (KindedTV, PlainTV),
    Type (AppT, ConT, StarT, VarT),
    reify,
    reportWarning,
  )

bndrFresh :: TyVarBndr flag -> Q (TyVarBndr flag)
bndrFresh (PlainTV n f) = flip PlainTV f <$> newName (show n)
bndrFresh (KindedTV n f k) = (\nm -> KindedTV nm f k) <$> newName (show n)

bndrName :: TyVarBndr flag -> Name
bndrName (PlainTV n _) = n
bndrName (KindedTV n _ _) = n

bndrKind :: TyVarBndr flag -> Q Kind
bndrKind (PlainTV n _) = do
  reportWarning $ "Cannot infer the kind for " ++ show n ++ " assuming *"
  return StarT
bndrKind (KindedTV _ _ kind) = return kind

binderIsMode :: TyVarBndr flag -> Bool
binderIsMode (KindedTV _ _ kind) = kind == ConT ''EvaluationMode
binderIsMode _ = False

dataTypeWithTVars :: Maybe EvaluationMode -> Name -> [TyVarBndr flag] -> Q Type
dataTypeWithTVars evmode name = dataTypeWithTVars' evmode name . reverse
  where
    dataTypeWithTVars' _ name [] = return $ ConT name
    dataTypeWithTVars' evmode name (bndr : bndrs)
      | binderIsMode bndr && isJust evmode =
          case evmode of
            Just Con ->
              [t|
                $(dataTypeWithTVars' evmode name bndrs)
                  $(return $ PromotedT 'Con)
                |]
            Just Sym ->
              [t|
                $(dataTypeWithTVars' evmode name bndrs)
                  $(return $ PromotedT 'Sym)
                |]
            Nothing -> fail "EvaluationMode is not provided"
    dataTypeWithTVars' evmode name (bndr : bndrs) =
      [t|
        $(dataTypeWithTVars' evmode name bndrs)
          $(return $ VarT (bndrName bndr))
        |]

stripBinderInfo :: TyVarBndr flag -> TyVarBndr ()
stripBinderInfo (PlainTV n _) = PlainTV n ()
stripBinderInfo (KindedTV n _ kind) = KindedTV n () kind

getBinders :: Info -> Q [TyVarBndr ()]
getBinders (TyConI (DataD _ _ bndrs _ _ _)) = traverse bndrFresh bndrs
getBinders (TyConI (NewtypeD _ _ bndrs _ _ _)) =
  traverse (bndrFresh . stripBinderInfo) bndrs
getBinders _ =
  fail "Currently only non-GADTs data or newtype are supported."

data Strategy = Stock | Newtype | Via | Anyclass

deriveWithMode :: Maybe EvaluationMode -> Strategy -> Name -> Name -> Q [Dec]
deriveWithMode evmode strategy name cls = do
  d <- reify name
  bndrs <- getBinders d
  bndrConstraints <- concat <$> traverse genBndrConstraint bndrs
  deriveStrategy <- getStrategy bndrs
  dataType <- dataTypeWithTVars evmode name bndrs
  return
    [ StandaloneDerivD
        (Just deriveStrategy)
        bndrConstraints
        (AppT (ConT cls) dataType)
    ]
  where
    getStrategy bndrs =
      case strategy of
        Stock -> return StockStrategy
        Newtype -> return NewtypeStrategy
        Via ->
          ViaStrategy
            <$> [t|Default $(dataTypeWithTVars evmode name bndrs)|]
        Anyclass -> return AnyclassStrategy
    genBndrConstraint :: TyVarBndr flag -> Q [Type]
    genBndrConstraint bndr = do
      let name = bndrName bndr
      let tv = return $ VarT name
      kind <- bndrKind bndr
      case kind of
        StarT -> sequence [[t|$(return $ ConT cls) $tv|], [t|Mergeable $tv|]]
        (ConT nm)
          | nm == ''EvaluationMode && isNothing evmode ->
              singleton <$> [t|IsMode $tv|]
        (ConT nm) | nm == ''EvaluationMode -> return []
        (ConT nm)
          | nm == ''Nat -> sequence [[t|KnownNat $tv|], [t|1 <= $tv|]]
        _ -> fail $ "Unsupported kind in type arguments: " ++ show (ppr kind)

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

deriveConversionWithMode ::
  Maybe EvaluationMode -> Name -> Name -> Name -> Q [Dec]
deriveConversionWithMode evmode from to cls = do
  dfrom <- reify from
  dto <- reify to
  fromBndrs <- getBinders dfrom
  toBndrs <- getBinders dto
  when (length fromBndrs /= length toBndrs) $
    fail "The number of type arguments must be the same."
  bndrConstraints <- concat <$> zipWithM genBndrConstraint fromBndrs toBndrs
  fromDataType <- dataTypeWithTVars evmode from fromBndrs
  toDataType <- dataTypeWithTVars evmode to toBndrs
  return
    [ StandaloneDerivD
        (Just $ ViaStrategy toDataType)
        bndrConstraints
        (AppT (AppT (ConT cls) fromDataType) toDataType)
    ]
  where
    genBndrConstraint :: TyVarBndr () -> TyVarBndr () -> Q [Type]
    genBndrConstraint bndrFrom bndrTo = do
      let nameFrom = bndrName bndrFrom
      let nameTo = bndrName bndrTo
      let tvFrom = return $ VarT nameFrom
      let tvTo = return $ VarT nameTo
      kindFrom <- bndrKind bndrFrom
      kindTo <- bndrKind bndrTo
      when (kindFrom /= kindTo) $
        fail "The kinds of the type arguments must be the aligned."
      case kindFrom of
        StarT ->
          sequence
            [ [t|$(return $ ConT cls) $tvFrom $tvTo|],
              [t|Mergeable $tvFrom|],
              [t|Mergeable $tvTo|]
            ]
        (ConT nm)
          | nm == ''EvaluationMode && isNothing evmode ->
              sequence [[t|(IsMode $tvFrom)|], [t|(IsMode $tvTo)|]]
        (ConT nm) | nm == ''EvaluationMode -> return []
        (ConT nm)
          | nm == ''Nat ->
              sequence
                [ [t|KnownNat $tvFrom|],
                  [t|1 <= $tvFrom|],
                  [t|$tvFrom ~ $tvTo|]
                ]
        _ ->
          fail $
            "Unsupported kind in type arguments: "
              ++ show (ppr kindFrom)

deriveConversions ::
  Name -> Name -> [Name] -> Q [Dec]
deriveConversions from to =
  fmap concat . traverse (deriveConversionWithMode Nothing from to)

deriveConversionsWithMode ::
  EvaluationMode -> Name -> Name -> [Name] -> Q [Dec]
deriveConversionsWithMode mode from to =
  fmap concat . traverse (deriveConversionWithMode (Just mode) from to)
