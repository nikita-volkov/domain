module Util.TH.LeafTypes where

import Language.Haskell.TH.Syntax
import TemplateHaskell.Compat.V0208
import Prelude

fromDec :: Dec -> [Kind]
fromDec =
  \case
    NewtypeD a _ b c d _ ->
      fromCxt a
        <> concatMap fromTyVarBndr b
        <> foldMap fromType c
        <> fromCon d
    _ -> error "TODO"

fromTyVarBndr :: TyVarBndr flag -> [Kind]
fromTyVarBndr =
  maybeToList . tyVarBndrKind

fromCxt :: Cxt -> [Kind]
fromCxt =
  concatMap fromType

fromCon :: Con -> [Kind]
fromCon =
  \case
    NormalC _ bangTypes -> concatMap fromBangType bangTypes
    _ -> error "TODO"

fromBangType :: (a, Type) -> [Kind]
fromBangType (_, t) =
  fromType t

fromType :: Type -> [Kind]
fromType =
  \case
    ForallT a b c ->
      concatMap fromTyVarBndr a <> fromCxt b <> fromType c
    ForallVisT a b ->
      concatMap fromTyVarBndr a <> fromType b
    AppT l r ->
      fromType l <> fromType r
    AppKindT a _ ->
      fromType a
    SigT a _ ->
      fromType a
    InfixT a _ b ->
      fromType a <> fromType b
    UInfixT a _ b ->
      fromType a <> fromType b
    ParensT a ->
      fromType a
    ImplicitParamT _ a ->
      fromType a
    t ->
      [t]
