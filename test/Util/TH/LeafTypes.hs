module Util.TH.LeafTypes where

import Prelude
import Language.Haskell.TH.Syntax


fromDec =
  \ case
    NewtypeD a _ b c d _ ->
      fromCxt a <>
      concatMap fromTyVarBndr b <>
      foldMap fromType c <>
      fromCon d

fromTyVarBndr =
  \ case
    KindedTV _ a ->
      fromType a
    _ ->
      []

fromCxt =
  concatMap fromType

fromCon =
  \ case
    NormalC _ bangTypes -> concatMap fromBangType bangTypes

fromBangType (_, t) =
  fromType t

fromType =
  \ case
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
