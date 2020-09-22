module Facade.TH
where

import Facade.Prelude
import Facade.Model
import qualified Language.Haskell.TH as TH
import qualified Data.Text as Text
import qualified Data.Char as Char


typeDec =
  \ case
    TypeDec a b ->
      typeDefDec a b

typeDefDec a =
  \ case
    AliasTypeDef b ->
      typeSynonymDec a b
    WrapperTypeDef b ->
      newtypeDec a b
    EnumTypeDef b ->
      enumDec a b
    CompositeTypeDef b c ->
      case b of
        ProductComposition ->
          recordAdtDec a c
        SumComposition ->
          sumAdtDec a c

typeSynonymDec a b =
  TH.TySynD (textName a) [] (typeType b)

newtypeDec a b =
  TH.NewtypeD [] _name [] Nothing _con []
  where
    _name =
      textName a
    _accessorName =
      recordFieldName a "Value"
    _con =
      TH.RecC _name [(_accessorName, noBang, typeType b)]

textName =
  TH.mkName . Text.unpack

noBang =
  TH.Bang TH.NoSourceUnpackedness TH.NoSourceStrictness

fieldBang =
  TH.Bang TH.NoSourceUnpackedness TH.SourceStrict

typeType =
  \ case
    AppType a b ->
      TH.AppT (typeType a) (typeType b)
    RefType a ->
      typeRefType a
    ListType ->
      TH.ListT
    TupleType a ->
      TH.TupleT a

typeRefNameText =
  \ case
    LocalTypeRef a -> a
    GlobalTypeRef a b -> Text.intercalate "." a <> "." <> b

typeRefType =
  TH.ConT . textName . typeRefNameText

recordAdtDec a b =
  TH.DataD [] _name [] Nothing [_con] []
  where
    _name =
      textName a
    _con =
      TH.RecC _name (fmap (uncurry (recordVarBangType a)) b)

recordVarBangType _conName _fieldName _type =
  (recordFieldName _conName _fieldName, fieldBang, typeType _type)

recordFieldName a b =
  textName (onTextFirstChar Char.toLower a <> onTextFirstChar Char.toUpper b)

onTextFirstChar fn =
  foldMap (\ (a, b) -> Text.cons (fn a) b) .
  Text.uncons

sumAdtDec a b =
  TH.DataD [] (textName a) [] Nothing (fmap (uncurry (sumConstructor a)) b) []

sumConstructor a b c =
  TH.NormalC
    (sumConstructorName a b)
    [(fieldBang, typeType c)]

sumConstructorName a b =
  textName (onTextFirstChar Char.toUpper b <> a)

enumDec a b =
  TH.DataD [] (textName a) [] Nothing (fmap (enumConstructor a) b) []

enumConstructor a b =
  TH.NormalC (sumConstructorName a b) []
