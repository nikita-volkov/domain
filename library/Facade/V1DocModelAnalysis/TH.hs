{-|
Functions are named after what they produce.
-}
module Facade.V1DocModelAnalysis.TH
where

import Facade.Prelude hiding (Product, Sum, Enum)
import Facade.V1DocModel
import qualified Language.Haskell.TH as TH
import qualified Data.Text as Text
import qualified Data.Char as Char


byTypeNameDecs :: (Text -> a -> TH.Dec) -> ByTypeName a -> [TH.Dec]
byTypeNameDecs toDec (ByTypeName a) =
  fmap (uncurry toDec) a

decs (Doc a b c d e f) =
  byTypeNameDecs typeSynonymDec b <>
  byTypeNameDecs newtypeDec c <>
  byTypeNameDecs enumDec d <>
  byTypeNameDecs recordAdtDec e <>
  byTypeNameDecs sumAdtDec f

typeType =
  \ case
    AppType a b ->
      TH.AppT (typeType a) (typeType b)
    RefType a ->
      typeRefType a
    InParensType a ->
      error "TODO"
    InSquareBracketsType a ->
      TH.AppT TH.ListT (typeType a)

typeRefType =
  TH.ConT . TH.mkName . Text.unpack . Text.intercalate "." . coerce

newtypeDec a (WrapperDef b) =
  TH.NewtypeD [] _name [] Nothing _con []
  where
    _name =
      textName a
    _accessorName =
      textName (a <> "Value")
    _con =
      TH.RecC _name [(_accessorName, noBang, typeType b)]

textName =
  TH.mkName . Text.unpack

noBang =
  TH.Bang TH.NoSourceUnpackedness TH.NoSourceStrictness

fieldBang =
  TH.Bang TH.NoSourceUnpackedness TH.SourceStrict

recordAdtDec a (ProductDef (TypeByFieldName b)) =
  TH.DataD [] _name [] Nothing [_con] []
  where
    _name =
      textName a
    _con =
      TH.RecC _name (fmap (uncurry (recordVarBangType a)) b)

recordVarBangType _conName _fieldName _type =
  (textName (recordFieldNameText _conName _fieldName), fieldBang, typeType _type)

recordFieldNameText a b =
  detitledText a <> Text.toTitle b

detitledText =
  foldMap (\ (a, b) -> Text.cons (Char.toLower a) b) .
  Text.uncons

sumAdtDec a (SumDef (TypeByFieldName b)) =
  TH.DataD [] (textName a) [] Nothing (fmap (uncurry (sumConstructor a)) b) []

sumConstructor a b c =
  TH.NormalC
    (sumConstructorName a b)
    [(fieldBang, typeType c)]

sumConstructorName a b =
  textName (a <> Text.toTitle b)

enumDec a (EnumDef b) =
  TH.DataD [] (textName a) [] Nothing (fmap (enumConstructor a) b) []

enumConstructor a b =
  TH.NormalC (sumConstructorName a b) []

typeSynonymDec a (AliasDef b) =
  TH.TySynD (textName a) [] (typeType b)
