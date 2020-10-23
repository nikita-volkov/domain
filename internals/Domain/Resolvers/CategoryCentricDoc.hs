module Domain.Resolvers.CategoryCentricDoc
where

import Domain.Prelude hiding (lookup)
import Domain.Model
import qualified Domain.Models.CategoryCentricDoc as Doc
import qualified Domain.Util.List as List
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text


type Err =
  Text

type Eff =
  Either Err

possibleType :: Maybe Doc.Type -> Eff Type
possibleType =
  \ case
    Just a ->
      type_ a
    Nothing ->
      return (TupleType [])

type_ :: Doc.Type -> Eff Type
type_ =
  \ case
    Doc.RefType a ->
      RefType <$> typeRef a
    Doc.AppType a b ->
      AppType <$> ((:|) <$> type_ a <*> appTypeList b)
    Doc.InSquareBracketsType a ->
      ListType <$> type_ a
    Doc.InParensType a ->
      inParensType a
    Doc.SequenceType a ->
      sequenceType a

appTypeList =
  \ case
    Doc.AppType a b ->
      (:) <$> type_ a <*> appTypeList b
    a ->
      pure <$> type_ a

sequenceType :: [Maybe Doc.Type] -> Eff Type
sequenceType list =
  traverse possibleType list &
  fmap (TupleType)

inParensType :: [Doc.Type] -> Eff Type
inParensType =
  \ case
    a : [] ->
      type_ a
    a ->
      TupleType <$> traverse type_ a

typeRef :: Doc.TypeRef -> Eff Text
typeRef (Doc.TypeRef a) =
  return (Text.intercalate "." a)

byTypeName :: Doc.ByTypeName a -> (Text -> a -> Eff b) -> Eff [b]
byTypeName (Doc.ByTypeName a) b =
  traverse (\ (c, d) -> b c d) a

byTypeNameTypeDec :: Doc.ByTypeName a -> (a -> Eff TypeDef) -> Eff [TypeDec]
byTypeNameTypeDec (Doc.ByTypeName a) b =
  traverse (\ (c, d) -> TypeDec c <$> b d) a

aliasDef :: Doc.AliasDef -> Eff TypeDef
aliasDef (Doc.AliasDef a) =
  AliasTypeDef <$> possibleType a

wrapperDef :: Doc.WrapperDef -> Eff TypeDef
wrapperDef (Doc.WrapperDef a) =
  WrapperTypeDef <$> possibleType a

enumDef :: Doc.EnumDef -> Eff TypeDef
enumDef (Doc.EnumDef a) =
  pure (EnumTypeDef a)

productDef :: Doc.ProductDef -> Eff TypeDef
productDef (Doc.ProductDef (Doc.TypeByFieldName a)) =
  a & traverse (traverse possibleType) & fmap ProductTypeDef

sumDef :: Doc.SumDef -> Eff TypeDef
sumDef (Doc.SumDef (Doc.TypeByFieldName a)) =
  a & traverse (traverse sumConstructorType) & fmap SumTypeDef

sumConstructorType :: Maybe Doc.Type -> Eff [Type]
sumConstructorType =
  \ case
    Just a -> case a of
      Doc.InParensType a ->
        traverse type_ a
      Doc.SequenceType a ->
        traverse possibleType a
      a ->
        fmap pure (type_ a)
    Nothing ->
      return []

doc :: Doc.Doc -> Eff [TypeDec]
doc (Doc.Doc a b c d e f) =
  fmap concat $ sequence [
    byTypeNameTypeDec b aliasDef
    ,
    byTypeNameTypeDec c wrapperDef
    ,
    byTypeNameTypeDec d enumDef
    ,
    byTypeNameTypeDec e productDef
    ,
    byTypeNameTypeDec f sumDef
    ]
