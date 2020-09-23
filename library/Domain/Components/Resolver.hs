module Domain.Components.Resolver
where

import Domain.Prelude hiding (lookup)
import Domain.Model
import qualified Domain.V1DocModel as Doc
import qualified Domain.Util.List as List
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text


type Env =
  HashMap Text TypeRef

type Err =
  Text

type Eff =
  Either Err

type_ :: Doc.Type -> Eff Type
type_ =
  \ case
    Doc.RefType a ->
      RefType <$> typeRef a
    Doc.AppType a b ->
      AppType <$> type_ a <*> type_ b
    Doc.InSquareBracketsType a ->
      AppType ListType <$> type_ a
    Doc.InParensType a ->
      inParensType a

inParensType :: [Doc.Type] -> Eff Type
inParensType =
  \ case
    a : [] ->
      type_ a
    a ->
      fmap (foldl' AppType (TupleType (length a))) (traverse type_ a)

typeRef :: Doc.TypeRef -> Eff TypeRef
typeRef (Doc.TypeRef a) =
  case List.unsnoc a of
    Just (b, c) ->
      case b of
        [] ->
          return (LocalTypeRef c)
        _ ->
          return (GlobalTypeRef b c)
    Nothing ->
      throwError "Broken type ref"

byTypeName :: Doc.ByTypeName a -> (Text -> a -> Eff b) -> Eff [b]
byTypeName (Doc.ByTypeName a) b =
  traverse (\ (c, d) -> b c d) a

byTypeNameTypeDec :: Doc.ByTypeName a -> (a -> Eff TypeDef) -> Eff [TypeDec]
byTypeNameTypeDec (Doc.ByTypeName a) b =
  traverse (\ (c, d) -> TypeDec c <$> b d) a

aliasDef :: Doc.AliasDef -> Eff TypeDef
aliasDef (Doc.AliasDef a) =
  AliasTypeDef <$> type_ a

wrapperDef :: Doc.WrapperDef -> Eff TypeDef
wrapperDef (Doc.WrapperDef a) =
  WrapperTypeDef <$> type_ a

enumDef :: Doc.EnumDef -> Eff TypeDef
enumDef (Doc.EnumDef a) =
  pure (EnumTypeDef a)

productDef :: Doc.ProductDef -> Eff TypeDef
productDef (Doc.ProductDef (Doc.TypeByFieldName a)) =
  a & traverse (traverse type_) & fmap ProductTypeDef

sumDef :: Doc.SumDef -> Eff TypeDef
sumDef (Doc.SumDef (Doc.TypeByFieldName a)) =
  a & traverse (traverse sumConstructorType) & fmap SumTypeDef

sumConstructorType :: Doc.Type -> Eff [Type]
sumConstructorType =
  \ case
    Doc.InParensType a ->
      traverse type_ a
    a ->
      fmap pure (type_ a)

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
