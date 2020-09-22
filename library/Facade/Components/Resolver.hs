module Facade.Components.Resolver
where

import Facade.Prelude hiding (lookup)
import Facade.Model
import qualified Facade.V1DocModel as Doc
import qualified Facade.Util.List as List
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text


type Env =
  HashMap Text TypeRef

type Err =
  Text

type Eff =
  ExceptT Err ((->) Env)

resolve :: Text -> Eff TypeRef
resolve a =
  ExceptT $ \ b ->
    case HashMap.lookup a b of
      Just c ->
        Right c
      Nothing ->
        Left ("No definition found for name: " <> a)

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
    [] ->
      return (TupleType 0)
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
          resolve c
        _ ->
          return (GlobalTypeRef b c)
    Nothing ->
      throwE "Broken type ref"

byTypeName :: Doc.ByTypeName a -> (Text -> a -> Eff b) -> Eff [b]
byTypeName (Doc.ByTypeName a) b =
  traverse (\ (c, d) -> b c d) a

byTypeNameTypeDec :: Doc.ByTypeName a -> (a -> Eff TypeDef) -> Eff [Dec]
byTypeNameTypeDec (Doc.ByTypeName a) b =
  traverse (\ (c, d) -> TypeDec c <$> b d) a

aliasDef :: Doc.AliasDef -> Eff TypeDef
aliasDef (Doc.AliasDef a) =
  AliasTypeDef <$> type_ a

wrapperDef :: Doc.WrapperDef -> Eff TypeDef
wrapperDef (Doc.WrapperDef a) =
  WrapperTypeDef <$> type_ a

enumDef (Doc.EnumDef a) =
  pure (EnumTypeDef a)

productDef (Doc.ProductDef a) =
  a & typeByFieldName & fmap (CompositeTypeDef ProductComposition)

sumDef (Doc.SumDef a) =
  a & typeByFieldName & fmap (CompositeTypeDef SumComposition)

typeByFieldName (Doc.TypeByFieldName a) =
  a & traverse (traverse type_)

doc :: Doc.Doc -> Eff [Dec]
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
