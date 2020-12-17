module Domain.Resolvers.TypeCentricDoc
where

import Domain.Prelude hiding (lookup)
import DomainCore.Model
import qualified Domain.Models.TypeCentricDoc as Doc
import qualified Domain.Models.TypeString as TypeString
import qualified Data.Text as Text


eliminateDoc :: Applicative f => Doc.Doc -> f [TypeDec]
eliminateDoc =
  traverse (uncurry (structureTypeDecs [])) >>> fmap join

structureTypeDecs :: Applicative f => [Text] -> Text -> Doc.Structure -> f [TypeDec]
structureTypeDecs namespace name structure =
  (:) <$> primary <*> structureGeneratedTypeDecs nextNamespace structure
  where
    primary =
      TypeDec renderedName <$> structureTypeDef nextNamespace structure
      where
        renderedName =
          Text.concat (reverse nextNamespace)
    nextNamespace =
      name : namespace

structureGeneratedTypeDecs :: Applicative f => [Text] -> Doc.Structure -> f [TypeDec]
structureGeneratedTypeDecs namespace =
  \ case
    Doc.ProductStructure structure ->
      traverse (uncurry (nestedTypeExpressionTypeDecs namespace . Text.toTitle)) structure
        & fmap join
    Doc.SumStructure structure ->
      traverse (\(a, b) -> traverse (nestedTypeExpressionTypeDecs namespace (Text.toTitle a)) b) structure
        & fmap (join . join)
    _ ->
      pure []

nestedTypeExpressionTypeDecs namespace name =
  \ case
    Doc.StructureNestedTypeExpression a ->
      structureTypeDecs namespace name a
    _ ->
      pure []

structureTypeDef :: Applicative f => [Text] -> Doc.Structure -> f TypeDef
structureTypeDef namespace =
  \ case
    Doc.ProductStructure structure ->
      ProductTypeDef <$> traverse (uncurry (eliminateProductStructureUnit namespace)) structure
    Doc.SumStructure structure ->
      SumTypeDef <$> traverse (uncurry (eliminateSumStructureUnit namespace)) structure
    Doc.EnumStructure variants ->
      pure (SumTypeDef (fmap (,[]) variants))

eliminateProductStructureUnit :: Applicative f => [Text] -> Text -> Doc.NestedTypeExpression -> f (Text, Type)
eliminateProductStructureUnit namespace name productTypeExpression =
  (,) name <$> nestedTypeExpressionType namespace name productTypeExpression

eliminateSumStructureUnit :: Applicative f => [Text] -> Text -> [Doc.NestedTypeExpression] -> f (Text, [Type])
eliminateSumStructureUnit namespace name sumTypeExpression =
  (,) name <$> traverse (nestedTypeExpressionType namespace name) sumTypeExpression

nestedTypeExpressionType :: Applicative f => [Text] -> Text -> Doc.NestedTypeExpression -> f Type
nestedTypeExpressionType namespace name =
  \ case
    Doc.AppSeqNestedTypeExpression a ->
      AppType <$> eliminateTypeStringAppSeq a
    Doc.StructureNestedTypeExpression _ ->
      pure (RefType (Text.concat (reverse (Text.toTitle name : namespace))))

eliminateTypeStringCommaSeq =
  traverse eliminateTypeStringAppSeq

eliminateTypeStringAppSeq :: Applicative f => NonEmpty TypeString.Unit -> f (NonEmpty Type)
eliminateTypeStringAppSeq =
  traverse eliminateTypeStringUnit

eliminateTypeStringUnit =
  \ case
    TypeString.InSquareBracketsUnit appSeq ->
      eliminateTypeStringAppSeq appSeq &
        fmap (ListType . AppType)
    TypeString.InParensUnit commaSeq ->
      eliminateTypeStringCommaSeq commaSeq &
        fmap (tupleIfNotOne . fmap AppType)
      where
        tupleIfNotOne =
          \ case
            [a] -> a
            a -> TupleType a
    TypeString.RefUnit typeRef ->
      eliminateTypeRef typeRef &
        fmap RefType

eliminateTypeRef =
  pure . Text.intercalate "." . toList
