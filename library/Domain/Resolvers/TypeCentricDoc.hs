module Domain.Resolvers.TypeCentricDoc where

import qualified Data.Text as Text
import qualified Domain.Models.TypeCentricDoc as Doc
import qualified Domain.Models.TypeString as TypeString
import Domain.Prelude hiding (lookup)
import qualified Domain.Text as Text
import DomainCore.Model

eliminateDoc :: (Applicative f) => Doc.Doc -> f [TypeDec]
eliminateDoc =
  traverse (uncurry (structureTypeDecs [])) >>> fmap join

structureTypeDecs :: (Applicative f) => [Text] -> Text -> Doc.Structure -> f [TypeDec]
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

structureGeneratedTypeDecs :: (Applicative f) => [Text] -> Doc.Structure -> f [TypeDec]
structureGeneratedTypeDecs namespace =
  \case
    Doc.ProductStructure structure ->
      traverse (uncurry (nestedTypeExpressionTypeDecs namespace . Text.ucFirst)) structure
        & fmap join
    Doc.SumStructure structure ->
      traverse (\(a, b) -> traverse (nestedTypeExpressionTypeDecs namespace (Text.ucFirst a)) b) structure
        & fmap (join . join)
    _ ->
      pure []

nestedTypeExpressionTypeDecs :: (Applicative f) => [Text] -> Text -> Doc.NestedTypeExpression -> f [TypeDec]
nestedTypeExpressionTypeDecs namespace name =
  \case
    Doc.StructureNestedTypeExpression a ->
      structureTypeDecs namespace name a
    _ ->
      pure []

structureTypeDef :: (Applicative f) => [Text] -> Doc.Structure -> f TypeDef
structureTypeDef namespace =
  \case
    Doc.ProductStructure structure ->
      ProductTypeDef <$> traverse (uncurry (eliminateProductStructureUnit namespace)) structure
    Doc.SumStructure structure ->
      SumTypeDef <$> traverse (uncurry (eliminateSumStructureUnit namespace)) structure
    Doc.EnumStructure variants ->
      pure (SumTypeDef (fmap (,[]) variants))

eliminateProductStructureUnit :: (Applicative f) => [Text] -> Text -> Doc.NestedTypeExpression -> f (Text, Type)
eliminateProductStructureUnit namespace name productTypeExpression =
  (,) name <$> nestedTypeExpressionType namespace name productTypeExpression

eliminateSumStructureUnit :: (Applicative f) => [Text] -> Text -> [Doc.NestedTypeExpression] -> f (Text, [Type])
eliminateSumStructureUnit namespace name sumTypeExpression =
  (,) name <$> traverse (nestedTypeExpressionType namespace name) sumTypeExpression

nestedTypeExpressionType :: (Applicative f) => [Text] -> Text -> Doc.NestedTypeExpression -> f Type
nestedTypeExpressionType namespace name =
  \case
    Doc.AppSeqNestedTypeExpression a ->
      AppType <$> eliminateTypeStringAppSeq a
    Doc.StructureNestedTypeExpression _ ->
      pure (RefType (Text.concat (reverse (Text.ucFirst name : namespace))))

eliminateTypeStringCommaSeq :: (Traversable t, Applicative f) => t (NonEmpty TypeString.Unit) -> f (t (NonEmpty Type))
eliminateTypeStringCommaSeq =
  traverse eliminateTypeStringAppSeq

eliminateTypeStringAppSeq :: (Applicative f) => NonEmpty TypeString.Unit -> f (NonEmpty Type)
eliminateTypeStringAppSeq =
  traverse eliminateTypeStringUnit

eliminateTypeStringUnit :: (Applicative f) => TypeString.Unit -> f Type
eliminateTypeStringUnit =
  \case
    TypeString.InSquareBracketsUnit appSeq ->
      eliminateTypeStringAppSeq appSeq
        & fmap (ListType . AppType)
    TypeString.InParensUnit commaSeq ->
      eliminateTypeStringCommaSeq commaSeq
        & fmap (tupleIfNotOne . fmap AppType)
      where
        tupleIfNotOne =
          \case
            [a] -> a
            a -> TupleType a
    TypeString.RefUnit typeRef ->
      eliminateTypeRef typeRef
        & fmap RefType

eliminateTypeRef :: (Applicative f) => NonEmpty Text -> f Text
eliminateTypeRef =
  pure . Text.intercalate "." . toList
