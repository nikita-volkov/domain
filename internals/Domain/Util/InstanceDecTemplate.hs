module Domain.Util.InstanceDecTemplate
where

import Domain.Prelude
import Language.Haskell.TH
import Domain.Util.TH
import qualified TemplateHaskell.Compat.V0208 as Compat
import qualified Data.Text as Text


sumConstructorIsLabel :: Name -> Name -> TyLit -> [Type] -> Dec
sumConstructorIsLabel typeName conName label memberTypes =
  InstanceD Nothing [] headType [fromLabelDec]
  where
    headType =
      multiAppT (ConT ''IsLabel) [LitT label, repType]
      where
        repType =
          foldr (\ a b -> AppT (AppT ArrowT a) b) (ConT typeName) memberTypes
    fromLabelDec =
      FunD 'fromLabel [Clause [] body []]
      where
        body =
          NormalB (ConE conName)

enumConstructorIsLabel :: Name -> Name -> TyLit -> Dec
enumConstructorIsLabel typeName conName label =
  InstanceD Nothing [] headType [fromLabelDec]
  where
    headType =
      AppT (AppT (ConT ''IsLabel) (LitT label))
        (ConT typeName)
    fromLabelDec =
      FunD 'fromLabel [Clause [] body []]
      where
        body =
          NormalB (ConE conName)

productAccessorIsLabel :: Name -> TyLit -> Exp -> Type -> Dec
productAccessorIsLabel typeName label accessor resultType =
  InstanceD Nothing [] headType [fromLabelDec]
  where
    headType =
      multiAppT (ConT ''IsLabel) [LitT label, repType]
      where
        repType =
          multiAppT ArrowT [ConT typeName, resultType]
    fromLabelDec =
      FunD 'fromLabel [Clause [] (NormalB accessor) []]


-- * 'HasField'
-------------------------

{-| The most general template for 'HasField'. -}
hasField :: TyLit -> Type -> Type -> [Clause] -> Dec
hasField fieldLabel ownerType projectionType getFieldFunClauses =
  InstanceD Nothing [] headType [getFieldDec]
  where
    headType =
      multiAppT (ConT ''HasField) [LitT fieldLabel, ownerType, projectionType]
    getFieldDec =
      FunD 'getField getFieldFunClauses

{-|
Field which projects enum values into bools.
-}
boolEnumHasField :: TyLit -> Type -> Name -> Dec
boolEnumHasField fieldLabel ownerType constructorName =
  hasField fieldLabel ownerType projectionType getFieldFunClauses
  where
    projectionType =
      ConT ''Bool
    getFieldFunClauses =
      [matching, unmatching]
      where
        matching =
          Clause [ConP constructorName []] (NormalB bodyExp) []
          where
            bodyExp =
              ConE 'True
        unmatching =
          Clause [WildP] (NormalB bodyExp) []
          where
            bodyExp =
              ConE 'False

sumHasField :: TyLit -> Type -> Name -> [Type] -> Dec
sumHasField fieldLabel ownerType constructorName memberTypes =
  hasField fieldLabel ownerType projectionType getFieldFunClauses
  where
    projectionType =
      AppT (ConT ''Maybe) (appliedTupleOrSingletonT memberTypes)
    getFieldFunClauses =
      [matching, unmatching]
      where
        varNames =
          enumFromTo 1 (length memberTypes) &
          fmap (mkName . showChar '_' . show)
        matching =
          Clause [ConP constructorName pats] (NormalB bodyExp) []
          where
            pats =
              fmap VarP varNames
            bodyExp =
              AppE (ConE 'Just) (appliedTupleE (fmap VarE varNames))
        unmatching =
          Clause [WildP] (NormalB bodyExp) []
          where
            bodyExp =
              ConE 'Nothing

productHasField :: TyLit -> Type -> Type -> Name -> Int -> Int -> Dec
productHasField fieldLabel ownerType projectionType constructorName totalMemberTypes offset =
  hasField fieldLabel ownerType projectionType getFieldFunClauses
  where
    getFieldFunClauses =
      [Clause [ConP constructorName pats] (NormalB bodyExp) []]
      where
        pats =
          replicate offset WildP <>
          bool empty [VarP aName] (totalMemberTypes > 0) <>
          replicate (totalMemberTypes - offset - 1) WildP
        bodyExp =
          VarE aName
