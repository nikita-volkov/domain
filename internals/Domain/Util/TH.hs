{-|
Model-unaware helpers.
-}
module Domain.Util.TH
where

import Domain.Prelude
import Language.Haskell.TH
import qualified TemplateHaskell.Compat.V0208 as Compat
import qualified Data.Text as Text


-- * Decs
-------------------------

typeSynonymDec :: Name -> Type -> Dec
typeSynonymDec a b =
  TySynD a [] b

recordNewtypeDec :: Name -> Name -> Type -> Dec
recordNewtypeDec _name _accessorName _type =
  NewtypeD [] _name [] Nothing _con []
  where
    _con =
      RecC _name [(_accessorName, noBang, _type)]

normalNewtypeDec :: Name -> Type -> Dec
normalNewtypeDec _name _type =
  NewtypeD [] _name [] Nothing _con []
  where
    _con =
      NormalC _name [(noBang, _type)]

recordAdtDec :: Name -> [(Name, Type)] -> Dec
recordAdtDec typeName fields =
  DataD [] typeName [] Nothing [con] []
  where
    con =
      RecC typeName (fmap (\ (fieldName, fieldType) -> (fieldName, fieldBang, fieldType)) fields)

productAdtDec :: Name -> [Type] -> Dec
productAdtDec typeName memberTypes =
  DataD [] typeName [] Nothing [con] []
  where
    con =
      NormalC typeName (fmap ((fieldBang,)) memberTypes)

sumAdtDec :: Name -> [(Name, [Type])] -> Dec
sumAdtDec a b =
  DataD [] a [] Nothing (fmap (uncurry sumCon) b) []

sumCon :: Name -> [Type] -> Con
sumCon a b =
  NormalC a (fmap (fieldBang,) b)

enumDec :: Name -> [Name] -> Dec
enumDec a b =
  DataD [] a [] Nothing (fmap (\ c -> NormalC c []) b) []


-- *
-------------------------

textName :: Text -> Name
textName =
  mkName . Text.unpack

textTyLit :: Text -> TyLit
textTyLit =
  StrTyLit . Text.unpack

noBang :: Bang
noBang =
  Bang NoSourceUnpackedness NoSourceStrictness

fieldBang :: Bang
fieldBang =
  Bang NoSourceUnpackedness SourceStrict

multiAppT :: Type -> [Type] -> Type
multiAppT base args =
  foldl' AppT base args

multiAppE :: Exp -> [Exp] -> Exp
multiAppE base args =
  foldl' AppE base args

appliedTupleT :: [Type] -> Type
appliedTupleT a =
  foldl' AppT (TupleT (length a)) a

appliedTupleOrSingletonT :: [Type] -> Type
appliedTupleOrSingletonT =
  \ case
    [a] -> a
    a -> appliedTupleT a

appliedTupleE :: [Exp] -> Exp
appliedTupleE =
  Compat.tupE

appliedTupleOrSingletonE :: [Exp] -> Exp
appliedTupleOrSingletonE =
  \ case
    [a] -> a
    a -> appliedTupleE a

indexName :: Int -> Name
indexName =
  mkName . showChar '_' . show

enumNames :: Int -> [Name]
enumNames =
  fmap indexName . enumFromTo 0 . pred

aName :: Name
aName =
  mkName "a"

bName :: Name
bName =
  mkName "b"

cName :: Name
cName =
  mkName "c"

eqConstraintT :: Name -> Type -> Type
eqConstraintT name =
  AppT (AppT EqualityT (VarT name))

{-|
Lambda expression, which extracts a product member by index.
-}
productAccessor :: Name -> Int -> Int -> Exp
productAccessor conName numMembers index =
  LamE [pat] exp
  where
    varName =
      indexName index
    pat =
      ConP conName pats
      where
        pats =
          replicate index WildP <>
          pure (VarP varName) <>
          replicate (numMembers - index - 1) WildP
    exp =
      VarE varName


-- * Instance templates
-- | Standard instance templates.
-------------------------

sumConstructorIsLabelInstanceDec :: Name -> Name -> TyLit -> [Type] -> Dec
sumConstructorIsLabelInstanceDec typeName conName label memberTypes =
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

enumConstructorIsLabelInstanceDec :: Name -> Name -> TyLit -> Dec
enumConstructorIsLabelInstanceDec typeName conName label =
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

productAccessorIsLabelInstanceDec :: Name -> TyLit -> Exp -> Type -> Dec
productAccessorIsLabelInstanceDec typeName label accessor resultType =
  InstanceD Nothing [] headType [fromLabelDec]
  where
    headType =
      multiAppT (ConT ''IsLabel) [LitT label, repType]
      where
        repType =
          multiAppT ArrowT [ConT typeName, resultType]
    fromLabelDec =
      FunD 'fromLabel [Clause [] (NormalB accessor) []]

-- ** 'HasField'
-------------------------

{-| The most general template for 'HasField'. -}
hasFieldInstanceDec :: TyLit -> Type -> Type -> [Clause] -> Dec
hasFieldInstanceDec fieldLabel ownerType projectionType getFieldFunClauses =
  InstanceD Nothing [] headType [getFieldDec]
  where
    headType =
      multiAppT (ConT ''HasField) [LitT fieldLabel, ownerType, projectionType]
    getFieldDec =
      FunD 'getField getFieldFunClauses

{-|
Field which projects enum values into bools.
-}
boolEnumHasFieldInstanceDec :: TyLit -> Type -> Name -> Dec
boolEnumHasFieldInstanceDec fieldLabel ownerType constructorName =
  hasFieldInstanceDec fieldLabel ownerType projectionType getFieldFunClauses
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

sumHasFieldInstanceDec :: TyLit -> Type -> Name -> [Type] -> Dec
sumHasFieldInstanceDec fieldLabel ownerType constructorName memberTypes =
  hasFieldInstanceDec fieldLabel ownerType projectionType getFieldFunClauses
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

productHasFieldInstanceDec :: TyLit -> Type -> Type -> Name -> Int -> Int -> Dec
productHasFieldInstanceDec fieldLabel ownerType projectionType constructorName totalMemberTypes offset =
  hasFieldInstanceDec fieldLabel ownerType projectionType getFieldFunClauses
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
