{-|
Model-unaware helpers.
-}
module Domain.Util.TH
where

import Domain.Prelude
import Language.Haskell.TH
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

listAppT :: Type -> [Type] -> Type
listAppT base args =
  foldl' AppT base args

appliedTupleT :: [Type] -> Type
appliedTupleT a =
  foldl' AppT (TupleT (length a)) a

indexName :: Int -> Name
indexName =
  mkName . showChar '_' . show

enumNames :: Int -> [Name]
enumNames =
  fmap indexName . enumFromTo 0 . pred

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


-- *
-------------------------

sumConstructorIsLabelInstanceDec :: Name -> Name -> TyLit -> [Type] -> Dec
sumConstructorIsLabelInstanceDec typeName conName label memberTypes =
  InstanceD Nothing [] headType [fromLabelDec]
  where
    headType =
      listAppT (ConT ''IsLabel) [LitT label, repType]
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
      listAppT (ConT ''IsLabel) [LitT label, repType]
      where
        repType =
          listAppT ArrowT [ConT typeName, resultType]
    fromLabelDec =
      FunD 'fromLabel [Clause [] (NormalB accessor) []]
