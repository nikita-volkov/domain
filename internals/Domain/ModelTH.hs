module Domain.ModelTH
where

import Domain.Prelude
import Domain.Model
import qualified Language.Haskell.TH as TH
import qualified THLego.Helpers as TH
import qualified Domain.ModelText as Text
import qualified Data.Text as Text
import qualified Data.Char as Char


typeDec fieldNaming (TypeDec a b) =
  case b of
    AliasTypeDef b ->
      TH.typeSynonymDec (TH.textName a) (typeType b)
    WrapperTypeDef b ->
      case fieldNaming of
        Just fieldNaming ->
          TH.recordNewtypeDec (TH.textName a) (recordFieldName fieldNaming a "value") (typeType b)
        Nothing ->
          TH.normalNewtypeDec (TH.textName a) (typeType b)
    EnumTypeDef b ->
      TH.enumDec (TH.textName a) (sumConstructorName a <$> b)
    SumTypeDef b ->
      TH.sumAdtDec (TH.textName a) (fmap (bimap (sumConstructorName a) (fmap typeType)) b)
    ProductTypeDef fields ->
      case fieldNaming of
        Just fieldNaming ->
          case fields of
            [(memberName, memberType)] ->
              TH.recordNewtypeDec (TH.textName a) (recordFieldName fieldNaming a memberName) (typeType memberType)
            _ ->
              TH.recordAdtDec (TH.textName a) (fmap (bimap (recordFieldName fieldNaming a) typeType) fields)
        Nothing ->
          case fields of
            [(_, memberType)] ->
              TH.normalNewtypeDec (TH.textName a) (typeType memberType)
            _ ->
              TH.productAdtDec (TH.textName a) (fmap (typeType . snd) fields)

typeType =
  \ case
    AppType a b ->
      TH.AppT (typeType a) (typeType b)
    RefType a ->
      TH.ConT (TH.textName a)
    ListType ->
      TH.ListT
    TupleType a ->
      TH.TupleT a

recordFieldName fieldNaming a b =
  TH.textName (Text.recordField fieldNaming a b)

sumConstructorName a b =
  TH.textName (Text.sumConstructor a b)
