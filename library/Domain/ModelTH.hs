module Domain.ModelTH
where

import Domain.Prelude
import Domain.Model
import qualified Language.Haskell.TH as TH
import qualified Domain.Util.TH as TH
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
          TH.recordNewtypeDec (TH.textName a) (recordFieldName fieldNaming a "Value") (typeType b)
        Nothing ->
          TH.normalNewtypeDec (TH.textName a) (typeType b)
    EnumTypeDef b ->
      TH.enumDec (TH.textName a) (sumConstructorName a <$> b)
    SumTypeDef b ->
      TH.sumAdtDec (TH.textName a) (fmap (bimap (sumConstructorName a) (fmap typeType)) b)
    ProductTypeDef b ->
      case fieldNaming of
        Just fieldNaming ->
          TH.recordAdtDec (TH.textName a) (fmap (bimap (recordFieldName fieldNaming a) typeType) b)
        Nothing ->
          TH.productAdtDec (TH.textName a) (fmap (typeType . snd) b)

typeType =
  \ case
    AppType a b ->
      TH.AppT (typeType a) (typeType b)
    RefType a ->
      typeRefType a
    ListType ->
      TH.ListT
    TupleType a ->
      TH.TupleT a

typeRefType =
  TH.ConT . TH.textName . Text.typeRef

recordFieldName fieldNaming a b =
  TH.textName (Text.recordField fieldNaming a b)

sumConstructorName a b =
  TH.textName (Text.sumConstructor a b)
