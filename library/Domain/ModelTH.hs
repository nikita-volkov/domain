module Domain.ModelTH
where

import Domain.Prelude
import Domain.Model
import qualified Language.Haskell.TH as TH
import qualified Domain.Util.TH as TH
import qualified Domain.ModelText as Text
import qualified Data.Text as Text
import qualified Data.Char as Char


typeDec nameFields (TypeDec a b) =
  case b of
    AliasTypeDef b ->
      TH.typeSynonymDec (TH.textName a) (typeType b)
    WrapperTypeDef b ->
      if nameFields
        then TH.recordNewtypeDec (TH.textName a) (recordFieldName a "Value") (typeType b)
        else TH.normalNewtypeDec (TH.textName a) (typeType b)
    EnumTypeDef b ->
      TH.enumDec (TH.textName a) (sumConstructorName a <$> b)
    SumTypeDef b ->
      TH.sumAdtDec (TH.textName a) (fmap (bimap (sumConstructorName a) (fmap typeType)) b)
    ProductTypeDef b ->
      if nameFields
        then TH.recordAdtDec (TH.textName a) (fmap (bimap (recordFieldName a) typeType) b)
        else TH.productAdtDec (TH.textName a) (fmap (typeType . snd) b)

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

recordFieldName a b =
  TH.textName (Text.recordField a b)

sumConstructorName a b =
  TH.textName (Text.sumConstructor a b)
