module Domain.TH.TypeDec where

import Domain.Prelude
import DomainCore.Model
import qualified DomainCore.TH as CoreTH
import qualified Language.Haskell.TH as TH
import qualified THLego.Helpers as TH

typeDec :: Maybe (Bool, Bool) -> TypeDec -> TH.Dec
typeDec fieldNaming (TypeDec a b) =
  case b of
    SumTypeDef b ->
      TH.sumAdtDec (TH.textName a) (fmap (bimap (CoreTH.sumConstructorName a) (fmap CoreTH.typeType)) b)
    ProductTypeDef fields ->
      case fieldNaming of
        Just (underscore, prefixWithTypeName) ->
          case fields of
            [(memberName, memberType)] ->
              TH.recordNewtypeDec (TH.textName a) (CoreTH.recordFieldName underscore prefixWithTypeName a memberName) (CoreTH.typeType memberType)
            _ ->
              TH.recordAdtDec (TH.textName a) (fmap (bimap (CoreTH.recordFieldName underscore prefixWithTypeName a) CoreTH.typeType) fields)
        Nothing ->
          case fields of
            [(_, memberType)] ->
              TH.normalNewtypeDec (TH.textName a) (CoreTH.typeType memberType)
            _ ->
              TH.productAdtDec (TH.textName a) (fmap (CoreTH.typeType . snd) fields)
