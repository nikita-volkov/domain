-- |
-- Model-adapted instance declaration templates.
module Domain.TH.InstanceDec where

import Domain.Prelude
import DomainCore.Model
import qualified DomainCore.TH as CoreTH
import qualified Language.Haskell.TH as TH
import qualified THLego.Helpers as Helpers
import qualified THLego.Instances as Instances

-- * HasField

-------------------------

enumHasField :: Text -> Text -> TH.Dec
enumHasField typeName label =
  Instances.enumHasField fieldLabel ownerType constructorName
  where
    fieldLabel =
      Helpers.textTyLit label
    ownerType =
      TH.ConT (Helpers.textName typeName)
    constructorName =
      CoreTH.sumConstructorName typeName label

sumHasField :: Text -> Text -> [Type] -> TH.Dec
sumHasField typeName label memberTypes =
  if null memberTypes
    then Instances.enumHasField thFieldLabel thOwnerType thConstructorName
    else Instances.sumHasField thFieldLabel thOwnerType thConstructorName thMemberTypes
  where
    thFieldLabel =
      Helpers.textTyLit label
    thOwnerType =
      TH.ConT (Helpers.textName typeName)
    thConstructorName =
      CoreTH.sumConstructorName typeName label
    thMemberTypes =
      fmap CoreTH.typeType memberTypes

productHasField :: Text -> Text -> Type -> Int -> Int -> TH.Dec
productHasField typeName fieldName projectionType numMemberTypes offset =
  Instances.productHasField
    thFieldLabel
    thOwnerType
    thProjectionType
    thConstructorName
    numMemberTypes
    offset
  where
    thFieldLabel =
      Helpers.textTyLit fieldName
    thOwnerType =
      TH.ConT (Helpers.textName typeName)
    thProjectionType =
      CoreTH.typeType projectionType
    thConstructorName =
      Helpers.textName typeName

-- * IsLabel

-------------------------

-- ** Accessor

-------------------------

productAccessorIsLabel :: Text -> Text -> Type -> Int -> Int -> TH.Dec
productAccessorIsLabel typeName fieldName projectionType numMemberTypes offset =
  Instances.productAccessorIsLabel
    thFieldLabel
    thOwnerType
    thProjectionType
    thConstructorName
    numMemberTypes
    offset
  where
    thFieldLabel =
      Helpers.textTyLit fieldName
    thOwnerType =
      TH.ConT (Helpers.textName typeName)
    thProjectionType =
      CoreTH.typeType projectionType
    thConstructorName =
      Helpers.textName typeName

sumAccessorIsLabel :: Text -> Text -> [Type] -> TH.Dec
sumAccessorIsLabel typeName label memberTypes =
  if null memberTypes
    then
      Instances.enumAccessorIsLabel
        thFieldLabel
        thOwnerType
        thConstructorName
    else
      Instances.sumAccessorIsLabel
        thFieldLabel
        thOwnerType
        thConstructorName
        thMemberTypes
  where
    thFieldLabel =
      Helpers.textTyLit label
    thOwnerType =
      TH.ConT (Helpers.textName typeName)
    thConstructorName =
      CoreTH.sumConstructorName typeName label
    thMemberTypes =
      fmap CoreTH.typeType memberTypes

enumAccessorIsLabel :: Text -> Text -> TH.Dec
enumAccessorIsLabel typeName label =
  Instances.enumAccessorIsLabel
    thFieldLabel
    thOwnerType
    thConstructorName
  where
    thFieldLabel =
      Helpers.textTyLit label
    thOwnerType =
      TH.ConT (Helpers.textName typeName)
    thConstructorName =
      CoreTH.sumConstructorName typeName label

-- ** Constructor

-------------------------

curriedSumConstructorIsLabel :: Text -> Text -> [Type] -> TH.Dec
curriedSumConstructorIsLabel typeName label memberTypes =
  Instances.sumConstructorIsLabel
    thFieldLabel
    thOwnerType
    thConstructorName
    thMemberTypes
  where
    thFieldLabel =
      Helpers.textTyLit label
    thOwnerType =
      TH.ConT (Helpers.textName typeName)
    thConstructorName =
      CoreTH.sumConstructorName typeName label
    thMemberTypes =
      fmap CoreTH.typeType memberTypes

uncurriedSumConstructorIsLabel :: Text -> Text -> [Type] -> TH.Dec
uncurriedSumConstructorIsLabel typeName label memberTypes =
  Instances.tupleAdtConstructorIsLabel
    thFieldLabel
    thOwnerType
    thConstructorName
    thMemberTypes
  where
    thFieldLabel =
      Helpers.textTyLit label
    thOwnerType =
      TH.ConT (Helpers.textName typeName)
    thConstructorName =
      CoreTH.sumConstructorName typeName label
    thMemberTypes =
      fmap CoreTH.typeType memberTypes

enumConstructorIsLabel :: Text -> Text -> TH.Dec
enumConstructorIsLabel typeName label =
  Instances.enumConstructorIsLabel
    thFieldLabel
    thOwnerType
    thConstructorName
  where
    thFieldLabel =
      Helpers.textTyLit label
    thOwnerType =
      TH.ConT (Helpers.textName typeName)
    thConstructorName =
      CoreTH.sumConstructorName typeName label

wrapperConstructorIsLabel :: Text -> Type -> TH.Dec
wrapperConstructorIsLabel typeName memberType =
  Instances.newtypeConstructorIsLabel
    thFieldLabel
    thOwnerType
    thConstructorName
    thMemberType
  where
    thFieldLabel =
      TH.StrTyLit "value"
    thOwnerType =
      TH.ConT (Helpers.textName typeName)
    thConstructorName =
      Helpers.textName typeName
    thMemberType =
      CoreTH.typeType memberType

-- ** Mapper

-------------------------

wrapperMapperIsLabel :: Text -> Type -> TH.Dec
wrapperMapperIsLabel typeName memberType =
  Instances.productMapperIsLabel
    thFieldLabel
    thOwnerType
    thMemberType
    thConstructorName
    1
    0
  where
    thFieldLabel =
      TH.StrTyLit "value"
    thOwnerType =
      TH.ConT (Helpers.textName typeName)
    thConstructorName =
      Helpers.textName typeName
    thMemberType =
      CoreTH.typeType memberType

productMapperIsLabel :: Text -> Text -> Type -> Int -> Int -> TH.Dec
productMapperIsLabel typeName fieldName projectionType numMemberTypes offset =
  Instances.productMapperIsLabel
    thFieldLabel
    thOwnerType
    thProjectionType
    thConstructorName
    numMemberTypes
    offset
  where
    thFieldLabel =
      Helpers.textTyLit fieldName
    thOwnerType =
      TH.ConT (Helpers.textName typeName)
    thProjectionType =
      CoreTH.typeType projectionType
    thConstructorName =
      Helpers.textName typeName

sumMapperIsLabel :: Text -> Text -> [Type] -> TH.Dec
sumMapperIsLabel typeName label memberTypes =
  Instances.sumMapperIsLabel
    thFieldLabel
    thOwnerType
    thConstructorName
    thMemberTypes
  where
    thFieldLabel =
      Helpers.textTyLit label
    thOwnerType =
      TH.ConT (Helpers.textName typeName)
    thConstructorName =
      CoreTH.sumConstructorName typeName label
    thMemberTypes =
      fmap CoreTH.typeType memberTypes

-------------------------

deriving_ :: TH.Name -> Text -> TH.Dec
deriving_ className typeNameText =
  TH.StandaloneDerivD Nothing [] headType
  where
    headType =
      TH.AppT (TH.ConT className) (TH.ConT (Helpers.textName typeNameText))

empty :: TH.Name -> Text -> TH.Dec
empty className typeNameText =
  TH.InstanceD Nothing [] headType []
  where
    headType =
      TH.AppT (TH.ConT className) (TH.ConT (Helpers.textName typeNameText))
