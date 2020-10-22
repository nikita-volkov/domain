{-|
Model-adapted instance declaration templates.
-}
module Domain.InstanceDec
where

import Domain.Prelude
import Domain.Model
import qualified Language.Haskell.TH as TH
import qualified Data.Text as Text
import qualified Domain.ModelTH as ModelTH
import qualified Domain.ModelText as Text
import qualified THLego.Instances as Instances
import qualified THLego.Helpers as Helpers


-- * HasField
-------------------------

enumHasField :: Text -> Text -> TH.Dec
enumHasField typeName label =
  Instances.enumHasField fieldLabel ownerType constructorName
  where
    fieldLabel =
      TH.StrTyLit (Text.unpack label)
    ownerType =
      TH.ConT (TH.mkName (Text.unpack typeName))
    constructorName =
      TH.mkName (Text.unpack (Text.sumConstructor typeName label))

sumHasField :: Text -> Text -> [Type] -> TH.Dec
sumHasField typeName label memberTypes =
  if null memberTypes
    then 
      Instances.enumHasField thFieldLabel thOwnerType thConstructorName
    else
      Instances.sumHasField thFieldLabel thOwnerType thConstructorName thMemberTypes
  where
    thFieldLabel =
      TH.StrTyLit (Text.unpack label)
    thOwnerType =
      TH.ConT (TH.mkName (Text.unpack typeName))
    thConstructorName =
      TH.mkName (Text.unpack (Text.sumConstructor typeName label))
    thMemberTypes =
      fmap ModelTH.typeType memberTypes

productHasField :: Text -> Text -> Type -> Int -> Int -> TH.Dec
productHasField typeName fieldName projectionType numMemberTypes offset =
  Instances.productHasField thFieldLabel thOwnerType thProjectionType
    thConstructorName numMemberTypes offset
  where
    thFieldLabel =
      TH.StrTyLit (Text.unpack fieldName)
    thOwnerType =
      TH.ConT (TH.mkName (Text.unpack typeName))
    thProjectionType =
      ModelTH.typeType projectionType
    thConstructorName =
      TH.mkName (Text.unpack typeName)


-- * IsLabel
-------------------------

-- ** Accessor
-------------------------

productAccessorIsLabel :: Text -> Text -> Type -> Int -> Int -> TH.Dec
productAccessorIsLabel typeName fieldName projectionType numMemberTypes offset =
  Instances.productAccessorIsLabel
    thFieldLabel thOwnerType thProjectionType thConstructorName
    numMemberTypes offset
  where
    thFieldLabel =
      TH.StrTyLit (Text.unpack fieldName)
    thOwnerType =
      TH.ConT (TH.mkName (Text.unpack typeName))
    thProjectionType =
      ModelTH.typeType projectionType
    thConstructorName =
      TH.mkName (Text.unpack typeName)

sumAccessorIsLabel :: Text -> Text -> [Type] -> TH.Dec
sumAccessorIsLabel typeName label memberTypes =
  if null memberTypes
    then
      Instances.enumAccessorIsLabel
        thFieldLabel thOwnerType thConstructorName
    else
      Instances.sumAccessorIsLabel
        thFieldLabel thOwnerType thConstructorName thMemberTypes
  where
    thFieldLabel =
      TH.StrTyLit (Text.unpack label)
    thOwnerType =
      TH.ConT (TH.mkName (Text.unpack typeName))
    thConstructorName =
      TH.mkName (Text.unpack (Text.sumConstructor typeName label))
    thMemberTypes =
      fmap ModelTH.typeType memberTypes

enumAccessorIsLabel :: Text -> Text -> TH.Dec
enumAccessorIsLabel typeName label =
  Instances.enumAccessorIsLabel
    thFieldLabel thOwnerType thConstructorName
  where
    thFieldLabel =
      TH.StrTyLit (Text.unpack label)
    thOwnerType =
      TH.ConT (TH.mkName (Text.unpack typeName))
    thConstructorName =
      TH.mkName (Text.unpack (Text.sumConstructor typeName label))

-- ** Constructor
-------------------------

curriedSumConstructorIsLabel :: Text -> Text -> [Type] -> TH.Dec
curriedSumConstructorIsLabel typeName label memberTypes =
  Instances.sumConstructorIsLabel
    thFieldLabel thOwnerType thConstructorName thMemberTypes
  where
    thFieldLabel =
      TH.StrTyLit (Text.unpack label)
    thOwnerType =
      TH.ConT (TH.mkName (Text.unpack typeName))
    thConstructorName =
      TH.mkName (Text.unpack (Text.sumConstructor typeName label))
    thMemberTypes =
      fmap ModelTH.typeType memberTypes

uncurriedSumConstructorIsLabel :: Text -> Text -> [Type] -> TH.Dec
uncurriedSumConstructorIsLabel typeName label memberTypes =
  Instances.tupleAdtConstructorIsLabel
    thFieldLabel thOwnerType thConstructorName thMemberTypes
  where
    thFieldLabel =
      TH.StrTyLit (Text.unpack label)
    thOwnerType =
      TH.ConT (TH.mkName (Text.unpack typeName))
    thConstructorName =
      TH.mkName (Text.unpack (Text.sumConstructor typeName label))
    thMemberTypes =
      fmap ModelTH.typeType memberTypes

enumConstructorIsLabel :: Text -> Text -> TH.Dec
enumConstructorIsLabel typeName label =
  Instances.enumConstructorIsLabel
    thFieldLabel thOwnerType thConstructorName
  where
    thFieldLabel =
      TH.StrTyLit (Text.unpack label)
    thOwnerType =
      TH.ConT (TH.mkName (Text.unpack typeName))
    thConstructorName =
      TH.mkName (Text.unpack (Text.sumConstructor typeName label))

wrapperConstructorIsLabel :: Text -> Type -> TH.Dec
wrapperConstructorIsLabel typeName memberType =
  Instances.newtypeConstructorIsLabel
    thFieldLabel thOwnerType thConstructorName thMemberType
  where
    thFieldLabel =
      TH.StrTyLit "value"
    thOwnerType =
      TH.ConT (TH.mkName (Text.unpack typeName))
    thConstructorName =
      TH.mkName (Text.unpack typeName)
    thMemberType =
      ModelTH.typeType memberType

-- ** Mapper
-------------------------

wrapperMapperIsLabel :: Text -> Type -> TH.Dec
wrapperMapperIsLabel typeName memberType =
  Instances.productMapperIsLabel
    thFieldLabel thOwnerType thMemberType thConstructorName 1 0
  where
    thFieldLabel =
      TH.StrTyLit "value"
    thOwnerType =
      TH.ConT (TH.mkName (Text.unpack typeName))
    thConstructorName =
      TH.mkName (Text.unpack typeName)
    thMemberType =
      ModelTH.typeType memberType

productMapperIsLabel :: Text -> Text -> Type -> Int -> Int -> TH.Dec
productMapperIsLabel typeName fieldName projectionType numMemberTypes offset =
  Instances.productMapperIsLabel
    thFieldLabel thOwnerType thProjectionType thConstructorName
    numMemberTypes offset
  where
    thFieldLabel =
      TH.StrTyLit (Text.unpack fieldName)
    thOwnerType =
      TH.ConT (TH.mkName (Text.unpack typeName))
    thProjectionType =
      ModelTH.typeType projectionType
    thConstructorName =
      TH.mkName (Text.unpack typeName)

sumMapperIsLabel :: Text -> Text -> [Type] -> TH.Dec
sumMapperIsLabel typeName label memberTypes =
  Instances.sumMapperIsLabel
    thFieldLabel thOwnerType thConstructorName thMemberTypes
  where
    thFieldLabel =
      TH.StrTyLit (Text.unpack label)
    thOwnerType =
      TH.ConT (TH.mkName (Text.unpack typeName))
    thConstructorName =
      TH.mkName (Text.unpack (Text.sumConstructor typeName label))
    thMemberTypes =
      fmap ModelTH.typeType memberTypes


-- *
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
