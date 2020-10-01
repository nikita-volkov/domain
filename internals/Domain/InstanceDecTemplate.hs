{-|
Model-adapted instance declaration templates.
-}
module Domain.InstanceDecTemplate
where

import Domain.Prelude
import Domain.Model
import qualified Language.Haskell.TH as TH
import qualified Data.Text as Text
import qualified Domain.ModelTH as ModelTH
import qualified Domain.ModelText as Text
import qualified Domain.Util.TH as Templates


enumHasField :: Text -> Text -> TH.Dec
enumHasField typeName variantName =
  Templates.boolEnumHasFieldInstanceDec fieldLabel ownerType constructorName
  where
    fieldLabel =
      TH.StrTyLit (Text.unpack variantName)
    ownerType =
      TH.ConT (TH.mkName (Text.unpack typeName))
    constructorName =
      TH.mkName (Text.unpack (Text.sumConstructor typeName variantName))

sumHasField :: Text -> Text -> [Type] -> TH.Dec
sumHasField typeName variantName memberTypes =
  Templates.sumHasFieldInstanceDec thFieldLabel thOwnerType thConstructorName thMemberTypes
  where
    thFieldLabel =
      TH.StrTyLit (Text.unpack variantName)
    thOwnerType =
      TH.ConT (TH.mkName (Text.unpack typeName))
    thConstructorName =
      TH.mkName (Text.unpack (Text.sumConstructor typeName variantName))
    thMemberTypes =
      fmap ModelTH.typeType memberTypes

productHasField :: Text -> Text -> Type -> Int -> Int -> TH.Dec
productHasField typeName fieldName projectionType numMemberTypes offset =
  Templates.productHasFieldInstanceDec thFieldLabel thOwnerType thProjectionType
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
