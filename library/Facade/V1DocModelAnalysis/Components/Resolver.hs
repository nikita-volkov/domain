module Facade.V1DocModelAnalysis.Components.Resolver
where

import Facade.Prelude hiding (lookup)
import Facade.V1DocModel
import qualified Facade.Model as Norm
import qualified Facade.Util.List as List
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text


type Env =
  HashMap Text Norm.TypeRef

type Err =
  Text

type Eff =
  ExceptT Err ((->) Env)

resolve :: Text -> Eff Norm.TypeRef
resolve =
  error "TODO"

type_ :: Type -> Eff Norm.Type
type_ =
  \ case
    RefType a ->
      Norm.RefType <$> typeRef a

typeRef :: TypeRef -> Eff Norm.TypeRef
typeRef (TypeRef a) =
  case List.unsnoc a of
    Just (b, c) ->
      case b of
        [] ->
          resolve c
        _ ->
          return (Norm.GlobalTypeRef b c)
    Nothing ->
      throwE "Broken type ref"
