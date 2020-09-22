module Facade.V1DocModelAnalysis.Components.TypeResolutionMapBuilder
where

import Facade.Prelude
import Facade.V1DocModel
import qualified Facade.Model as Norm
import qualified Facade.Util.List as List
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text


type Model =
  HashMap Text Norm.TypeRef

type Err =
  Text

type Eff =
  StateT Model (Either Err)

run :: Eff () -> Either Err (HashMap Text Norm.TypeRef)
run eff =
  execStateT eff mempty

add :: Text -> Text -> Norm.TypeRef -> Eff ()
add label name ref =
  StateT $ fmap ((),) . HashMap.alterF altering name
  where
    altering =
      \ case
        Just oldRef ->
          let
            msg =                
              "Cannot define " <> label <> " type " <> name <> ". " <>
              "This name is already assigned."
            in Left msg
        Nothing ->
          Right (Just ref)

addImportDef :: Text -> ImportDef -> Eff ()
addImportDef name (ImportDef (TypeRef typeRefSegments)) =
  case List.unsnoc typeRefSegments of
    Just (typeRefNamespace, typeRefName) ->
      add "import" name (Norm.GlobalTypeRef typeRefNamespace typeRefName)
    Nothing ->
      lift (Left "Broken type ref")

addSelfReference :: Text -> Text -> Eff ()
addSelfReference label name =
  add label name (Norm.LocalTypeRef name)

byTypeName :: (Text -> a -> Eff ()) -> ByTypeName a -> Eff ()
byTypeName mapper (ByTypeName btn) =
  traverse_ (uncurry mapper) btn

addDoc :: Doc -> Eff ()
addDoc (Doc a b c d e f) =
  do
    byTypeName addImportDef a
    byTypeName (const . addSelfReference "alias") b
    byTypeName (const . addSelfReference "wrapper") c
    byTypeName (const . addSelfReference "enum") d
    byTypeName (const . addSelfReference "product") e
    byTypeName (const . addSelfReference "sum") f
