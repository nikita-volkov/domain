module Facade.Components.TypeResolutionMapBuilder
where

import Facade.Prelude
import Facade.Model
import qualified Facade.V1DocModel as Doc
import qualified Facade.Util.List as List
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text


type Model =
  HashMap Text TypeRef

type Err =
  Text

type Eff =
  StateT Model (Either Err)

run :: Eff () -> Either Err (HashMap Text TypeRef)
run eff =
  execStateT eff mempty

add :: Text -> Text -> TypeRef -> Eff ()
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

addImportDef :: Text -> Doc.ImportDef -> Eff ()
addImportDef name (Doc.ImportDef (Doc.TypeRef typeRefSegments)) =
  case List.unsnoc typeRefSegments of
    Just (typeRefNamespace, typeRefName) ->
      add "import" name (GlobalTypeRef typeRefNamespace typeRefName)
    Nothing ->
      lift (Left "Broken type ref")

addSelfReference :: Text -> Text -> Eff ()
addSelfReference label name =
  add label name (LocalTypeRef name)

byTypeName :: (Text -> a -> Eff ()) -> Doc.ByTypeName a -> Eff ()
byTypeName mapper (Doc.ByTypeName btn) =
  traverse_ (uncurry mapper) btn

addDoc :: Doc.Doc -> Eff ()
addDoc (Doc.Doc a b c d e f) =
  do
    byTypeName addImportDef a
    byTypeName (const . addSelfReference "alias") b
    byTypeName (const . addSelfReference "wrapper") c
    byTypeName (const . addSelfReference "enum") d
    byTypeName (const . addSelfReference "product") e
    byTypeName (const . addSelfReference "sum") f
