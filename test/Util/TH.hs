module Util.TH where

import Prelude
import qualified Data.Text as Text
import Language.Haskell.TH.Syntax as TH
import Language.Haskell.TH.Quote as TH


tryRunExpQ :: QuasiQuoter -> Text -> Q Exp
tryRunExpQ q text =
  recover (pure (ConE 'Nothing)) $
  fmap (AppE (ConE 'Just)) $
  quoteExp q $
  Text.unpack text
