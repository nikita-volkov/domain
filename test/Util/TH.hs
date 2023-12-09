module Util.TH where

import qualified Domain
import qualified DomainCore.Model as Model
import Language.Haskell.TH.Quote as TH
import Language.Haskell.TH.Syntax as TH
import Prelude

tryQuoteExp :: QuasiQuoter -> String -> Q Exp
tryQuoteExp q =
  recover (pure (ConE 'Nothing))
    . fmap (AppE (ConE 'Just))
    . quoteExp q

tryExpQ :: Q Exp -> Q Exp
tryExpQ =
  recover (pure (ConE 'Nothing))
    . fmap (AppE (ConE 'Just))

mapQQExpQ :: (Q Exp -> Q Exp) -> QuasiQuoter -> QuasiQuoter
mapQQExpQ mapper (QuasiQuoter a b c d) =
  QuasiQuoter (mapper . a) b c d

maybeDecsQQ :: QuasiQuoter
maybeDecsQQ =
  mapQQExpQ (fmap mapper . tryExpQ) Domain.schema
  where
    mapper =
      AppE
        ( AppE
            (VarE 'fmap)
            (SigE (VarE 'unsafeCoerce) sig)
        )
      where
        sig =
          AppT
            (AppT ArrowT (ConT ''Domain.Schema))
            (AppT ListT (ConT ''Model.TypeDec))
