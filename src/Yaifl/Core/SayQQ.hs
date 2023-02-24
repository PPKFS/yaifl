{-# LANGUAGE TemplateHaskellQuotes #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}

module Yaifl.Core.SayQQ
  ( saying
  , sayingTell
  ) where

import Solitude

import Language.Haskell.TH hiding (Type)
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax hiding (Type)

import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P

data SayingKind = Tell | Raw

data SayingPiece (sayKind :: SayingKind) =
  TextLit Text
  | Sayable Text
  | SayablePrefix Text Text
saying :: QuasiQuoter
saying = QuasiQuoter {
    quoteExp  = compile (Proxy @'Raw)
  , quotePat  = notHandled "patterns"
  , quoteType = notHandled "types"
  , quoteDec  = notHandled "declarations"
  }
  where notHandled things = error $
          things <> " are not handled by the saying quasiquoter."

sayingTell :: QuasiQuoter
sayingTell = QuasiQuoter {
    quoteExp  = compile (Proxy @'Tell)
  , quotePat  = notHandled "patterns"
  , quoteType = notHandled "types"
  , quoteDec  = notHandled "declarations"
  }
  where notHandled things = error $
          things <> " are not handled by the saying quasiquoter."

compile :: forall sayKind. Lift (SayingPiece sayKind) => Proxy sayKind -> String -> ExpQ
compile _ s =
  case P.parse @Void @String @[SayingPiece sayKind] sayingParser "" s of
    Left  err -> fail (show err)
    Right aat -> [e| sequence_ aat |]

instance Lift (SayingPiece 'Tell) where
  lift (TextLit t) = [e| sayTell t |]
  lift (Sayable t) = [e| sayTell t |]
  lift (SayablePrefix pref t) =
    let tyName = "SayType_" <> toString pref
        tyAppl = (ConE $ mkName $ tyName) in
          pure $ AppE (VarE $ mkName "sayTell") (AppE tyAppl (VarE $ mkName $ toString t))

instance Lift (SayingPiece 'Raw) where
  lift (TextLit t) = [e| say t |]
  lift (Sayable t) = [e| say t |]
  lift (SayablePrefix pref t) =
    let tyName = "SayType_" <> toString pref
        tyAppl = (ConE $ mkName $ tyName) in
          pure $ AppE (VarE $ mkName "say") (AppE tyAppl (VarE $ mkName $ toString t))

sayingParser :: P.Parsec Void String [SayingPiece a]
sayingParser = many piece <* P.eof where
  piece = sayable <|> sayableTwoPart <|> lit
  sayable = P.try $ Sayable . toText <$> (P.string "{" *> P.takeWhile1P Nothing (\x -> x `notElem` ['}', ' ']) <* P.char '}')
  sayableTwoPart = do
    P.string "{"
    fs <- toText <$> P.takeWhile1P Nothing (\x -> x `notElem` ['}', ' '])
    P.single ' '
    sn <- toText <$> P.takeWhile1P Nothing (\x -> x `notElem` ['}', ' '])
    P.char '}'
    pure $ SayablePrefix fs sn
  lit = TextLit . toText <$> P.takeWhile1P Nothing (/= '{')
