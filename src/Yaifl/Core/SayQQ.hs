{-# LANGUAGE TemplateHaskellQuotes #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}

module Yaifl.Core.SayQQ
  ( saying
  , sayingTell
  ) where

import Solitude

import Data.Char (isUpper, toUpper, toLower)
import Language.Haskell.TH hiding (Type)
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax hiding (Type)
import qualified Data.Text as T
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P

data SayingKind = Tell | Raw

class Verb v where

data SayingPiece (sayKind :: SayingKind) =
  RegularText Text -- ^ some literal text
  | Sayable Text -- ^ something sayable directly (a binding); {foo}
  | SayLiteral Text -- ^ SayLit_Foo; #{foo}
  | SayModal Text Text-- ^ SayModal_Foo; #{modal foo}
  | SayRegarding Text -- ^ SayRegarding_Foo; #{regarding foo}
  | SayArticle Text Text -- ^ SayArticle_Foo; {article foo}
  | SayAdapt Text Text -- ^ SayAdapt_Foo; #{adapt foo for tense/person bar}

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
  lift = pure . liftSayingWithMethod "sayTell"

instance Lift (SayingPiece 'Raw) where
  lift = pure . liftSayingWithMethod "say"

makeCapitalised ::
  String
  -> Text
  -> Exp
makeCapitalised prefix t =
  if isUpper (T.head t)
    then AppE (ConE $ mkName $ prefix <> toString t) (ConE $ mkName "True")
    else AppE (ConE $ mkName $ prefix <> toString (t & _head %~ toUpper)) (ConE $ mkName "False")

uncapitaliseString ::
  Text
  -> String
uncapitaliseString = toString . (_head %~ toLower)

isCapitalised ::
  Text
  -> String
isCapitalised = show . isUpper . T.head

methodVarE ::
  String
  -> Exp
methodVarE method = VarE $ mkName method

liftSayingWithMethod ::
  String
  -> SayingPiece sayKind
  -> Exp
liftSayingWithMethod method (RegularText t) =
  AppE
    (AppTypeE
      (methodVarE method)
      (ConT $ mkName "Text")
    ) -- say or sayTell @Text
    (LitE $ StringL $ toString t)

liftSayingWithMethod method (SayLiteral t) =
  AppE
    (methodVarE method) -- say or sayTell
    (AppE
      (AppTypeE
        (ConE $ mkName "SayLiteral")
        (LitT $ StrTyLit $ uncapitaliseString t) -- SayLiteral @"foo"
      )
      (ConE $ mkName $ isCapitalised t) -- Capitalised?
    )

liftSayingWithMethod method (Sayable t) =
  AppE
    (methodVarE method)
    (VarE $ mkName $ toString t)

liftSayingWithMethod method (SayArticle pref t) =
  AppE
    (methodVarE method)
    (AppE
      (AppE
        (AppTypeE
          (ConE $ mkName "SayArticle")
          (LitT $ StrTyLit $ uncapitaliseString pref) -- SayArticle @"foo"
        )
        (ConE $ mkName $ isCapitalised t) -- Capitalised?
      )
      (VarE $ mkName $ toString t)
    )
liftSayingWithMethod method (SayModal modal t) =
  AppE
    (methodVarE method)
    (AppE
      (AppE
        (AppTypeE
          (AppTypeE
            (ConE $ mkName "SayModal")
            (LitT $ StrTyLit $ uncapitaliseString modal) -- SayArticle @"foo"
          )
          (LitT $ StrTyLit $ uncapitaliseString t)
        )
        (ConE $ mkName $ isCapitalised modal) -- Capitalised?
      )
      (ConE $ mkName $ isCapitalised t)
    )
liftSayingWithMethod _method (SayRegarding _reg) = error "todo regarding"
liftSayingWithMethod _method (SayAdapt _adaptTo _t) = error "todo adapt to"


sayingParser :: P.Parsec Void String [SayingPiece a]
sayingParser = many piece <* P.eof where
  piece = literalSub <|> sayableSub <|> lit
  literalSub = do
    P.string "#{"
    subPieces <- oneOrTwoPieces
    case subPieces of
      [] -> error "no substitution body found"
      [x] -> pure $ SayLiteral x
      ["regarding", x] -> pure $ SayRegarding x
      ["adapt", v, "for", t] -> pure $ SayAdapt t v
      [modal, x] -> pure $ SayModal modal x
      _ -> error "too many substitution pieces"
  sayableSub = do
    P.string "{"
    subPieces <- oneOrTwoPieces
    case subPieces of
      [] -> error "no substitution body found"
      [x] -> pure $ Sayable x
      [art, x] -> pure $ SayArticle art x
      _ -> error "too many substitution pieces"
  oneOrTwoPieces = toText <$$> many (P.takeWhile1P Nothing (\x -> x `notElem` ['}', ' ']) <* optional (P.single ' ')) <* P.char '}'
  lit = RegularText . toText <$> P.takeWhile1P Nothing (\x -> x `notElem` ['{', '#'])
