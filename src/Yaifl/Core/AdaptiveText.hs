{-# LANGUAGE InstanceSigs #-}
module Yaifl.Core.AdaptiveText
  ( AdaptiveText(..)
  , AdaptiveTextDomain
  , rawAdaptiveText
  ) where

import Replace.Megaparsec ( splitCap )
import Text.Megaparsec ( match, anySingle, manyTill, single, Parsec )
import Solitude
import Data.Text.Lazy.Builder


data AdaptiveText domain = StaticText Text | AdaptiveText [Text] [Text]
  deriving stock (Eq, Generic, Ord, Show)

type family AdaptiveTextDomain a

rawAdaptiveText ::
  AdaptiveText domain
  -> Text
rawAdaptiveText (StaticText t) = t
-- TODO
rawAdaptiveText (AdaptiveText _subs main) = mconcat main

instance IsString (AdaptiveText domain) where
  fromString :: String -> AdaptiveText domain
  fromString s = let
    subMatcher :: Parsec Void String String
    subMatcher = single '[' >> manyTill anySingle (single ']')
    splitter = splitCap (match subMatcher) s
    (ls, rs) = partitionEithers splitter
    in
    case (ls, rs) of
      (l, []) -> StaticText (toText $ mconcat l)
      (ls', rs') -> AdaptiveText (map toText ls') (map (toText . fst) rs')

instance Display (AdaptiveText domain) where
  displayBuilder = fromText . rawAdaptiveText

instance Buildable (AdaptiveText domain) where
  build = displayBuilder
