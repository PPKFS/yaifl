module Yaifl.Core.AdaptiveText
  ( AdaptiveText(..)
  , AdaptiveTextDomain
  ) where

import Replace.Megaparsec ( splitCap )
import Text.Megaparsec ( match, anySingle, manyTill, single, Parsec )
import Solitude

data AdaptiveText domain = StaticText Text | AdaptiveText [Text] [Text]
  deriving stock (Eq, Generic, Ord, Show)

type family AdaptiveTextDomain a

instance IsString (AdaptiveText domain) where
  fromString s = let
    subMatcher :: Parsec Void String String
    subMatcher = single '[' >> manyTill anySingle (single ']')
    splitter = splitCap (match subMatcher) s
    (ls, rs) = partitionEithers splitter
    in
    case (ls, rs) of
      ([l], []) -> StaticText (toText l)
      (ls', rs') -> AdaptiveText (map toText ls') (map (toText . fst) rs')

instance Buildable (AdaptiveText domain) where
  build = show
