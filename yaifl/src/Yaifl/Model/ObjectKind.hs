module Yaifl.Model.ObjectKind
  ( makeKindDAG
  , ObjectKindInfo(..)
  ) where

import Yaifl.Prelude
import Yaifl.Core.Kinds.Object

makeKindDAG :: Map ObjectKind (Set ObjectKind)
makeKindDAG = fromList
  [ ("object", fromList [])
  , ("thing", fromList ["object"])
  , ("room", fromList ["object"])
  -- probably useless because we don't have first class directions
  , ("direction", fromList [])
  , ("container", fromList ["thing"])
  , ("supporter", fromList ["thing"])
  , ("backdrop", fromList ["thing"])
  , ("person", fromList ["animal"])
  , ("man", fromList ["person"])
  , ("woman", fromList ["person"])
  , ("animal", fromList ["thing"])
  , ("device", fromList ["thing"])
  -- same as direction, probably useless
  , ("region", fromList [])
  , ("door", fromList ["thing"])
  -- we also haven't (yet) got concepts
  ]

data ObjectKindInfo = ObjectKindInfo
  { parentKinds :: Set ObjectKind
  , understandAs :: [Text]
  , pluralUnderstandAs :: [Text]
  }

makeFieldLabelsNoPrefix ''ObjectKindInfo