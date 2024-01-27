module Yaifl.Model.ObjectKind
  ( makeKindDAG
  , ObjectKindInfo(..)
  ) where

import Solitude
import Yaifl.Model.Kinds.Object
import Effectful.Optics
import qualified Data.Set as S

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
  , ("animal", fromList ["thing"])
  -- same as direction, probably useless
  , ("region", fromList [])
  , ("door", fromList ["thing"])
  -- we also haven't (yet) got concepts
  ]

data ObjectKindInfo = ObjectKindInfo
  { childKinds :: Set ObjectKind
  , understandAs :: [Text]
  , pluralUnderstandAs :: [Text]
  }

makeFieldLabelsNoPrefix ''ObjectKindInfo