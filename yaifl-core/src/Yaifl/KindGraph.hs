{-|
Module      : Yaifl.KindGraph
Copyright   : (c) Avery 2023-2026
License     : MIT
Maintainer  : ppkfs@outlook.com

The kind graph defines hierarchical relationships between object kinds
for ontological querying and classification.

This module provides a directed acyclic graph (DAG) structure that:

- Establishes kind relationships (e.g., person → animal)
- Enables runtime type queries (e.g., "is this a door?")
- Uses simple string-based relationships with no associated data or behaviour,
  avoiding the complexity of multiple inheritance systems

See also:
- `Yaifl.Object.Kind` for the ObjectKind type definition
- `Yaifl.Metadata` for the kind system usage in game logic
-}

module Yaifl.KindGraph
  ( makeKindDAG
  , ObjectKindInfo(..)
  ) where

import Yaifl.Prelude
import Yaifl.Object.Kind

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