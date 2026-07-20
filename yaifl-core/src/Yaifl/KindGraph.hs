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
  ( -- * Kind Graph
    makeKindDAG
  -- * Kind Information
  , ObjectKindInfo(..)
  ) where

import Yaifl.Prelude
import Yaifl.Object.Kind

-- | The hierarchical relationships between object kinds.
--
-- This DAG defines the inheritance structure for game objects:
-- - Basic types: object, thing, room
-- - Container types: container, supporter
-- - Character types: person, man, woman, animal
-- - Special types: device, door, backdrop
--
-- Each entry maps a kind to its parent kinds. For example:
-- - "thing" inherits from "object"
-- - "person" inherits from "animal" which inherits from "thing"
-- - "container" inherits from "thing"
--
-- This structure enables runtime type queries and classification.
makeKindDAG :: Map ObjectKind (Set ObjectKind)
makeKindDAG = fromList
  [ ("object", fromList [])  -- Root type, no parents
  , ("thing", fromList ["object"])  -- Physical objects
  , ("room", fromList ["object"])  -- Locations
  , ("direction", fromList [])  -- Cardinal directions
  , ("container", fromList ["thing"])  -- Objects that can contain others
  , ("supporter", fromList ["thing"])  -- Objects that can support others
  , ("backdrop", fromList ["thing"])  -- Scenery objects
  , ("person", fromList ["animal"])  -- Human characters
  , ("man", fromList ["person"])  -- Male characters
  , ("woman", fromList ["person"])  -- Female characters
  , ("animal", fromList ["thing"])  -- Non-human creatures
  , ("device", fromList ["thing"])  -- Interactive objects
  , ("region", fromList [])  -- Geographic areas
  , ("door", fromList ["thing"])  -- Portals between rooms
  ]

-- | Runtime information about an object kind.
--
-- This record stores metadata associated with each object kind that applies
-- to ALL objects of that kind. This enables automatic understanding of objects
-- without requiring manual configuration for each instance.
--
-- The understanding terms are used by the parser to recognize different
-- ways players might refer to objects. For example, if "sword" has
-- "blade" and "weapon" in its `understandAs` list, then ANY sword
-- in the game can be referred to as "blade" or "weapon" without
-- needing to manually add these terms to each individual sword object.
--
-- This system provides automatic synonym support and reduces the need
-- for per-object configuration.
data ObjectKindInfo = ObjectKindInfo
  { parentKinds :: Set ObjectKind
  -- ^ Parent kinds this kind inherits from
  , understandAs :: [Text]
  -- ^ Terms that apply to all objects of this kind (e.g., "blade" for swords)
  , pluralUnderstandAs :: [Text]
  -- ^ Plural forms of the kind-specific terms
  }

makeFieldLabelsNoPrefix ''ObjectKindInfo