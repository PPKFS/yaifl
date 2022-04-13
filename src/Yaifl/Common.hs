{-|
Module      : Yaifl.Common
Description : Entities (IDs) and Stores (maps of entities to objects). Otherwise basic stuff.
Copyright   : (c) Avery, 2022
License     : MIT
Maintainer  : ppkfs@outlook.com
Stability   : No
-}

-- for Display a (sorry not sorry, Hécate)
{-# OPTIONS_GHC -Wno-orphans #-}

module Yaifl.Common
  (-- * Datatypes
  Entity(..)
  , Store(..)
  , HasID(..)
  , Timestamp(..)
  , WorldModel(..)
  , RoomDescriptions(..)
  , PlainWorldModel
  , defaultVoidID
  , emptyStore
  -- * Object querying
  , isThing
  , isRoom
    -- * Type family nonsense
  , WMObjSpecifics
  , WMValues
  , WMDirections
  )
where

import Solitude
import qualified Data.EnumMap.Strict as EM
import qualified Data.IntMap.Strict as IM
import Display

instance {-# OVERLAPPABLE #-} Display a where
  display = const "No display instance"
  
-- | Again lifted directly from Inform; this sets whether to always print room
-- descriptions (No..) even if the room is visited, to only print them on the first
-- entry (Sometimes..) or never.
data RoomDescriptions = SometimesAbbreviatedRoomDescriptions
  | AbbreviatedRoomDescriptions
  | NoAbbreviatedRoomDescriptions 
  deriving stock (Eq, Show, Read, Ord, Enum, Generic)

-- | An 'Entity' is an integer ID that is used to reference between objects.
newtype Entity = Entity
  { unID :: Int
  } deriving stock   (Show, Generic)
    deriving newtype (Eq, Num, Read, Bounded, Hashable, Enum, Ord, Real, Integral)

-- | A way to extract an `Entity` from something.
class HasID n where
  getID :: n -> Entity

instance HasID Entity where
  getID = id

defaultVoidID :: Entity
defaultVoidID = Entity (-1)
-- | This is kind of hacky, but it works: a `Thing` has ID above 0, and a `Room` has a negative ID.
isThing ::
  (HasID a)
  => a
  -> Bool
isThing a = getID a >= 0

isRoom ::
  (HasID a)
  => a
  -> Bool
isRoom = not . isThing

-- | A 'Store' is a map from 'Entity's to @a@s.
newtype Store a = Store
  { unStore :: EM.EnumMap Entity a
  } deriving stock   (Show, Generic)
    deriving newtype (Eq, Ord, Read)

emptyStore :: Store a
emptyStore = Store EM.empty

-- | For now, a timestamp is simply an integer. The timestamp is updated whenever some
-- modification is made to the 'World'; therefore it does not directly correspond to
-- some sort of in-game turn counter. For example, throwing an object would result in
-- multiple timestamp jumps (an object moving, potential interactions on it hitting
-- something) whereas a sequence of 10 look actions will not (as the world does not
-- change). This is primarily used to ensure we can cache updates of objects that
-- change properties (e.g. strings).
newtype Timestamp = Timestamp
  { unTimestamp :: Int
  } deriving stock   (Show, Read, Generic)
    deriving newtype (Eq, Num, Enum, Ord, Real, Integral)

-- first let's define our own alterF for EnumMap...
alterEMF
  :: (Functor f, Enum k)
  => (Maybe a -> f (Maybe a))
  -> k
  -> EM.EnumMap k a -> f (EM.EnumMap k a)
alterEMF upd k m = EM.intMapToEnumMap <$> IM.alterF upd (fromEnum k) (EM.enumMapToIntMap m)

-- | alterF wrapper for Store, since it's a wrapper around a wrapper...
alterNewtypeEMF
  :: Functor f
  => Enum k
  => (Maybe a -> f (Maybe a))
  -> k
  -> (nt -> EM.EnumMap k a)
  -> (EM.EnumMap k a -> nt)
  -> nt
  -> f nt
alterNewtypeEMF upd k unwrap wrap' m = wrap' <$> alterEMF upd k (unwrap m)

instance At (Store a) where
  at k = lensVL $ \f -> alterNewtypeEMF f k unStore Store

type instance IxValue (Store a) = a
type instance Index (Store a) = Entity
instance Ixed (Store a)

-- | A WorldModel is the canonical implementation of the `wm` world model parameter.
-- the reason for its existence is primarily so we can avoid having massive type
-- sigs everywhere for `World`.
data WorldModel = WorldModel Type Type Type Type

type PlainWorldModel = 'WorldModel () () () ()

type family WMObjSpecifics (r :: WorldModel) :: Type where
  WMObjSpecifics ('WorldModel objSpec dir o v) = objSpec
type family WMValues (r :: WorldModel) :: Type where
  WMValues ('WorldModel objSpec dir o v) = o
type family WMDirections (r :: WorldModel) :: Type where
  WMDirections ('WorldModel objSpec dir o v) = dir 