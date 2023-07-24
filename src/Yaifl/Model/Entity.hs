{-# LANGUAGE TypeFamilies #-}
{-|
Module      : Yaifl.Model.Entity
Description : Object IDs and stores.
Copyright   : (c) Avery 2022-2023
License     : MIT
Maintainer  : ppkfs@outlook.com

Object IDs and wrappers around `Map`s indexed by `Entity`.
-}

module Yaifl.Model.Entity
  ( -- * Entities
    Entity(..)
  , HasID(..)
    -- ** Special IDs
    --
    -- | There are a handful of special IDs for objects which are created at build time and we *assume* the library user
    -- doesn't fiddle with them. This is much simpler than having them be entries in `Metadata`.
  , voidID
  , defaultPlayerID
  , nothingID
    -- * Stores
  , Store(..)
  , emptyStore
  ) where

import Solitude

import qualified Data.EnumMap as EM
import qualified Data.IntMap as IM
import Formatting.Buildable ( Buildable(..) )
import Data.Text.Display ( Display(..) )

-- | An object ID. If something has an ID of 0, it's an error.
newtype Entity = Entity
  { unID :: Int
  } deriving stock (Show, Generic)
    deriving newtype (Eq, Num, Read, Bounded, Hashable, Enum, Ord, Real, Integral)

-- | Lens-y typeclass for extracting Entities from something.
class HasID n where
  -- | Get an ID.
  getID :: n -> Entity

-- | Trivial instance.
instance HasID Entity where
  getID = id

-- | For pretty printing in logs.
instance Buildable Entity where
  build (Entity i) = "(ID: " <> show i <> ")"

instance Display Entity where
  displayBuilder = build

-- | A place where new `Yaifl.Model.Objects.Thing`s are placed by default, to avoid having locations be `Maybe`.
voidID :: Entity
voidID = Entity (-1)

-- | An error object.
nothingID :: Entity
nothingID = Entity 0

-- | The player who is created at the start of the game. This can change (whereas e.g. the Void changing makes no
-- sense) which is why this is named slightly differently.
defaultPlayerID :: Entity
defaultPlayerID = Entity 1

---- Stores ----

-- | An `EnumMap` specialised over `Entity`s.
newtype Store a = Store
  { unStore :: EM.EnumMap Entity a
  } deriving stock   (Show, Generic)
    deriving newtype (Eq, Ord, Read, Foldable)

-- | A store with no items.
emptyStore :: Store a
emptyStore = Store EM.empty

alterEMF ::
  (Functor f, Enum k)
  => (Maybe a -> f (Maybe a))
  -> k
  -> EM.EnumMap k a
  -> f (EM.EnumMap k a)
alterEMF upd k m = EM.intMapToEnumMap <$> IM.alterF upd (fromEnum k) (EM.enumMapToIntMap m)

alterNewtypeEMF ::
  (Functor f, Enum k)
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