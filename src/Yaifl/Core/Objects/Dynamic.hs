{-|
Module      : Yaifl.Core.Objects.Dynamic
Description : Objects which can update themselves.
Copyright   : (c) Avery 2022
License     : MIT
Maintainer  : ppkfs@outlook.com

This is interpretation-specific in that we never interact with cached objects outside of the interpreters
for object querying, but it's useful here.
-}
module Yaifl.Core.Objects.Dynamic (
  -- * Dynamic objects
    TimestampedObject(..)
  , ObjectUpdateFunc(..)
  , AbstractObject(..)
  ) where

import Yaifl.Core.Entity (HasID(..))
import Yaifl.Core.Metadata (Timestamp)
import Yaifl.Core.Object (Object)
import Yaifl.Core.WorldModel (WorldModel)
import Solitude
import Effectful

-- | An object that has been cached at time `_tsCacheStamp`.
data TimestampedObject wm d = TimestampedObject
  { _tsCachedObject :: !(Object wm d)
  , _tsCacheStamp :: !Timestamp
  , _tsUpdateFunc :: ObjectUpdateFunc wm d
  } deriving stock (Generic)

instance HasID (TimestampedObject wm d) where
  getID (TimestampedObject o _ _) = getID o

-- TODO: add more constraints
-- | Function to update an object. It is read-only on the world; i.e. it can only modify itself
newtype ObjectUpdateFunc (wm :: WorldModel) d = ObjectUpdateFunc
  { runObjectUpdate :: forall es. Object wm d -> Eff es (Object wm d)
  }

-- | An object as we store them.
data AbstractObject wm d
  = DynamicObject (TimestampedObject wm d)
  | StaticObject (Object wm d)

instance Buildable (AbstractObject wm d) where
  build = const "blah"

instance HasID (AbstractObject wm d) where
  getID (StaticObject o) = getID o
  getID (DynamicObject ts) = getID ts