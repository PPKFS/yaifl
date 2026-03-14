module Yaifl.Openable.Kind
  ( -- * Types
    Openability(..)
  , defaultDoorOpenability
  , defaultContainerOpenability
  , Openable(..)
  , Opened(..)
  , Locked(..)
  , Lockability(..)
  , CouldBeOpened(..)

  , getOpenabilityMaybe
  , getLockabilityMaybe
  , isOpen
  , isClosed
  , isLocked
  , isUnlocked
  ) where

import Yaifl.Prelude
import Yaifl.TH
import Yaifl.Entity

-- | Whether the thing is open or not.
data Opened = Open | Closed
  deriving stock (Eq, Show, Read, Ord, Generic)

-- | Whether the thing is openable or not.
data Openable = Openable | NotOpenable
  deriving stock (Eq, Show, Read, Ord, Generic)

-- | Whether the thing is locked or not.
data Locked = Locked | Unlocked
  deriving stock (Eq, Show, Read, Ord, Generic)


data Lockability = Lockability
  { locked :: Locked
  , matchingKey :: Maybe ThingEntity
  } deriving stock (Eq, Show, Read, Ord, Generic)

data Openability = Openability
  { opened :: Opened
  , openable :: Openable
  , lockability :: Maybe Lockability -- ^ Nothing = not lockable
  } deriving stock (Eq, Show, Read, Ord, Generic)

makeFieldLabelsNoPrefix ''Openability
makeFieldLabelsNoPrefix ''Lockability

makeGetMaybe ''Openability
makeGetMaybe ''Lockability

defaultContainerOpenability :: Openability
defaultContainerOpenability = Openability { opened = Open, openable = NotOpenable, lockability = Nothing }

defaultDoorOpenability :: Openability
defaultDoorOpenability = Openability { opened = Closed, openable = Openable, lockability = Nothing }

-- | Not quite a Has typeclass - however we have sensible semantics for the @Nothing@ case. Thus,
-- this constraint should be read as "an @o@ has the possibility of being opened", where something that
-- could not be opened will always be `False`.

class CouldBeOpened o where
  hasOpenability :: o -> Maybe Openability

instance {-# OVERLAPPABLE #-} (WMWithProperty wm Openability , CanBeAny wm o) => CouldBeOpened o  where
  hasOpenability = getOpenabilityMaybe

instance CouldBeOpened Openability where
  hasOpenability = Just

isClosed ::
  CouldBeOpened o
  => o
  -> Bool
isClosed o = Just Closed == (opened <$> hasOpenability o)

isOpen ::
  CouldBeOpened o
  => o
  -> Bool
isOpen o = Just Open == (opened <$> hasOpenability o)

isLocked ::
  CouldBeOpened o
  => o
  -> Bool
isLocked o =
  let mbOpen = hasOpenability o
  in Just Locked == (mbOpen ^? _Just % #lockability % _Just % #locked)

-- | Notably, anything that is openable but isn't explicitly locked is unlocked
-- (even if it doesn't have a lockable part, when it's just always unlocked).
isUnlocked ::
  CouldBeOpened o
  => o
  -> Bool
isUnlocked o =
  let mbOpen = hasOpenability o
      mbLocked = mbOpen ^? _Just % #lockability % _Just % #locked
  in case mbOpen of
    Nothing -> False
    Just _ -> mbLocked /= Just Locked