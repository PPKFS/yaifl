

module Yaifl.Std.Kinds.Openable
  ( -- * Types
    Openability(..)
  , getOpenabilityMaybe
  , setOpenability
  , modifyOpenability
  , defaultDoorOpenability
  , defaultContainerOpenability
  , Openable(..)
  , Opened(..)
  , Locked(..)
  , Lockability(..)
  , isOpen
  , isClosed
  , openIt
  , closeIt
  ) where

import Yaifl.Prelude

import Yaifl.Core.Effects
import Yaifl.Core.Kinds.AnyObject
import Yaifl.Core.Kinds.Thing
import Yaifl.Core.Query.Property
import Yaifl.Core.TH
import Yaifl.Core.Entity

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

makeSpecificsWithout [] ''Openability
makeFieldLabelsNoPrefix ''Openability
makeSpecificsWithout [] ''Lockability
makeFieldLabelsNoPrefix ''Lockability

openIt ::
  NoMissingObjects wm es
  => WMWithProperty wm Openability
  => Thing wm
  -> Eff es ()
openIt = flip modifyOpenability (#opened .~ Open)

closeIt ::
  NoMissingObjects wm es
  => WMWithProperty wm Openability
  => Thing wm
  -> Eff es ()
closeIt = flip modifyOpenability (#opened .~ Closed)

defaultContainerOpenability :: Openability
defaultContainerOpenability = Openability { opened = Open, openable = NotOpenable, lockability = Nothing }

defaultDoorOpenability :: Openability
defaultDoorOpenability = Openability { opened = Closed, openable = Openable, lockability = Nothing }

isClosed ::
  WMWithProperty wm Openability
  => CanBeAny wm o
  => o
  -> Bool
isClosed o = Just Closed == (opened <$> getOpenabilityMaybe o)

isOpen ::
  WMWithProperty wm Openability
  => CanBeAny wm o
  => o
  -> Bool
isOpen o = Just Open == (opened <$> getOpenabilityMaybe o)

isLocked ::
  WMWithProperty wm Openability
  => CanBeAny wm o
  => o
  -> Bool
isLocked o =
  let mbOpen = getOpenabilityMaybe o
  in Just Locked == (mbOpen ^? _Just % #lockability % _Just % #locked)

-- | Notably, anything that is openable but isn't explicitly locked is unlocked
-- (even if it doesn't have a lockable part, when it's just always unlocked).
isUnlocked ::
  WMWithProperty wm Openability
  => CanBeAny wm o
  => o
  -> Bool
isUnlocked o =
  let mbOpen = getOpenabilityMaybe o
      mbLocked = mbOpen ^? _Just % #lockability % _Just % #locked
  in case mbOpen of
    Nothing -> False
    Just _ -> mbLocked /= Just Locked