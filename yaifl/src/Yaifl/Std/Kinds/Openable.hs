

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
import Yaifl.Core.WorldModel

-- | Whether the thing is open or not.
data Opened = Open | Closed
  deriving stock (Eq, Show, Read, Ord, Generic)

-- | Whether the thing is openable or not.
data Openable = Openable | NotOpenable
  deriving stock (Eq, Show, Read, Ord, Generic)

data Openability = Openability
  { opened :: Opened
  , openable :: Openable
  } deriving stock (Eq, Show, Read, Ord, Generic)

makeSpecificsWithout [] ''Openability
makeFieldLabelsNoPrefix ''Openability

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
defaultContainerOpenability = Openability Open NotOpenable

defaultDoorOpenability :: Openability
defaultDoorOpenability = Openability Closed Openable

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