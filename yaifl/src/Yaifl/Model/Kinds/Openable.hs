

module Yaifl.Model.Kinds.Openable
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

import Solitude

import Yaifl.Model.Effects
import Yaifl.Model.HasProperty
import Yaifl.Model.Query
import Yaifl.Model.TH
import Yaifl.Model.Kinds.AnyObject
import Yaifl.Model.Kinds.Thing

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
closeIt = flip modifyOpenability (#opened .~ Open)

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