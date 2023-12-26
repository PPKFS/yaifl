

module Yaifl.Model.Properties.Openable
  ( -- * Types
    Openability(..)
  , getOpenabilityMaybe
  , setOpenability
  , modifyOpenability
  , defaultDoorOpenability
  , defaultContainerOpenability
  , Openable(..)
  , Opened(..)
  , openIt
  , closeIt
  ) where

import Solitude

import Yaifl.Model.Object
import Yaifl.Model.Objects.Effects
import Yaifl.Model.Properties.Has
import Yaifl.Model.Properties.Query
import Yaifl.Model.Properties.TH

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