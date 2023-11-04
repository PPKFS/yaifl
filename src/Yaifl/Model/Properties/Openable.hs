{-# LANGUAGE TemplateHaskell #-}

module Yaifl.Model.Properties.Openable
  ( -- * Types
    Openable(..)
  , getOpenable
  , isClosed
  , isOpen
  ) where


import Solitude

import Yaifl.Model.Objects.Query
import Yaifl.Model.Properties.Has
import Yaifl.Model.Properties.Query
import Yaifl.Model.Properties.TH
import Yaifl.Model.Objects.Effects

-- | Whether the thing is open or not.
data Openable = Open | Closed
  deriving stock (Eq, Show, Read, Ord, Generic)

makeSpecificsWithout [] ''Openable

isClosed ::
  NoMissingRead wm es
  => WMHasProperty wm Openable
  => ObjectLike wm o
  => o
  -> Eff es Bool
isClosed o = (Just Closed ==) <$> getOpenable o

isOpen ::
  NoMissingRead wm es
  => WMHasProperty wm Openable
  => ObjectLike wm o
  => o
  -> Eff es Bool
isOpen o = (Just Open ==) <$> getOpenable o