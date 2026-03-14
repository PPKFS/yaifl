module Yaifl.Openable.Query
  ( setOpenability
  , modifyOpenability
  , setLockability
  , modifyLockability

  , openIt
  , closeIt
  ) where

import Yaifl.Prelude

import Yaifl.Openable.Kind
import Yaifl.Effects.ObjectQuery
import Yaifl.AnyObject
import Yaifl.Thing.Kind
import Yaifl.Property.Query
import Yaifl.TH

makeModify ''Lockability
makeModify ''Openability

openIt ::
  WithoutMissingObjects wm es
  => WMWithProperty wm Openability
  => Thing wm
  -> Eff es ()
openIt = flip modifyOpenability (#opened .~ Open)

closeIt ::
  WithoutMissingObjects wm es
  => WMWithProperty wm Openability
  => Thing wm
  -> Eff es ()
closeIt = flip modifyOpenability (#opened .~ Closed)