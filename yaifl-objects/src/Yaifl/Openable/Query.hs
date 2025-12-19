module Yaifl.Openable.Query
  ( setOpenability
  , modifyOpenability

  , openIt
  , closeIt
  ) where

import Yaifl.Prelude

import Yaifl.Effects.ObjectQuery
import Yaifl.AnyObject
import Yaifl.Thing.Kind
import Yaifl.Core.Query.Property
import Yaifl.TH
import Yaifl.Entity