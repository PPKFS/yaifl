module Yaifl.Model.Kinds.Device where

import Solitude
import Yaifl.Model.TH (makeSpecificsWithout)
import Yaifl.Model.Kinds.AnyObject
import Yaifl.Model.Effects
import Yaifl.Model.HasProperty
import Yaifl.Model.Query
import Yaifl.Model.Kinds.Object

newtype Device = Device
  { switchedOn :: Bool
  } deriving stock (Eq, Ord, Show, Generic, Read)

instance Pointed Device where
  identityElement = Device False

makeFieldLabelsNoPrefix ''Device
makeSpecificsWithout [] ''Device