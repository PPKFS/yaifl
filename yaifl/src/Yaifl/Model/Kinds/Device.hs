module Yaifl.Model.Kinds.Device where

import Yaifl.Prelude
import Yaifl.Model.TH (makeSpecificsWithout)
import Yaifl.Model.Kinds.AnyObject
import Yaifl.Model.Effects
import Yaifl.Model.HasProperty
import Yaifl.Model.Query
import Yaifl.Model.Kinds.Object
import Yaifl.Model.Kinds.Thing

newtype Device = Device
  { switchedOn :: Bool
  } deriving newtype (Show)
    deriving stock (Eq, Ord, Generic, Read)

instance Pointed Device where
  identityElement = Device False

makeFieldLabelsNoPrefix ''Device
makeSpecificsWithout [] ''Device

switchItOn ::
  NoMissingObjects wm es
  => WMWithProperty wm Device
  => Thing wm
  -> Eff es ()
switchItOn = flip modifyDevice (#switchedOn .~ True)