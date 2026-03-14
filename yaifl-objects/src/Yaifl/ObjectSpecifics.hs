module Yaifl.ObjectSpecifics
  ( -- * Specifics
  ObjectSpecifics(..)
  , WMHasObjSpecifics(..)
  ) where

import Yaifl.Prelude

import Yaifl.Container.Kind
import Yaifl.Door.Kind
import Yaifl.Enclosing.Kind ( Enclosing (..) )
import Yaifl.Property.Has ( MayHaveProperty(..) )
import Yaifl.MultiLocated.Kind
import Yaifl.Openable.Kind
import Yaifl.Device.Kind
import Yaifl.Person.Kind
import Yaifl.Supporter.Kind
import Yaifl.Backdrop.Kind
import Yaifl.WorldModel

data ObjectSpecifics =
  NoSpecifics
  | EnclosingSpecifics Enclosing
  | ContainerSpecifics Container
  | OpenabilitySpecifics Openability
  | DoorSpecifics Door
  | DeviceSpecifics Device
  | PersonSpecifics Person
  | SupporterSpecifics Supporter
  | BackdropSpecifics Backdrop
  deriving stock (Eq, Show, Read, Generic)

makePrisms ''ObjectSpecifics

instance Pointed ObjectSpecifics where
  identityElement = NoSpecifics

class WMHasObjSpecifics (wm :: WorldModel) where
  inj :: Proxy wm -> ObjectSpecifics -> WMObjSpecifics wm

instance WMHasObjSpecifics ('WorldModel ObjectSpecifics a b c x e ac r se act) where
  inj _ = id

instance MayHaveProperty ObjectSpecifics Enclosing where
  propertyAT = _EnclosingSpecifics
    `thenATraverse` (_PersonSpecifics % #carrying)
    `thenATraverse` (_ContainerSpecifics % #enclosing)
    `thenATraverse` (_SupporterSpecifics % #enclosing)

instance MayHaveProperty ObjectSpecifics MultiLocated where
  propertyAT = _DoorSpecifics % #multiLocated
    `thenATraverse` (_BackdropSpecifics % coerced @_ @MultiLocated)

instance MayHaveProperty ObjectSpecifics Container where
  propertyAT = castOptic _ContainerSpecifics

instance MayHaveProperty ObjectSpecifics Enterable where
  propertyAT = _ContainerSpecifics % #enterable

instance MayHaveProperty ObjectSpecifics Openability where
  propertyAT = _OpenabilitySpecifics
    `thenATraverse` (_ContainerSpecifics % #openable)
    `thenATraverse` (_DoorSpecifics % #opened)

instance MayHaveProperty ObjectSpecifics Door where
  propertyAT = castOptic _DoorSpecifics

instance MayHaveProperty ObjectSpecifics Device where
  propertyAT = castOptic _DeviceSpecifics

instance MayHaveProperty ObjectSpecifics Person where
  propertyAT = castOptic _PersonSpecifics

instance MayHaveProperty ObjectSpecifics Supporter where
  propertyAT = castOptic _SupporterSpecifics

instance MayHaveProperty ObjectSpecifics Backdrop where
  propertyAT = castOptic _BackdropSpecifics
