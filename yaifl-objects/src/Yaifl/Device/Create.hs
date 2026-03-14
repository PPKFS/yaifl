module Yaifl.Device.Create
  ( addDevice

  ) where

import Yaifl.Prelude

import Yaifl.Entity
import Yaifl.Object.Kind
import Yaifl.Object.Create
import Yaifl.Enclosing.Kind ( Enclosing (..) )
import Yaifl.Property.Has ( WMWithProperty )
import Yaifl.Device.Kind
import Yaifl.WorldModel
import Yaifl.ObjectSpecifics
import Yaifl.Thing.Create

addDevice ::
  forall wm es.
  WMHasObjSpecifics wm
  => WMWithProperty wm Enclosing
  => AddObjects wm es
  => WMText wm -- ^ Name.
  -> "initialAppearance" :? WMText wm
  -> "description" :? WMText wm -- ^ Description.
  -> "device" :? Device
  -> Eff es ThingEntity
addDevice n ia d (argDef #device identityElement -> dev) = addThing @wm n ia d
  ! #specifics (inj (Proxy @wm) (DeviceSpecifics dev))
  ! #type (ObjectKind "device")
  ! done