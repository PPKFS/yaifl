module Yaifl.Combinators where

import Yaifl.Prelude
import Yaifl.Thing.Kind
import Yaifl.Object.Kind
import Yaifl.Object.Create
import Yaifl.Enclosing.Kind
import Yaifl.Openable.Kind
import Yaifl.Container.Kind
import Yaifl.Room.Kind

type ConfigCombinator s t sym val = LabelOptic sym A_Lens s t val val
type Modification s t x sym = ConfigCombinator s t sym (Eff '[State x] ())
type ThingModification wm s t = Modification s t (Thing wm) "thingModify"
type RoomModification wm s t = Modification s t (Room wm) "roomModify"

amendThingModify :: ThingModification wm s t => Eff '[State (Thing wm)] () -> s -> t
amendThingModify f = #thingModify %~ (>> f)

amendRoomModify :: RoomModification wm s t => Eff '[State (Room wm)] () -> s -> t
amendRoomModify f = #roomModify %~ (>> f)

makeThingImproperlyNamed ::
  WithLabel "nameProperness" NameProperness (Thing wm)
  => ThingModification wm s t
  => s
  -> t
makeThingImproperlyNamed = amendThingModify makeNameImproper

makeThingProperlyNamed ::
  WithLabel "nameProperness" NameProperness (Thing wm)
  => ThingModification wm s t
  => s
  -> t
makeThingProperlyNamed = amendThingModify makeNameProper

makeRoomImproperlyNamed ::
  WithLabel "nameProperness" NameProperness (Room wm)
  => RoomModification wm s t
  => s
  -> t
makeRoomImproperlyNamed = amendRoomModify makeNameImproper

makeRoomProperlyNamed ::
  WithLabel "nameProperness" NameProperness (Room wm)
  => RoomModification wm s t
  => s
  -> t
makeRoomProperlyNamed = amendRoomModify makeNameProper
placeIt ::
  ConfigCombinator s t "location" (Maybe EnclosingEntity)
  => EnclosingEntity
  -> s
  -> t
placeIt e = #location ?~ e

makeItClosedAndOpenable ::
  ConfigCombinator s t "openStatus" (Opened, Openable)
  => s
  -> t
makeItClosedAndOpenable = #openStatus .~ (Closed, Openable)

makeItEnterable ::
  ConfigCombinator s t "enterable" Enterable
  => s
  -> t
makeItEnterable = #enterable .~ Enterable

makeItTransparent ::
  ConfigCombinator s t "opacity" Opacity
  => s
  -> t
makeItTransparent = #opacity .~ Transparent

makeItPlural ::
  ThingModification wm s t
  => s
  -> t
makeItPlural = amendThingModify $ #namePlurality .= PluralNamed

makeItScenery ::
  ThingModification wm s t
  => s
  -> t
makeItScenery = amendThingModify $ #objectData % #isScenery .= True