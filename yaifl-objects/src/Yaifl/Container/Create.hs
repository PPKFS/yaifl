module Yaifl.Container.Create
  ( addContainer

  ) where

import Yaifl.Prelude

import Yaifl.Entity
import Yaifl.Object.Kind
import Yaifl.Object.Create
import Yaifl.Thing.Kind
import Yaifl.Container.Kind
import Yaifl.Enclosing.Kind ( Enclosing (..) )
import Yaifl.Property.Has ( WMWithProperty )
import Yaifl.Openable.Kind
import Yaifl.Tag
import Yaifl.WorldModel
import Yaifl.ObjectSpecifics
import Yaifl.Thing.Create

addContainer ::
  forall wm es.
  WMHasObjSpecifics wm
  => WMWithProperty wm Enclosing
  => AddObjects wm es
  => WMText wm -- ^ Name.
  -> "initialAppearance" :? WMText wm
  -> "description" :? WMText wm
  -> "carryingCapacity" :? Int
  -> "opacity" :? Opacity
  -> "enterable" :? Enterable
  -> "openable" :? Openable
  -> "opened" :? Opened
  -> "location" :? EnclosingEntity
  -> "portable" :? ThingPortable
  -> "modify" :? Eff '[State (Thing wm)] ()
  -> Eff es ContainerEntity
addContainer n ia d
  (argF #carryingCapacity -> cc)
  (argF #opacity -> op)
  (argF #enterable -> e)
  (argF #openable -> o)
  (argF #opened -> od)
  (argF #location -> l)
  (argF #portable -> p)
  (argF #modify -> m) = do
    let cs = makeContainer cc op e o od
    c <- addThing @wm n ia d
        ! #specifics (inj (Proxy @wm) $ ContainerSpecifics cs)
        ! #type (ObjectKind "container")
        ! paramF #location l
        ! paramF #portable p
        ! paramF #modify m
        ! done
    pure $ tagEntity @Container @ContainerTag cs c