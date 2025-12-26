module Yaifl.Person.Create
  ( addPerson

  ) where

import Yaifl.Prelude

import Yaifl.Entity
import Yaifl.Object.Kind
import Yaifl.Object.Create
import Yaifl.Thing.Kind
import Yaifl.Enclosing.Kind ( Enclosing (..) )
import Yaifl.Property.Has ( WMWithProperty )
import Yaifl.Person.Kind
import Yaifl.WorldModel
import Yaifl.ObjectSpecifics
import Yaifl.Thing.Create

addPerson ::
  forall wm es.
  WMHasObjSpecifics wm
  => WMWithProperty wm Enclosing
  => AddObjects wm es
  => WMText wm -- ^ Name.
  -> "gender" :! Gender
  -> "initialAppearance" :? WMText wm
  -> "description" :? WMText wm -- ^ Description.
  -> "carrying" :? Enclosing
  -> "modify" :? Eff '[State (Thing wm)] ()
  -> Eff es ThingEntity
addPerson n (Arg g) ia d (argF #carrying -> e) (argF #modify -> m) = addThing @wm n ia d
  ! #specifics (inj (Proxy @wm) (PersonSpecifics (Person g (fromMaybe defaultPersonEnclosing e))))
  ! #type (case g of
    Male -> ObjectKind "man"
    Female -> ObjectKind "woman"
    NonBinary -> ObjectKind "person"
    Other _ -> ObjectKind "person")
  ! paramF #modify m
  ! done