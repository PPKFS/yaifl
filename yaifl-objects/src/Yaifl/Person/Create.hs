{-# LANGUAGE RecordWildCards #-}
module Yaifl.Person.Create
  ( addPerson
  , PersonConfig(..)
  , newPerson

  ) where

import Yaifl.Prelude

import Yaifl.Entity
import Yaifl.Object.Kind
import Yaifl.Object.Create
import Yaifl.Thing.Kind
import Yaifl.Enclosing.Kind ( Enclosing (..) )
import Yaifl.Person.Kind
import Yaifl.WorldModel
import Yaifl.ObjectSpecifics
import Yaifl.Thing.Create

data PersonConfig wm = PersonConfig
  { description :: WMText wm
  , initialAppearance :: WMText wm
  , gender :: Gender
  , carrying :: Enclosing
  , thingModify :: Eff '[State (Thing wm)] ()
  } deriving stock (Generic)

newPerson :: IsString (WMText wm) => Gender -> PersonConfig wm
newPerson g = PersonConfig
  { description = ""
  , initialAppearance = ""
  , gender = g
  , carrying = defaultPersonEnclosing
  , thingModify = pass
  }

addPerson ::
  forall wm es.
  AddObjects wm es
  => WMText wm -- ^ Name.
  -> PersonConfig wm
  -> Eff es ThingEntity
addPerson n PersonConfig{..} = addThing @wm n newThing
  { initialAppearance
  , description
  , specifics = inj (Proxy @wm) (PersonSpecifics (Person gender carrying))
  , objType = case gender of
      Male -> ObjectKind "man"
      Female -> ObjectKind "woman"
      NonBinary -> ObjectKind "person"
      Other _ -> ObjectKind "person"
  , thingModify
  }