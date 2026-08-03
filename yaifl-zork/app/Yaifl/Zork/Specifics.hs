{-# LANGUAGE RecordWildCards #-}
module Yaifl.Zork.Specifics where

import Yaifl.Prelude

import Yaifl.Actions.Imports
import Yaifl
import Yaifl.Text.DynamicText (DynamicText)
import Yaifl.ObjectSpecifics
import Yaifl.Direction.Kind
import Yaifl.Text.ResponseCollection (ResponseCollection)
import Yaifl.Zork.Scoring
import Yaifl.Zork.Metadata
import Yaifl.Person.Kind
import Yaifl.Property.Has
import Yaifl.Person.Create
import Yaifl.Entity
import Yaifl.Object.Create
import Yaifl.Object.Kind
import Yaifl.Zork.Actions

data Defeated = Defeated | NotDefeated
data ZorkSpecifics = YaiflSpecifics (ObjectSpecifics) | ExtendedPersonSpecifics (Defeated, Person)
  deriving stock (Generic)

instance Pointed ZorkSpecifics where
  identityElement = YaiflSpecifics identityElement

instance MayHaveProperty ObjectSpecifics v => MayHaveProperty ZorkSpecifics v where
  propertyAT = #_YaiflSpecifics % propertyAT


addZorkPerson ::
  forall wm es.
  (WMObjSpecifics wm ~ ZorkSpecifics)
  => AddObjects wm es
  => WMText wm -- ^ Name.
  -> PersonConfig wm
  -> Eff es ThingEntity
addZorkPerson name config@PersonConfig{..} = addPerson name $ config & #thingModify .~ (#specifics .= (ExtendedPersonSpecifics (NotDefeated, ((Person gender carrying)))))


type ZorkWorldModel = 'WorldModel ZorkSpecifics Direction ZorkData ZorkThingData () () ActivityCollection ResponseCollection DynamicText ZorkActions

instance WMHasObjSpecifics ZorkWorldModel where
  inj _ = YaiflSpecifics