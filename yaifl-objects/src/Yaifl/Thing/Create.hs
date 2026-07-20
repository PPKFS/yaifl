{-# LANGUAGE RecordWildCards #-}
module Yaifl.Thing.Create
  ( addThing
  , ThingConfig(..)
  , newThing
  ) where

import Yaifl.Prelude

import Yaifl.Builder
import Yaifl.Effects.ObjectQuery
import Yaifl.Entity
import Yaifl.Object.Create
import Yaifl.Object.Kind
import Yaifl.Object.Query
import Yaifl.Thing.Kind
import Yaifl.WorldModel

data ThingConfig wm p = ThingConfig
  { description :: WMText wm
  , specifics :: WMObjSpecifics wm
  , initialAppearance :: WMText wm
  , thingModify :: Eff '[State (Thing wm)] ()
  , location :: Maybe EnclosingEntity
  , objType :: ObjectKind
  , thingData :: ThingData wm
  } deriving stock (Generic)

newThing :: forall wm. (Pointed (WMObjSpecifics wm), IsString (WMText wm), Pointed (WMThingData wm)) => ThingConfig wm 'Complete
newThing = ThingConfig
  { description = ""
  , initialAppearance = ""
  , thingModify = pass
  , location = Nothing
  , specifics = identityElement @(WMObjSpecifics wm)
  , objType = ObjectKind "thing"
  , thingData = blankThingData ""
  }

addThingInternal ::
  AddObjects wm es
  => WMText wm -- ^ Name.
  -> WMText wm -- ^ Description.
  -> ObjectKind -- ^ Type.
  -> WMObjSpecifics wm
  -> ThingData wm
  -> Maybe EnclosingEntity
  -> Eff es ThingEntity
addThingInternal name desc objtype specifics details mbLoc = do
  t <- Thing <$> addObject (setThing . Thing) name desc objtype True (Just specifics) details mbLoc
  pure (tagThingEntity t)

addThing ::
  forall wm es.
  AddObjects wm es
  => WMText wm
  -> ThingConfig wm 'Complete
  -> Eff es ThingEntity
addThing n ThingConfig{..} = do
    let td' = thingData & #initialAppearance .~ initialAppearance
    t <- addThingInternal n description objType specifics td' location
    failHorriblyIfMissing $ modifyThing t (`runLocalState` thingModify)
    pure t