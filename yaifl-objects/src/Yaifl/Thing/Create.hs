{-# LANGUAGE RecordWildCards #-}
module Yaifl.Thing.Create
  ( addThing
  , ThingConfig(..)
  , newThing
  , replaceObject
  ) where

import Yaifl.Prelude

import Yaifl.Builder
import Yaifl.Effects.ObjectQuery
import Yaifl.Entity
import Yaifl.Object.Create
import Yaifl.Object.Kind
import Yaifl.Object.Query
import Yaifl.ObjectLike (ThingLike(..))
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


-- | Replace one object with another in the world.
-- This modifies the old object in place to have the new configuration,
-- preserving the entity ID so that references remain valid.
replaceObject ::
  WithoutMissingObjects wm es
  => ThingLike wm oldObj
  => WMText wm
  -> ThingConfig wm 'Complete
  -> oldObj
  -> Eff es ThingEntity
replaceObject newName newConfig oldObj = do
  modifyThing oldObj $ \oldThingObj ->
    let ThingConfig{description = configDesc, initialAppearance = configInitApp,
                    specifics = configSpecs, thingData = configThingData} = newConfig
        td' = configThingData & #initialAppearance .~ configInitApp
    in oldThingObj
      & #name .~ newName
      & #description .~ configDesc
      & #objectType .~ objType newConfig
      & #specifics .~ configSpecs
      & #objectData .~ td'
  pure $ unsafeTagEntity (getEntity oldObj)
