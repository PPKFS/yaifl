module Yaifl.Thing.Create
  ( makeItScenery
  , addThing

  ) where

import Yaifl.Prelude

import Yaifl.Entity
import Yaifl.Object.Kind
import Yaifl.WorldModel

import Yaifl.Effects.ObjectQuery
import Yaifl.Object.Query
import Yaifl.Thing.Kind
import Yaifl.Enclosing.Kind ( Enclosing )

import Yaifl.Property.Has
import Yaifl.Object.Create

addThingInternal ::
  WMWithProperty wm Enclosing
  => AddObjects wm es
  => WMText wm -- ^ Name.
  -> WMText wm
  -> WMText wm -- ^ Description.
  -> ObjectKind -- ^ Type.
  -> Maybe (WMObjSpecifics wm)
  -> Maybe (ThingData wm)
  -> Maybe EnclosingEntity
  -> Eff es ThingEntity
addThingInternal name ia desc objtype specifics details mbLoc = do
  t <- Thing <$> addObject (setThing . Thing) name desc objtype
        True specifics (fromMaybe (blankThingData ia) details) mbLoc
  pure (tagThingEntity t)

addThing ::
  forall wm es.
  WMWithProperty wm Enclosing
  => AddObjects wm es
  => WMText wm -- ^ Name.
  -> "initialAppearance" :? WMText wm
  -> "description" :? WMText wm -- ^ Description.
  -> "specifics" :? WMObjSpecifics wm
  -> "modify" :? Eff '[State (Thing wm)] () -- ^ Build your own thing monad!
  -> "location" :? EnclosingEntity
  -> "type" :? ObjectKind
  -> "thingData" :? ThingData wm
  -> "portable" :? ThingPortable
  -> Eff es ThingEntity
addThing n
  (argDef #initialAppearance "" -> ia)
  (argDef #description "" -> d)
  (argF #specifics -> s)
  (argF #modify -> stateUpdate)
  (argF #location -> loc)
  (argDef #type (ObjectKind "thing") -> ki)
  (argDef #thingData (blankThingData ia) -> td)
  (argF #portable -> p) = do
    let td' = td & (#portable %~ maybe id const p)
    t <- addThingInternal n ia d ki s (Just td') loc
    whenJust stateUpdate $ \su -> failHorriblyIfMissing $ modifyThing t (`runLocalState` su)
    pure t

makeItScenery :: Eff '[State (Thing wm)] ()
makeItScenery = #objectData % #isScenery .= True