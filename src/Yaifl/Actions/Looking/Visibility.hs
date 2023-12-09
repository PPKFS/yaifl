{-# LANGUAGE StrictData #-}

module Yaifl.Actions.Looking.Visibility where

import Solitude

import Breadcrumbs ( addAnnotation )
import Yaifl.Activities.Activity (WithPrintingNameOfADarkRoom, WithPrintingDescriptionOfADarkRoom)
import Yaifl.Model.Object
import Yaifl.Model.Objects.Query
import Yaifl.Model.Objects.RoomData
import Yaifl.Model.Objects.ThingData
import Yaifl.Model.Properties.Enclosing
import Yaifl.Model.Properties.Has
import Yaifl.Model.Properties.Query
import Yaifl.Rules.Args
import Yaifl.Model.Properties.Container
import Yaifl.Model.Properties.Openable
import Yaifl.Model.Properties.Supporter
import qualified Data.EnumSet as DES
import Data.Text.Display
import Yaifl.Model.WorldModel
import Yaifl.Text.Responses
import Yaifl.Activities.PrintingTheLocaleDescription
import Yaifl.Model.Objects.Effects
import Yaifl.Model.Objects.ObjectLike

-- | An easier way to describe the 3 requirements to look.
type HasLookingProperties wm =
  ( WMWithProperty wm Enclosing
  , WMWithProperty wm Enterable
  , WMWithProperty wm Container
  , Display (WMSayable wm)
  , IsString (WMSayable wm)
  , WithPrintingNameOfADarkRoom wm
  , WithPrintingTheLocaleDescription wm
  , WithPrintingDescriptionOfADarkRoom wm
  , WithResponseSet wm "roomDescriptions" (RoomDescriptionResponses wm))

data RoomDescriptionResponses wm = RDR
  { roomDescriptionHeadingA :: Response wm ()
  , roomDescriptionHeadingB :: Response wm (AnyObject wm)
  , roomDescriptionHeadingC :: Response wm (AnyObject wm)
  , roomDescriptionBodyA :: Response wm ()
  } deriving stock (Generic)

data LookingActionVariables wm = LookingActionVariables
  { lookingFrom :: AnyObject wm
  -- The looking action has an object called the visibility ceiling. (this is the last element).
  , visibilityLevels :: [AnyObject wm]
  -- The looking action has an action name called the room-describing action.
  , roomDescribingAction :: Text
  }
  deriving stock (Eq, Generic)

visibilityCount ::
  LookingActionVariables wm
  -> Int
visibilityCount = length . view #visibilityLevels

instance Refreshable wm (LookingActionVariables wm) where
  refreshVariables av = do
    lf <- getObject (lookingFrom av)
    vls <- mapM getObject (visibilityLevels av)
    return $ av { lookingFrom = lf, visibilityLevels = vls }

getVisibilityLevels ::
  NoMissingObjects wm es
  => HasLookingProperties wm
  => AnyObject wm
  -> Eff es [AnyObject wm]
getVisibilityLevels e = do
  vh <- findVisibilityHolder e
  if vh `objectEquals` e
      then return [e]
      else (vh :) <$> getVisibilityLevels vh

-- | the visibility holder of a room or an opaque, closed container is itself; otherwise, the enclosing entity
findVisibilityHolder ::
  NoMissingObjects wm es
  => HasLookingProperties wm
  => AnyObject wm
  -> Eff es (AnyObject wm)
findVisibilityHolder obj = do
  let mCont = getContainerMaybe obj
  let n = obj ^. #name
  case (tagObject obj, isOpaqueClosedContainer <$?> mCont) of
    -- a nonopaque or open container thing
    (Left thingTag, False) -> do
      t <- getThing thingTag
      getObject (t ^. #objectData % #containedBy)
    _ -> do
      addAnnotation $ "The visibility holder of " <> display n <> " is itself"
      --return it
      return obj

-- Inform Designer's Manual, Page 146
-- we recalculate the light of the immediate holder of an object
-- there is light exactly when the parent (p) "offers light"
-- has light is if it's lit, or see through and it contains light
-- offers light means it lights INTO itself
-- has light means it lights OUT AWAY from itself
recalculateLightOfParent ::
  NoMissingObjects wm es
  => HasLookingProperties wm
  => CanBeAny wm o
  => o
  -> Eff es Int
recalculateLightOfParent = asThingOrRoom
  (\t -> do
    parent <- getObject (t ^. #objectData % #containedBy)
    ol <- offersLight parent
    if
      ol
    then
      (1+) <$> recalculateLightOfParent parent
    else
      return 0
  )
  (const $ return 0) . toAny

-- | An object offers light if:
-- - it is a lit thing (lit thing or lighted room)
-- - it contains a thing that has light
-- - it is see-through (an object) and its parent offers light
-- this goes DOWN the object tree; the light goes to its contents
offersLight ::
  NoMissingObjects wm es
  => HasLookingProperties wm
  => AnyObject wm
  -> Eff es Bool
offersLight obj = do
  let parentOffersLight o = getObject (o ^. #objectData % #containedBy) >>= offersLight
      seeThruWithParent = maybe (return False) (\o' -> isSeeThrough o' &&^ parentOffersLight o')
  t <- getThingMaybe obj

  pure (objectItselfHasLight obj) -- it is a lit thing (lit thing or lighted room)
    ||^ seeThruWithParent t -- - it is see-through thing and its parent offers light
    ||^ containsLitObj obj -- - it contains a thing that has light

-- | an object is see through if...
isSeeThrough ::
  NoMissingObjects wm es
  => HasLookingProperties wm
  => Thing wm
  -> Eff es Bool
isSeeThrough e = do
  let c = getContainerMaybe e
      en = getEnterableMaybe e
  s <- isSupporter e
  isContainer <- isType e "container"
  let isOpenContainer = fmap _containerOpenable c == Just Open && isContainer
      isTransparent = fmap _containerOpacity c == Just Transparent
      isEnterableNotContainer = en == Just Enterable && not isContainer
  return $ s --if it's a supporter
      || isTransparent -- it's transparent
      || isEnterableNotContainer -- it's enterable but not a container
      || isOpenContainer -- it's an open container

containsLitObj ::
  NoMissingObjects wm es
  => HasLookingProperties wm
  => AnyObject wm -- ^ the object
  -> Eff es Bool
containsLitObj obj = do
  let enc = getEnclosingMaybe obj
  enc & maybe (return False) (\encs -> anyM (getObject >=> hasLight) (DES.elems $ contents encs))

{- | (4) An object itself has light if:
  (a) it's a room with the lighted property,
  (b) it's a thing with the lit property.
  If you want to include transitive light, you want `hasLight`.
-}
objectItselfHasLight ::
  AnyObject wm -- ^ the object
  -> Bool
objectItselfHasLight = asThingOrRoom
  ((Lit ==) . view (#objectData % #lit))
  ((Lighted ==) . view (#objectData % #darkness))

{- | (4) An object has light if:
  (a) it itself has the light attribute set, or
  (b) it is see-through and any of its immediate possessions have light, or
  (c) any object it places in scope using the property add_to_scope has light.
  ignoring (c) for now; TODO?
  this goes UP the object tree; it provides light TO its surroundings.
-}
hasLight ::
  NoMissingObjects wm es
  => HasLookingProperties wm
  => AnyObject wm
  -> Eff es Bool
hasLight obj = do
  ts <- getThingMaybe obj
  pure (objectItselfHasLight obj)
    ||^ (maybe (return False) isSeeThrough ts
      &&^ containsLitObj obj)