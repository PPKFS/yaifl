{-# LANGUAGE StrictData #-}

module Yaifl.Lamp.Visibility where

import Solitude

import Breadcrumbs ( addAnnotation )
import Text.Interpolation.Nyan ( rmode', int )
import Yaifl.Core.Actions.Activity (WithPrintingNameOfADarkRoom, WithPrintingDescriptionOfADarkRoom)
import Yaifl.Core.Entity
import Yaifl.Core.Object
import Yaifl.Core.Objects.Query
import Yaifl.Core.Objects.RoomData
import Yaifl.Core.Objects.ThingData
import Yaifl.Core.Properties.Enclosing
import Yaifl.Core.Properties.Has
import Yaifl.Core.Properties.Query
import Yaifl.Core.Rules.Args
import Yaifl.Lamp.Activities.DescribingLocale
import Yaifl.Lamp.Properties.Container
import Yaifl.Lamp.Properties.Openable
import Yaifl.Lamp.Properties.Supporter
import qualified Data.EnumSet as DES
import Data.Text.Display
import Yaifl.Core.WorldModel
import Yaifl.Core.Responses

-- | An easier way to describe the 3 requirements to look.
type HasLookingProperties wm =
  (WMHasProperty wm Enclosing, WMHasProperty wm Enterable, WMHasProperty wm Container
  , Display (WMSayable wm)
  , IsString (WMSayable wm)
  , WithPrintingNameOfADarkRoom wm
  , WithDescribingLocale wm
  , WithPrintingDescriptionOfADarkRoom wm
  , WithResponse wm "roomDescriptionHeadingA" ()
  , WithResponse wm "roomDescriptionHeadingB" (AnyObject wm)
  , WithResponse wm "roomDescriptionHeadingC" (AnyObject wm)
  , WithResponse wm "roomDescriptionBodyA" ())

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
  => CanBeAny wm o
  => ObjectLike wm o
  => o
  -> Eff es (AnyObject wm)
findVisibilityHolder e' = do
  obj <- getObject e'
  mCont <- getContainer e'
  let n = obj ^. #name
  if
    isRoom obj || isOpaqueClosedContainer <$?> mCont
  then
    do
      addAnnotation [int|t|The visibility holder of #{display n} is itself|]
      --return it
      return (toAny e')
  else
    do
      --get its container; we know it's a thing at this stage
      t <- getThing e'
      getObject (t ^. #objectData % #containedBy)

-- Inform Designer's Manual, Page 146
-- we recalculate the light of the immediate holder of an object
-- there is light exactly when the parent (p) "offers light"
-- has light is if it's lit, or see through and it contains light
-- offers light means it lights INTO itself
-- has light means it lights OUT AWAY from itself
recalculateLightOfParent ::
  NoMissingObjects wm es
  => HasLookingProperties wm
  => ObjectLike wm o
  => o
  -> Eff es Int
recalculateLightOfParent e = do
  (parent :: Maybe Entity) <- view (#objectData % #containedBy) <$$> getThingMaybe e
  case parent of
    --it's a room.
    Nothing -> return 0
    Just p -> do
      ol <- offersLight p
      if
        ol
      then
        (1+) <$> recalculateLightOfParent p
      else
        return 0


-- | An object offers light if:
-- - it is a lit thing (lit thing or lighted room)
-- - it contains a thing that has light
-- - it is see-through (an object) and its parent offers light
-- this goes DOWN the object tree; the light goes to its contents
offersLight ::
  NoMissingObjects wm es
  => HasLookingProperties wm
  => ObjectLike wm o
  => o
  -> Eff es Bool
offersLight e = do
  let parentOffersLight o = offersLight (o ^. #objectData % #containedBy)
      seeThruWithParent = maybe (return False) (\o' -> isSeeThrough o' &&^ parentOffersLight o')
  o <- getThingMaybe e

  objectItselfHasLight e -- it is a lit thing (lit thing or lighted room)
    ||^ seeThruWithParent o -- - it is see-through and its parent offers light
    ||^ containsLitObj e -- - it contains a thing that has light

-- | an object is see through if...
isSeeThrough ::
  NoMissingObjects wm es
  => HasLookingProperties wm
  => Thing wm
  -> Eff es Bool
isSeeThrough e = do
  (c, en, s) <- (,,) <$> getContainer e <*> getEnterable e <*> isSupporter e
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
  => ObjectLike wm o
  => o -- ^ the object
  -> Eff es Bool
containsLitObj e = do
  enc <- getEnclosing e
  enc & maybe (return False) (\encs -> anyM hasLight (DES.elems $ contents encs))

{- | (4) An object itself has light if:
  (a) it's a room with the lighted property,
  (b) it's a thing with the lit property.
  If you want to include transitive light, you want `hasLight`.
-}
objectItselfHasLight ::
  NoMissingObjects wm es
  => ObjectLike wm o
  => o -- ^ the object
  -> Eff es Bool
objectItselfHasLight e = asThingOrRoom e
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
  => ObjectLike wm o
  => o
  -> Eff es Bool
hasLight e = do
  ts <- getThingMaybe e
  objectItselfHasLight e
    ||^ (maybe (return False) isSeeThrough ts
      &&^ containsLitObj e)