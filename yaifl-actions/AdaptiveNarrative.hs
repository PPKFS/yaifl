module Yaifl.Text.AdaptiveNarrative where

import Yaifl.Prelude

import Breadcrumbs
import Yaifl.Effects.ObjectQuery
import Yaifl.AnyObject
import Yaifl.Object.Kind
import Yaifl.Room.Kind
import Yaifl.Thing.Kind
import Yaifl.Metadata
import Yaifl.Object.Query
import Yaifl.Person.Kind
import Yaifl.WorldModel

regarding ::
  State (AdaptiveNarrative wm) :> es
  => CanBeAny wm a
  => Maybe a
  -> Eff es ()
regarding mbObj = do
  #priorNamedObject .= (toAny <$> mbObj)
  #priorQuantity .= 1

regardingMany ::
  State (AdaptiveNarrative wm) :> es
  => Eff es ()
regardingMany = do
  #priorQuantity .= 2

regardingNothing ::
  State (AdaptiveNarrative wm) :> es
  => Eff es ()
regardingNothing = regarding (Nothing :: Maybe (AnyObject wm))

regardingThePlayer ::
  forall wm es.
  State (AdaptiveNarrative wm) :> es
  => WithoutMissingObjects wm es
  => Eff es ()
regardingThePlayer = do
  p <- getPlayer' @wm
  regarding $ Just $ toAny p

getMentioned ::
  State (AdaptiveNarrative wm) :> es
  => Eff es (Maybe (AnyObject wm))
getMentioned = use #priorNamedObject

getMentionedRoom ::
  forall wm es.
  State (AdaptiveNarrative wm) :> es
  => State Metadata :> es
  => Display (WMText wm)
  => ObjectQuery wm :> es
  => Breadcrumbs :> es
  => Eff es (Room wm)
getMentionedRoom = do
  (mbObj :: Maybe (AnyObject wm)) <- use @(AdaptiveNarrative wm) #priorNamedObject
  r <- join <$> forM mbObj getRoomMaybe
  case r of
    Nothing -> error "The last mentioned object was expected to be a room, but it was not"
    Just x -> pure x

withRoom ::
  forall wm es a.
  State (AdaptiveNarrative wm) :> es
  => State Metadata :> es
  => Display (WMText wm)
  => ObjectQuery wm :> es
  => Breadcrumbs :> es
  => (Room wm -> Eff es a)
  -> Eff es a
withRoom f = do
  r <- getMentionedRoom
  f r

getMentionedThing ::
  forall wm es.
  State (AdaptiveNarrative wm) :> es
  => HasCallStack
  => State Metadata :> es
  => ObjectQuery wm :> es
  => Display (WMText wm)
  => Breadcrumbs :> es
  => Eff es (Thing wm)
getMentionedThing = do
  (mbObj :: Maybe (AnyObject wm)) <- use @(AdaptiveNarrative wm) #priorNamedObject
  r <- join <$> forM mbObj getThingMaybe
  case r of
    Nothing -> error "The last mentioned object was expected to be a thing, but it was not"
    Just x -> pure x

withThing ::
  forall wm es a.
  HasCallStack
  => State (AdaptiveNarrative wm) :> es
  => State Metadata :> es
  => Display (WMText wm)
  => ObjectQuery wm :> es
  => Breadcrumbs :> es
  => (Thing wm -> Eff es a)
  -> Eff es a
withThing f = do
  r <- getMentionedThing
  f r

getPersonageOfObject ::
  forall wm es.
  State Metadata :> es
  => State (AdaptiveNarrative wm) :> es
  => Eff es VerbPersonage
getPersonageOfObject = do
  o <- getMentioned
  q <- use @(AdaptiveNarrative wm) #priorQuantity
  case o of
    Nothing -> pure ThirdPersonSingular
    Just someObj -> do
      ifM (isPlayer someObj)
        (use @(AdaptiveNarrative wm) #narrativeViewpoint)
        (pure $ if someObj ^. #namePlurality == PluralNamed || q > 1 then ThirdPersonPlural else ThirdPersonSingular)
