module Yaifl.Text.AdaptiveNarrative where

import Yaifl.Prelude

import Yaifl.AnyObject
import Yaifl.Text.Verb
import Yaifl.Metadata
import Yaifl.TH
import Yaifl.Effects.ObjectQuery
import Breadcrumbs
import Yaifl.Room.Kind
import Yaifl.Thing.Kind
import Yaifl.Object.Query
import Yaifl.Object.Kind

{-
let views be {first person singular, first person plural, second person singular, second person plural, third person singular, third person plural };
	let tenses be {past tense, present tense, future tense, perfect tense, past perfect tense};
	repeat with the p running through views:
		repeat with the t running through tenses:
			now the story viewpoint is p;
			now the story tense is t;
			say "[p] [t] - [We] [negate the verb see] / [We] [eat] [line break]";
-}

data AdaptiveNarrative wm = AdaptiveNarrative
  { narrativeViewpoint :: VerbPersonage
  --, adaptiveTextViewpoint :: NarrativeViewpoint
  , tense :: Tense
  , priorNamedObject :: Maybe (AnyObject wm)
  , priorQuantity :: Int
  } deriving stock (Generic)

makeFieldLabelsNoPrefix ''AdaptiveNarrative

blankAdaptiveNarrative :: AdaptiveNarrative wm
blankAdaptiveNarrative = AdaptiveNarrative
  { narrativeViewpoint = SecondPersonSingular
  --, adaptiveTextViewpoint = FirstPersonPlural
  , tense = Present
  , priorNamedObject = Nothing
  , priorQuantity = 0
  }

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
