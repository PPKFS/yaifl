module Yaifl.Text.AdaptiveNarrative where


import Yaifl.Model.Object
import Solitude
import Effectful.Optics (use, (.=))
import Yaifl.Model.Objects.Query
import Yaifl.Metadata
import Breadcrumbs
import Yaifl.Rules.Args (getPlayer)
import Yaifl.Text.Verb
import Yaifl.Model.Objects.Effects

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
  } deriving stock (Generic)

blankAdaptiveNarrative :: AdaptiveNarrative wm
blankAdaptiveNarrative = AdaptiveNarrative
  { narrativeViewpoint = SecondPersonSingular
  --, adaptiveTextViewpoint = FirstPersonPlural
  , tense = Present
  , priorNamedObject = Nothing
  }

regarding ::
  State (AdaptiveNarrative wm) :> es
  => Maybe (AnyObject wm)
  -> Eff es ()
regarding mbObj = #priorNamedObject .= mbObj

regardingThePlayer ::
  forall wm es.
  State (AdaptiveNarrative wm) :> es
  => NoMissingObjects wm es
  => Eff es ()
regardingThePlayer = do
  p <- getPlayer @wm
  regarding $ Just $ toAny p

getMentioned ::
  State (AdaptiveNarrative wm) :> es
  => Eff es (Maybe (AnyObject wm))
getMentioned = use #priorNamedObject

getMentionedRoom ::
  forall wm es.
  State (AdaptiveNarrative wm) :> es
  => State Metadata :> es
  => ObjectLookup wm :> es
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
  => ObjectLookup wm :> es
  => Breadcrumbs :> es
  => (Room wm -> Eff es a)
  -> Eff es a
withRoom f = do
  r <- getMentionedRoom
  f r

getMentionedThing ::
  forall wm es.
  State (AdaptiveNarrative wm) :> es
  => State Metadata :> es
  => ObjectLookup wm :> es
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
  State (AdaptiveNarrative wm) :> es
  => State Metadata :> es
  => ObjectLookup wm :> es
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
  case o of
    Nothing -> pure ThirdPersonSingular
    Just someObj -> do
      ifM (isPlayer someObj)
        (use @(AdaptiveNarrative wm) #narrativeViewpoint)
        (pure $ if someObj ^. #namePlurality == PluralNamed then ThirdPersonPlural else ThirdPersonSingular)
