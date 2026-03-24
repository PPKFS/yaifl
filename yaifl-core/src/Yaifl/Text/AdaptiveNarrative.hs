

{-|
Module      : Yaifl.Text.AdaptiveNarrative
Copyright   : (c) Avery 2023-2026
License     : MIT
Maintainer  : ppkfs@outlook.com

Adaptive narrative context for dynamic text generation.

Tracks narrative state including viewpoint, tense, and object references
to enable context-aware text generation with proper pronoun resolution
and grammatical consistency.

Key components:
- `AdaptiveNarrative`: Core narrative context type
- `getMentioned*`: Functions for retrieving referenced objects
- `with*`: Convenience functions for working with mentioned objects
- `getPersonageOfObject`: Verb conjugation helper
-}

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

-- | Narrative context containing the current state of text generation.
data AdaptiveNarrative wm = AdaptiveNarrative
  { narrativeViewpoint :: VerbPersonage
  -- ^ The narrative perspective (first, second, or third person)
  , tense :: Tense
  -- ^ The tense in which text should be generated
  , priorNamedObject :: Maybe (AnyObject wm)
  -- ^ The last mentioned object for pronoun resolution
  , priorQuantity :: Int
  -- ^ Quantity of the last mentioned object for pluralization
  } deriving stock (Generic)

-- | Generate field labels without prefix for the 'AdaptiveNarrative' type.
-- This enables using field names directly as lenses.
makeFieldLabelsNoPrefix ''AdaptiveNarrative

-- | Create a default 'AdaptiveNarrative' with sensible defaults.
--
-- The default narrative uses:
-- * Second person singular viewpoint ("you")
-- * Present tense
-- * No prior named object
-- * Quantity of 0
--
-- This is suitable for most interactive fiction scenarios where the player
-- is addressed directly in the present tense.
blankAdaptiveNarrative :: AdaptiveNarrative wm
blankAdaptiveNarrative = AdaptiveNarrative
  { narrativeViewpoint = SecondPersonSingular
  -- ^ Default to addressing the player directly
  , tense = Present
  -- ^ Default to present tense for immediate action
  , priorNamedObject = Nothing
  -- ^ No object has been mentioned yet
  , priorQuantity = 0
  -- ^ Default quantity is zero
  }

-- | Get the last mentioned object from the narrative context.
--
-- Returns 'Nothing' if no object has been mentioned yet, or 'Just' the
-- 'AnyObject' that was last referenced. This is used for pronoun resolution
-- and maintaining narrative coherence.
getMentioned ::
  State (AdaptiveNarrative wm) :> es
  => Eff es (Maybe (AnyObject wm))
getMentioned = use #priorNamedObject

-- | Get the last mentioned object as a 'Room'.
--
-- Retrieves the previously mentioned object and attempts to convert it to a
-- 'Room'. If the object is not a room, this function throws an error.
-- This is useful for narrative contexts where rooms are being described or
-- interacted with.
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

-- | Execute an action with the last mentioned room.
--
-- Convenience function that retrieves the last mentioned room and passes it
-- to the provided function. This is useful for operations that need to work
-- with the current narrative room context.
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

-- | Get the last mentioned object as a 'Thing'.
--
-- Retrieves the previously mentioned object and attempts to convert it to a
-- 'Thing'. If the object is not a thing, this function throws an error.
-- This is useful for narrative contexts where objects are being manipulated
-- or described.
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

-- | Execute an action with the last mentioned thing.
--
-- Convenience function that retrieves the last mentioned thing and passes it
-- to the provided function. This is useful for operations that need to work
-- with the current narrative object context.
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

-- | Determine the appropriate verb personage for the last mentioned object.
--
-- This function calculates the correct 'VerbPersonage' to use when referring
-- to the last mentioned object, taking into account:
--
-- * If the object is the player, use the narrative viewpoint
-- * If the object is plural or quantity > 1, use third person plural
-- * Otherwise, use third person singular
--
-- Returns 'ThirdPersonSingular' if no object has been mentioned.
--
-- Effects required:
-- * 'State' Metadata: Access to metadata
-- * 'State' (AdaptiveNarrative wm): Access to narrative context
--
-- Example:
--
-- @
-- -- Get the appropriate pronoun for the current object
-- personage <- getPersonageOfObject
-- verbForm <- conjugateVerb personage Present "go"
-- say $ "They " <> verbForm <> " quickly."
-- @

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
    -- ^ Default to third person singular if nothing mentioned
    Just someObj -> do
      ifM (isPlayer someObj)
        (use @(AdaptiveNarrative wm) #narrativeViewpoint)
        -- ^ Use narrative viewpoint for player
        (pure $ if someObj ^. #namePlurality == PluralNamed || q > 1 then ThirdPersonPlural else ThirdPersonSingular)
        -- ^ Determine plural vs singular for non-player objects
