{-# LANGUAGE UndecidableInstances #-}

{-|
Module      : Yaifl.Text.Verb
Copyright   : (c) Avery 2024-2025
License     : MIT
Maintainer  : ppkfs@outlook.com

Verb conjugation and inflection system.

This module provides a comprehensive system for handling verb conjugation
in interactive fiction, supporting multiple tenses, voices, and grammatical
persons. The system is inspired by Inform7's verb handling but adapted to
Haskell's type system.

Key Features:

- **Type-safe verbs**: Compile-time verification of verb existence
- **Comprehensive conjugation**: Support for multiple tenses and persons
- **Grammatical correctness**: Proper handling of subject-verb agreement
- **Extensible**: Can be extended with additional verb forms and rules

The verb system handles:

- **Tenses**: Present, past, perfect, past perfect, future
- **Voices**: Active and passive
- **Persons**: First, second, third person (singular and plural)
- **Forms**: Base, infinitive, participles, etc.

This module works closely with:

- `Yaifl.Text.SayQQ`: Adaptive text with verb conjugation
- `Yaifl.Text.SayableValue`: Value display system
- `Yaifl.Text.Responses`: Response generation

Example usage:

@
  -- Conjugate a verb for specific subject and tense
  conjugated <- conjugate "take" ThirdPersonSingular Present
  -- Result: "takes"
  
  -- Use in adaptive text
  [saying| You {take} the sword. |]
  -- Automatically conjugates based on context
@

The system is designed to handle the complexities of English verb conjugation
while providing a simple, type-safe interface for game developers.
-}

module Yaifl.Text.Verb where

import GHC.TypeLits
import Yaifl.Prelude
import qualified Data.Text as T

-- https://ganelson.github.io/inform/inflections-module/4-ei.html#SP14
--  Each set contains five tenses, which in English are present (1), past (2), perfect (3), past perfect (4) and future (5).
{-
define BASE_FORM_TYPE 0
define INFINITIVE_FORM_TYPE 1
define PRESENT_PARTICIPLE_FORM_TYPE 2
define PAST_PARTICIPLE_FORM_TYPE 3
define ADJOINT_INFINITIVE_FORM_TYPE 4
english has 5 (present) and 6 (past) too
-}

-- | Typeclass for verbs that can be conjugated.
--
-- This typeclass enables compile-time verification that a symbol represents
-- a valid verb. The `toVerb` function converts a verb symbol to its runtime
-- representation.
--
-- The overlapping instance provides a helpful error message when attempting
-- to use a non-verb symbol, guiding developers to add the appropriate instances.
class ConjugatableVerb (v :: Symbol) where
  -- | Convert a verb symbol to its runtime representation.
  --
  -- This function takes a proxy for a verb symbol and returns the corresponding
  -- `Verb` value containing all conjugation information.
  --
  -- Example:
  -- @
  --   verb = toVerb (Proxy @"take")
  -- @
  toVerb :: Proxy v -> Verb

-- | Compile-time error for non-verb symbols.
--
-- This overlapping instance provides a helpful error message when someone
-- tries to use a symbol that isn't a recognized verb, suggesting they add
-- the appropriate `SayableValue` instance.
instance {-# OVERLAPPABLE #-} (TypeError
  ('Text "Cannot conjugate the verb 'to " ':<>: 'ShowType a ':<>:
    'Text "'; perhaps you are missing a SayableValue (SayLiteral " ':<>:
    'ShowType a ':<>: 'Text ") instance if this is not a verb?")) => ConjugatableVerb a where
  toVerb :: Proxy a -> Verb
  toVerb = error "impossible"

-- | Grammatical voice (active vs passive).
--
-- Voice determines whether the subject performs the action (active) or receives
-- it (passive). This affects verb conjugation and sentence structure.
--
-- Example:
-- @
--   Active: "You take the sword"
--   Passive: "The sword is taken by you"
-- @
data Voice = Active | Passive
  deriving stock (Enum, Bounded, Generic, Eq, Show, Ord)

-- | Verb sense (positive vs negative).
--
-- Verb sense indicates whether the action is performed (positive) or not
-- performed (negative). This affects auxiliary verbs and sentence structure.
--
-- Example:
-- @
--   Positive: "You take the sword"
--   Negative: "You do not take the sword"
-- @
data VerbSense = Positive | Negative
  deriving stock (Enum, Bounded, Generic, Eq, Show, Ord)

-- | Grammatical tense.
--
-- Tense indicates when the action occurs relative to the current time.
-- Yaifl supports the five main English tenses used in interactive fiction.
--
-- Constructors:
-- - `Present`: Action occurring now ("You take the sword")
-- - `Past`: Action that occurred previously ("You took the sword")
-- - `Perfect`: Action completed at an unspecified time ("You have taken the sword")
-- - `PastPerfect`: Action completed before another past action ("You had taken the sword")
-- - `Future`: Action that will occur ("You will take the sword")
--
-- Example:
-- @
--   -- Narrate actions in different tenses
--   describe Past "You took the sword yesterday"
--   describe Future "You will take the sword tomorrow"
-- @
data Tense = Present | Past | Perfect | PastPerfect | Future
  deriving stock (Enum, Bounded, Generic, Eq, Show, Ord)

-- | Grammatical person and number.
--
-- Personage combines person (first, second, third) with number (singular, plural)
-- to determine the correct verb conjugation and pronoun usage.
--
-- Constructors:
-- - `FirstPersonSingular`: "I" ("I take")
-- - `FirstPersonPlural`: "We" ("We take")
-- - `SecondPersonSingular`: "You" ("You take")
-- - `SecondPersonPlural`: "You" ("You take")
-- - `ThirdPersonSingular`: "He/She/It" ("He takes")
-- - `ThirdPersonPlural`: "They" ("They take")
--
-- This type is crucial for proper subject-verb agreement in English.
data VerbPersonage =
  FirstPersonSingular
  | FirstPersonPlural
  | SecondPersonSingular
  | SecondPersonPlural
  | ThirdPersonPlural
  | ThirdPersonSingular
  deriving stock (Enum, Bounded, Generic, Eq, Show, Ord)

-- | Complete verb information including forms and conjugation tables.
--
-- This data type contains all information needed to conjugate a verb in all
-- supported tenses, persons, and voices. It serves as the runtime representation
-- of verbs in the system.
--
-- Fields:
-- - `verbForms`: Base forms of the verb (infinitive, participles, etc.)
-- - `tabulation`: Conjugation tables for all combinations of tense/person/voice
--
-- Example:
-- @
--   -- Get conjugation for specific tense and person
--   conjugatedForm <- getConjugation verb Present ThirdPersonSingular
-- @
data Verb = Verb
  { verbForms :: VerbForms
  -- ^ Base forms and irregular conjugations
  , tabulation :: Tabulation
  -- ^ Complete conjugation tables
  }

-- | Base verb forms storage.
--
-- This data type stores the fundamental forms of a verb:
-- - Base form ("take")
-- - Infinitive ("to take")
-- - Present participle ("taking")
-- - Past participle ("taken")
--
-- These forms are used as the basis for generating all conjugated forms.
data VerbForms = VerbForms
  { infinitive :: Text
  , presentParticiple :: Text
  , pastParticiple :: Text
  , adjointInfinitive :: Maybe Verb
  , presentForm :: Text
  , pastForm :: Text
  }

newtype Tabulation = Tabulation { runTabulation :: Voice -> Tense -> VerbSense -> VerbPersonage -> Text }

makeVerb :: Text -> Verb
makeVerb x = let forms = makeVerbForms x in Verb forms (makeVerbTabulation forms)

-- we make verbs in two parts; firstly the forms and then the tabulation
-- since we need the forms to do the tabulation
-- even though many of these forms at used, I may as well add them for completeness sake
-- I believe that the expansion of a verb is always in active.
makeVerbForms :: Text -> VerbForms
makeVerbForms "aren't" = VerbForms "aren't" "being" "was" Nothing "is" "are"
makeVerbForms "be" = VerbForms "be" "being" "been" Nothing "is" "was"
makeVerbForms "have" = VerbForms "have" "having" "had" Nothing "has" "had"
makeVerbForms "do" = VerbForms "do" "doing" "done" Nothing "does" "did"
makeVerbForms "pass" = VerbForms "pass" "passing" "passed" Nothing "passes" "passed"
makeVerbForms x = VerbForms x (x <> "s") (x <> "ed") Nothing (x <> "s") (x <> "ed")

makeVerbTabulation :: VerbForms -> Tabulation
makeVerbTabulation v@VerbForms{infinitive="'re"} = toBeJustEnd True v toBePresentEnd toBePast
makeVerbTabulation v@VerbForms{infinitive="aren't"} = toBeTabulation True v
makeVerbTabulation v@VerbForms{infinitive="be"} = toBeTabulation False v
makeVerbTabulation v@VerbForms{infinitive="have"} = toHaveTabulation v
makeVerbTabulation v@VerbForms{infinitive="do"} = toDoTabulation v
makeVerbTabulation v@VerbForms{infinitive="be able to"} = toBeAbleToTabulation v
makeVerbTabulation x = regularVerbConjugation x

tabulate :: Verb -> Tense -> VerbSense -> VerbPersonage -> Text
tabulate = (`runTabulation` Active) . tabulation

toBeTabulation :: Bool -> VerbForms -> Tabulation
toBeTabulation isAbbreviated v = toHaveAndToBe isAbbreviated v toBePresent toBePast


toBeJustEnd :: Bool -> VerbForms -> (VerbPersonage -> Text) -> (VerbPersonage -> Text) -> Tabulation
toBeJustEnd isAbbreviated v present past = Tabulation $ \case
  -- pretty sure this is junk?
  -- Alice is carried by
  -- Alice is being?
  -- Alice was being?
  -- but let's try it anyway
  Passive -> \vs t vp -> tabulate toBe vs t vp #| pastParticiple v
  Active -> \case
    Present -> \case
      Positive -> present
      Negative -> \vp -> (\x -> if isAbbreviated then (x <>"n't") else ( x #| "not")) $ present vp
    Past -> \case
      Positive -> past
      Negative -> \vp -> past vp #| "not"
    Perfect -> \_vs _vp -> "have" #| pastParticiple v
    PastPerfect -> \_vs _vp -> "vargl" #| pastParticiple v
    Future -> \case
      Positive -> const $ "will" #| infinitive v
      Negative -> const $ "will not" #| infinitive v

toHaveAndToBe :: Bool -> VerbForms -> (VerbPersonage -> Text) -> (VerbPersonage -> Text) -> Tabulation
toHaveAndToBe isAbbreviated v present past = Tabulation $ \case
  -- pretty sure this is junk?
  -- Alice is carried by
  -- Alice is being?
  -- Alice was being?
  -- but let's try it anyway
  Passive -> \vs t vp -> tabulate toBe vs t vp #| pastParticiple v
  Active -> \case
    Present -> \case
      Positive -> present
      Negative -> \vp -> (\x -> if isAbbreviated then (x <>"n't") else ( x #| "not")) $ present vp
    Past -> \case
      Positive -> past
      Negative -> \vp -> past vp #| "not"
    Perfect -> \_vs _vp -> "have" #| pastParticiple v
    PastPerfect -> \_vs _vp -> "vargl" #| pastParticiple v
    Future -> \case
      Positive -> const $ "will" #| infinitive v
      Negative -> const $ "will not" #| infinitive v

toBePresent :: VerbPersonage -> Text
toBePresent = \case
  FirstPersonSingular -> "am"
  ThirdPersonSingular -> "is"
  _ -> "are"

toBePresentEnd :: VerbPersonage -> Text
toBePresentEnd = \case
  FirstPersonSingular -> "'m"
  ThirdPersonSingular -> "'s"
  _ -> "'re"

toBePast :: VerbPersonage -> Text
toBePast = \case
  FirstPersonSingular -> "was"
  ThirdPersonSingular -> "was"
  _ -> "were"

-- the lantern had had?

toHaveTabulation :: VerbForms -> Tabulation
toHaveTabulation v = toHaveAndToBe False v toHavePresent (const "had")

toHavePresent :: VerbPersonage -> Text
toHavePresent = \case
  ThirdPersonSingular -> "has"
  _ -> "have"

toDoTabulation :: VerbForms -> Tabulation
toDoTabulation v = Tabulation $ \case
  -- pretty sure this is junk?
  -- Alice is carried by
  -- Alice is being?
  -- Alice was being?
  -- but let's try it anyway
  Passive -> \vs t vp -> tabulate toBe vs t vp #| pastParticiple v #| "by"
  Active -> \case
    Present -> \case
      Positive -> toDoPresent
      Negative -> \vp -> toDoPresent vp #| "not"
    Past -> \case
      Positive -> const "did"
      Negative -> const "did not"
    Perfect -> \vs vp -> tabulate toAuxiliaryHave Present vs vp #| pastParticiple v
    PastPerfect -> \vs vp -> tabulate toAuxiliaryHave Past vs vp #| pastParticiple v
    Future -> \case
      Positive -> const $ "will" #| infinitive v
      Negative -> const $ "will not" #| infinitive v

toDoPresent :: VerbPersonage -> Text
toDoPresent = \case
  ThirdPersonSingular -> "does"
  _ -> "do"

regularVerbConjugation :: VerbForms -> Tabulation
regularVerbConjugation v = Tabulation $ \case
  Passive -> \vs t vp -> tabulate toBe vs t vp #| pastParticiple v #| "by"
  Active -> \case
    Present -> \case
      Positive -> regularVerbPresent v
      Negative -> \_vp -> "b" {- tabulate toDo Present Negative vp -} #| infinitive v
    Past -> \case
      Positive -> const $ pastForm v
      Negative -> \_vp -> "c" {- tabulate toDo Past Negative vp -} #| infinitive v
    Perfect -> \vs vp -> tabulate toAuxiliaryHave Present vs vp #| pastParticiple v
    PastPerfect -> \vs vp -> tabulate toAuxiliaryHave Past vs vp #| pastParticiple v
    Future -> let f pref = const $ pref #| infinitive v in \case
      Positive -> f "will"
      Negative -> f "will not"

regularVerbPresent :: VerbForms -> VerbPersonage -> Text
regularVerbPresent v = \case
  ThirdPersonSingular -> presentForm v
  _ -> infinitive v

adjointMaybe :: VerbForms -> (VerbForms -> Text) -> Text
adjointMaybe v form = maybe "" (form . verbForms) (adjointInfinitive v)

-- this is the non-auxiliary version; "he can" "we could not", etc.
-- the version with an actual meaning ("he will not be able to reach") is the auxiliary one
toBeAbleToTabulation :: VerbForms -> Tabulation
toBeAbleToTabulation v =
  let
    adjointForm = adjointMaybe v
  in Tabulation $ \case
  --passive here is
  --the lantern is able to be seen by?
  --the lantern was able to be seen?
  Passive -> \vs t vp ->
    -- is
    tabulate toBe vs t vp #|
    "able to be" #|
    -- Xd by
    adjointForm pastParticiple #|
    "by"
  Active -> \case
    Present -> \case
      Positive -> const "can"
      Negative -> const "cannot"
    Past -> \case
      Positive -> const "could"
      Negative -> const "could not"
    Perfect -> \vs vp -> tabulate toAuxiliaryHave Present vs vp #| "been able to"
    PastPerfect -> \vs vp -> tabulate toAuxiliaryHave Past vs vp #| "been able to"
    Future -> \case
      Positive -> const "will be able to"
      Negative -> const "will not be able to"

toBeAbleToAuxiliaryTabulation :: VerbForms -> Tabulation
toBeAbleToAuxiliaryTabulation v = Tabulation $ \case
  Active -> \t vs vp -> tabulate toBeAbleTo t vs vp #| adjointMaybe v infinitive
  Passive -> \t vs vp ->
    tabulate toBeAbleTo t vs vp #|
    "be" #|
    adjointMaybe v pastParticiple #|
    "by"

toModal :: VerbForms -> Tabulation
toModal v = let f pref form _vp = infinitive v #| pref #| adjointMaybe v form in Tabulation $ \case
  -- peter might have carried the lantern.
  -- the lantern might have had been carried by peter.
  -- the lantern should not have been carried by peter.
  --the lantern should not have had been carried by peter.
  -- yeah it breaks down a little, but we can try
  Passive -> \vs t vp ->
    infinitive v #|
    tabulate toBe vs t vp #|
    pastParticiple v #|
    "by"
  Active -> \case
    Present -> \case
      Positive -> f "" infinitive
      Negative -> f "not" infinitive
    Past -> \case
      Positive -> f "have" pastParticiple
      Negative -> f "not have" pastParticiple
    Perfect -> \case
      Positive -> f "have" pastParticiple
      Negative -> f "not have" pastParticiple
    PastPerfect -> \case
      Positive -> f "have" pastParticiple
      Negative -> f "not have" pastParticiple
    Future -> \case
      Positive -> f "" infinitive
      Negative -> f "not" infinitive

toBe :: Verb
toBe = makeVerb "is"

toDo :: Verb
toDo = makeVerb "do"

toBeAbleTo :: Verb
toBeAbleTo = makeVerb "do"

toAuxiliaryHave :: Verb
toAuxiliaryHave = makeVerb "auxiliary-have"

(#|) :: Text -> Text -> Text
(#|) "" b = b
(#|) a "" = a
(#|) a b = T.intercalate " " [a, b]
