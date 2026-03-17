

{-|
Module      : Yaifl.Actions.GoesWith
Copyright   : (c) Avery 2022-2026
License     : MIT
Maintainer  : ppkfs@outlook.com

Action parameter and compatibility system.

This module defines the system for determining what parameters actions can take
and how they interact with different object types. It provides:

- `ActionSignature`: Type-level description of action parameter requirements
- `ActionParameter`: Runtime representation of action parameters
- `GoesWith`: Typeclass for checking action-object compatibility
- `ActionExpects`: Constraint synonym for action parameter expectations

This system enables type-safe action parsing and object matching, ensuring
that actions are only applied to appropriate objects with valid parameters.

Key concepts:
- Action signatures describe what kinds of parameters an action accepts
- GoesWith instances define which object types are compatible with actions
- The system supports complex parameter patterns (optional, alternatives, etc.)
-}

module Yaifl.Actions.GoesWith
  ( ActionSignature(..)
  , ActionExpects
  , ActionParameter(..)
  , GoesWith(..)
  ) where

import Yaifl.Prelude hiding (show)

import Yaifl.WorldModel
import GHC.Show
import qualified Data.Set as S
import Yaifl.Thing.Kind
import Yaifl.AnyObject

-- | Type-level description of what parameters an action expects.
--
-- This algebraic data type defines the different kinds of parameters
-- that actions can accept. It's used at the type level to ensure
-- actions are called with appropriate arguments.
--
-- Constructors:
-- - `TakesNoParameter`: Action takes no parameters
-- - `Optionally sig`: Action optionally takes the parameter described by `sig`
-- - `TakesDirectionParameter`: Action requires a direction parameter
-- - `TakesObjectParameter`: Action requires an object parameter
-- - `TakesThingParameter`: Action requires a thing parameter
-- - `TakesOneOf sig1 sig2`: Action can take EITHER `sig1` OR `sig2` (exclusive choice)
-- - `TakesConstantParameter`: Action takes a constant/string parameter
--
-- Example signatures:
-- @
--   -- "go north" - takes a direction
--   TakesDirectionParameter
--
--   -- "take sword" - takes a thing
--   TakesThingParameter
--
--   -- "open door" or "open chest" - different ways to specify the target
--   TakesOneOf TakesDirectionParameter TakesObjectParameter
-- @
data ActionSignature =
  TakesNoParameter
  | Optionally ActionSignature
  | TakesDirectionParameter
  | TakesObjectParameter
  | TakesThingParameter
  | TakesOneOf ActionSignature ActionSignature
  | TakesConstantParameter
  deriving stock ( Show, Eq, Ord, Generic )

-- | Runtime representation of action parameters.
--
-- This data type represents the actual parameter values passed to actions
-- at runtime, corresponding to the type-level `ActionSignature`.
--
-- Constructors:
-- - `NoParameter`: No parameter was provided
-- - `DirectionParameter dir`: A direction value (e.g., North, South)
-- - `ObjectParameter obj`: An object reference (rooms, containers, etc.)
-- - `ThingParameter thing`: A thing reference (portable objects)
-- - `ConstantParameter text`: A constant string value (e.g., "red", "open")
-- - `PluralParameter [params]`: Multiple parameters for actions that operate on several objects
--
-- Example parameters:
-- @
--   -- "go north" parameter
--   DirectionParameter North
--
--   -- "take sword" parameter  
--   ThingParameter swordThing
--
--   -- "take all" might result in plural parameters
--   PluralParameter [ThingParameter sword, ThingParameter shield, ThingParameter potion]
-- @
data ActionParameter wm =
  NoParameter
  | DirectionParameter (WMDirection wm)
  | ObjectParameter (AnyObject wm)
  | ThingParameter (Thing wm)
  | ConstantParameter Text
  | PluralParameter [ActionParameter wm]
  deriving stock ( Generic )

instance Show (ActionParameter wm) where
  show = \case
  -- todo: better named stuff
    NoParameter -> "No parameter"
    DirectionParameter _ -> "direction"
    ObjectParameter _ -> "object"
    ConstantParameter t -> show t
    ThingParameter _ -> "thing"
    PluralParameter wm -> "Multiple " <> show wm <> "s"
deriving stock instance Eq (WMDirection wm) => Eq (ActionParameter wm)
deriving stock instance Ord (WMDirection wm) => Ord (ActionParameter wm)

-- | Map action signatures to their expected Haskell types.
--
-- This type family translates type-level `ActionSignature` values into
-- concrete Haskell types that actions can work with. It's the bridge
-- between the declarative action signature system and the actual
-- function types.
--
-- The mapping works as follows:
-- - `TakesNoParameter` → `()` (unit, no value needed)
-- - `Optionally sig` → `Maybe (ActionExpects wm sig)` (optional parameter)
-- - `TakesDirectionParameter` → `WMDirection wm` (direction value)
-- - `TakesObjectParameter` → `AnyObject wm` (object reference)
-- - `TakesThingParameter` → `Thing wm` (thing reference)
-- - `TakesConstantParameter` → `Text` (string constant)
-- - `TakesOneOf sig1 sig2` → `Either (ActionExpects wm sig1) (ActionExpects wm sig2)` (choice)
--
-- This enables type-safe action definitions where the compiler ensures
-- actions receive parameters of the correct type.
--
-- Example:
-- @
--   -- An action that takes a thing parameter
--   takeAction :: Thing wm -> Eff es ()
--   takeAction thing = -- ... implementation
--   
--   -- The type signature ensures this action can only be called
--   -- when `tryParseArguments` successfully parses thing parameters
-- @
type family ActionExpects wm (goesWith :: ActionSignature) where
  ActionExpects wm TakesNoParameter = ()
  ActionExpects wm (Optionally goesWith) = Maybe (ActionExpects wm goesWith)
  ActionExpects wm TakesDirectionParameter = WMDirection wm
  ActionExpects wm TakesObjectParameter = AnyObject wm
  ActionExpects wm TakesThingParameter = Thing wm
  ActionExpects wm TakesConstantParameter = Text
  ActionExpects wm (TakesOneOf goesWith1 goesWith2) = Either (ActionExpects wm goesWith1) (ActionExpects wm goesWith2)
  --ActionExpects wm (PluralParameter goesWith) = [ActionExpects wm goesWith]

-- | Typeclass for parsing action parameters into expected types.
--
-- This typeclass bridges the gap between type-level action signatures
-- and runtime parameter values. It enables type-safe action parsing by:
--
-- 1. Converting type-level signatures to runtime values (`goesWithA`)
-- 2. Parsing runtime parameters into type-safe values (`tryParseArguments`)
--
-- The system works by:
-- - `ActionSignature` describes what parameters an action expects (type-level)
-- - `ActionParameter` represents actual parameter values (runtime)
-- - `ActionExpects` maps signatures to concrete Haskell types
-- - `GoesWith` instances provide the parsing logic between them
--
-- Type parameter:
-- - `g`: Action signature (type-level `ActionSignature`)
class GoesWith (g :: ActionSignature) where
  -- | Convert type-level signature to runtime value.
  --
  -- This method extracts the action signature from the type system
  -- so it can be inspected and processed at runtime.
  --
  -- Example:
  -- @
  --   goesWithA (Proxy @'TakesThingParameter) == TakesThingParameter
  -- @
  goesWithA :: Proxy g -> ActionSignature

  -- | Parse runtime parameters into expected type.
  --
  -- This is the core parsing function that enables type-safe action execution.
  -- It takes a set of runtime parameters and attempts to convert them into
  -- the concrete Haskell type expected by the action signature.
  --
  -- Returns `Just` with the parsed value on success, `Nothing` on failure.
  --
  -- Example:
  -- @
  --   -- For a thing-taking action with a sword parameter
  --   tryParseArguments (Proxy @'TakesThingParameter) swordParamSet
  --     == Just swordThing  -- Success
  --
  --   -- For the same action with a direction parameter
  --   tryParseArguments (Proxy @'TakesThingParameter) directionParamSet
  --     == Nothing  -- Failure: wrong parameter type
  -- @
  tryParseArguments :: Proxy g -> Set (ActionParameter wm) -> Maybe (ActionExpects wm g)

  -- | Default implementation that always fails.
  --
  -- Concrete instances override this to provide specific parsing logic
  -- for their action signature type.
  default tryParseArguments :: Proxy g -> Set (ActionParameter wm) -> Maybe (ActionExpects wm g)
  tryParseArguments _ _ = Nothing

instance GoesWith 'TakesNoParameter where
  goesWithA _ = TakesNoParameter
  tryParseArguments _ s = if S.null s then Just () else Nothing

instance GoesWith a => GoesWith ('Optionally a) where
  goesWithA _ = Optionally (goesWithA (Proxy @a))
  tryParseArguments _ s = if S.null s then Just Nothing else Just <$> tryParseArguments (Proxy @a) s

instance GoesWith 'TakesDirectionParameter where
  goesWithA _ = TakesDirectionParameter
  tryParseArguments _ s = if S.size s == 1
    then (case S.findMin s of
      DirectionParameter d -> Just d
      _ -> Nothing) else Nothing

instance GoesWith 'TakesObjectParameter where
  goesWithA _ = TakesObjectParameter
  tryParseArguments _ s = if S.size s == 1
    then (case S.findMin s of
      ObjectParameter d -> Just d
      ThingParameter t -> Just (toAny t)
      _ -> Nothing) else Nothing

instance GoesWith 'TakesThingParameter where
  goesWithA _ = TakesThingParameter
  tryParseArguments _ s = if S.size s == 1
    then (case S.findMin s of
      ThingParameter d -> Just d
      _ -> Nothing) else Nothing

instance GoesWith 'TakesConstantParameter where
  goesWithA _ = TakesConstantParameter
  tryParseArguments _ s = if S.size s == 1
    then (case S.findMin s of
      ConstantParameter d -> Just d
      _ -> Nothing) else Nothing

instance (GoesWith a, GoesWith b) => GoesWith ('TakesOneOf a b) where
  goesWithA _ = TakesOneOf (goesWithA (Proxy @a)) (goesWithA (Proxy @b))
  tryParseArguments _ s = (Left <$> tryParseArguments (Proxy @a) s) <|> (Right <$> tryParseArguments (Proxy @b) s)