
module Yaifl.Model.Rules.Rulebook
  ( -- * Types
    Args(..)
  , UnverifiedArgs(..)
  , Rulebook(..)
    -- * Helper functions
  , addRuleFirst
  , addRuleLast
  , blankRulebook
  , getRuleNames

  , Rule(..)
  , Precondition(..)
  , RuleLimitedEffect(..)
  , Unconstrained
  , forPlayer'
  , forPlayer
  , forKind
  , parseAction
  , notImplementedRule
  , makeRule
  , makeRule'
  , rulePass
  , ruleWhenJustM
  , ruleGuard
  , ruleGuardM
  , forThing
  , stopTheAction
  ) where


import Yaifl.Prelude
import Breadcrumbs

import Yaifl.Model.Rules.RuleEffects
import Yaifl.Model.Kinds.Thing
import Yaifl.Model.ObjectLike
import Yaifl.Model.Actions.Args
import Yaifl.Model.Effects
import Yaifl.Model.Query
import Data.Text.Display
import Yaifl.Model.WorldModel
import Yaifl.Model.Kinds.Object
import Yaifl.Model.Metadata

newtype RuleLimitedEffect wm es a = RuleLimitedEffect (SayableValue (WMText wm) wm => Display (WMText wm) => Eff (es : ConcreteRuleStack wm) a)

data Precondition wm v = Precondition
  { preconditionName :: forall es. RuleEffects wm es => Eff es Text
  , checkPrecondition :: forall es. RuleEffects wm es => v -> Eff es Bool
  }

forPlayer :: Precondition wm (Args wm v)
forPlayer = Precondition (pure "actor is the player") $ \v -> do
  p <- getPlayer
  pure $ p == v ^. #source

forPlayer' :: [Precondition wm (Args wm v)]
forPlayer' = [forPlayer]

forKind :: ObjectKind -> Precondition wm (Args wm (Thing wm))
forKind k = Precondition (pure $ "of kind " <> show k) $ \v -> variables v `isKind` k

-- | A 'Rule' is a wrapped function with a name, that modifies the world (potentially)
-- and any rulebook variables, and might return an outcome (Just) or not (Nothing).
data Rule wm (x :: [Effect] -> Constraint) v r = Rule
  { name :: Text
  , preconditions :: [Precondition wm v]
  , runRule :: forall es. (RuleEffects wm es, Refreshable wm v, x es) => v -> Eff es (Maybe v, Maybe r)
  }

class Unconstrained t
instance Unconstrained t

-- | A helper for rules which are not implemented and therefore blank.
notImplementedRule ::
  Text
  -> Rule wm x v r
notImplementedRule n = makeRule' n (do
  ignoreSpan -- this will discard the rule span
  addAnnotation $ "Rule " <> n <> " needs implementing"
  return Nothing)

-- | Make a rule that does not modify the action arguments.
makeRule ::
  Text -- ^ Rule name.
  -> [Precondition wm v]
  -> (forall es. (RuleEffects wm es, Refreshable wm v, x es) => v -> Eff es (Maybe r)) -- ^ Rule function.
  -> Rule wm x v r
makeRule n c f = Rule n c (fmap (Nothing, ) . f)

-- | Make a rule that has no arguments. this is more convenient to avoid \() ->...
makeRule' ::
  Text -- ^ Rule name.
  -> (forall es. (RuleEffects wm es, x es) => Eff es (Maybe r)) -- ^ Rule function.
  -> Rule wm x v r
makeRule' n f = makeRule n [] (const f)

-- | Remove any unwanted return values from a `Rule`.
rulePass ::
  Monad m
  => m (Maybe a)
rulePass = return Nothing

stopTheAction ::
  Monad m
  => m (Maybe Bool)
stopTheAction = return (Just False)

ruleGuard ::
  Monad m
  => Bool
  -> m (Maybe b, Maybe r)
  -> m (Maybe b, Maybe r)
ruleGuard cond f = if cond then f else pure (Nothing, Nothing)

ruleGuardM ::
  Monad m
  => m Bool
  -> m (Maybe b, Maybe r)
  -> m (Maybe b, Maybe r)
ruleGuardM cond f = ifM cond f $ pure (Nothing, Nothing)

ruleWhenJustM ::
  Monad m
  => m (Maybe a)
  -> (a -> m (Maybe b, Maybe r))
  -> m (Maybe b, Maybe r)
ruleWhenJustM mb f = do
  m' <- mb
  maybe (pure (Nothing, Nothing)) f m'

forThing ::
  NoMissingObjects wm es
  => ObjectLike wm a
  => a
  -> (Thing wm -> Eff es (Maybe b, Maybe r))
  -> Eff es (Maybe b, Maybe r)
forThing e = ruleWhenJustM (getThingMaybe e)

makeFieldLabelsNoPrefix ''Rule

-- | A 'Rulebook' is a computation (ia -> m (Maybe r)) built out of an initialisation (ia -> Maybe v), a default `Maybe r`,
-- and component rules `[(Text, (v -> m (Maybe v, Maybe r))]`
data Rulebook wm x v r = Rulebook
  { name :: Text
  , defaultOutcome :: Maybe r
  , rules :: [Rule wm x v r]
  } deriving stock (Generic)

getRuleNames ::
  Rulebook wm x v r
  -> [Text]
getRuleNames r = map (\r' -> case r' ^. #name of
  "" -> r' ^. #name <> " blank rule"
  x -> x) (rules r)

blankRulebook ::
  Text
  -> Rulebook wm x v r
blankRulebook n = Rulebook n Nothing []

makeFieldLabelsNoPrefix ''Rulebook

-- | Add a rule to a rulebook last.
addRuleLast ::
  Rule wm x v r
  -> Rulebook wm x v r
  -> Rulebook wm x v r
addRuleLast r = #rules %~ (++ [r])

-- | Add a rule to a rulebook first.
addRuleFirst ::
  Rule wm x v r
  -> Rulebook wm x v r
  -> Rulebook wm x v r
addRuleFirst r = #rules %~ (r :)
