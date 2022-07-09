# Rules and Rulebooks

```haskell file=src/Yaifl/Core/Rulebooks/Rule.hs
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Yaifl.Core.Rulebooks.Rule 
  ( RuleEffects
  , Rule(..)

  , notImplementedRule
  , makeRule
  , makeRule'
  , rulePass

  , ruleName
  , runRule

  ) where

import Cleff.State
import Yaifl.Core.Common
import Solitude
import Yaifl.Core.Objects.Query
import Yaifl.Core.Logger
import Yaifl.Core.Say

type RuleEffects wm es = (State (Metadata wm) :> es, Log :> es, NoMissingObjects wm es, Saying :> es)

-- | A 'Rule' is a wrapped function with a name, that modifies the world (potentially)
-- and any rulebook variables, and might return an outcome (Just) or not (Nothing).
data Rule wm v r = Rule
  { _ruleName :: Text
  , _runRule :: forall es. (RuleEffects wm es) => v -> Eff es (Maybe v, Maybe r)
  }

-- | A helper for rules which are not implemented and therefore blank.
notImplementedRule ::
  Text
  -> Rule wm v r
notImplementedRule n = makeRule' n (do
    warn $ bformat (stext %! " needs implementing") n
    return Nothing)

-- | Make a rule that does not modify the action arguments.
makeRule :: 
  Text -- ^ Rule name.
  -> (forall es. (RuleEffects wm es) => v -> Eff es (Maybe r)) -- ^ Rule function.
  -> Rule wm v r
makeRule n f = Rule n (fmap (Nothing, ) . f)

-- | Make a rule that does has no arguments. this is more convenient to avoid \() ->...
makeRule' :: 
  Text -- ^ Rule name.
  -> (forall es. (RuleEffects wm es) => Eff es (Maybe r)) -- ^ Rule function.
  -> Rule wm v r
makeRule' n f = makeRule n (const f)

-- | Remove any unwanted return values from a `Rule`.
rulePass :: 
  Monad m
  => m (Maybe a)
rulePass = return Nothing

makeLenses ''Rule
```

```haskell file=src/Yaifl/Core/Rulebooks/Rulebook.hs
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TupleSections #-}

module Yaifl.Core.Rulebooks.Rulebook
  ( -- * Types
    Args(..)
  , UnverifiedArgs(..)
  , ParseArguments(..)
  , Rulebook(..)
  , StandardRulebook

    -- * Helper functions
  , addRuleFirst
  , addRuleLast
  , blankRulebook

    -- * Lenses
  , argsSource
  , argsVariables
  , argsTimestamp
  , ruleName
  , runRule
  , rbName
  , rbDefaultOutcome
  , rbParseArguments
  , rbRules
  ) where

import Solitude
import Yaifl.Core.Logger
import Cleff.State
import Yaifl.Core.Common
import Yaifl.Core.Objects.Query
import Yaifl.Core.Rulebooks.Args
import Yaifl.Core.Rulebooks.Rule

-- | `ParseArguments` is the equivalent of Inform7's `set rulebook variables`.
newtype ParseArguments wm ia v = ParseArguments
  { runParseArguments :: forall es. (State (Metadata wm) :> es, Log :> es, ObjectLookup wm :> es) => ia -> Eff es (Maybe v)
  }

-- | A 'Rulebook' is a computation (ia -> m (Maybe r)) built out of an initialisation (ia -> Maybe v), a default `Maybe r`,
-- and component rules `[(Text, (v -> m (Maybe v, Maybe r))]`
data Rulebook wm ia v r = Rulebook
  { _rbName :: Text
  , _rbDefaultOutcome :: Maybe r
  , _rbParseArguments :: ParseArguments wm ia v
  , _rbRules :: [Rule wm v r]
  }

blankRulebook :: 
  Text 
  -> Rulebook wm v v r 
blankRulebook n = Rulebook n Nothing (ParseArguments (return . Just)) []

-- | A `StandardRulebook` is one which expects to verify its own arguments.
type StandardRulebook wm v r = Rulebook wm (UnverifiedArgs wm) v r

makeLenses ''Rulebook

-- | Add a rule to a rulebook last.
addRuleLast :: 
  Rule wm v r
  -> Rulebook wm ia v r
  -> Rulebook wm ia v r
addRuleLast r = rbRules %~ (++ [r])

-- | Add a rule to a rulebook first.
addRuleFirst :: 
  Rule wm v r
  -> Rulebook wm ia v r
  -> Rulebook wm ia v r
addRuleFirst r = rbRules %~ (r :)

-- | Remove any unwanted return values from a `Rule`.
rulePass :: 
  Monad m
  => m (Maybe a)
rulePass = return Nothing

--TODO: add in specific places
--TODO: add in replacement
--TODO: reorder
{-
initRoomDescription
  :: NoMissingObjects wm m
  => MonadWorld wm m
  => m (Maybe a)
initRoomDescription = do
  ua <- playerNoArgs
  tryAction "looking" ua >> rulePass

-- | No Arguments, player source.
playerNoArgs
  :: forall wm m. NoMissingObjects wm m
  => MonadWorld wm m
  => m (Timestamp -> UnverifiedArgs s)
playerNoArgs = do
  ua <- withPlayerSource blank
  return (\ts -> ua & coercedTo @(Args wm [AnyObject s]) % argsTimestamp .~ ts)

withPlayerSource
  :: forall wm m. NoMissingObjects wm m
  => MonadWorld wm m
  => UnverifiedArgs s
  -> m (UnverifiedArgs s)
withPlayerSource u = do
  p <- getPlayer
  return $ u & coercedTo @(Args wm [AnyObject s]) % argsSource ?~ toAny p

positionPlayer
  :: MonadWorld wm m
  => HasProperty wm Enclosing
  => m (Maybe Bool)
positionPlayer = do
  fr <- gets _firstRoom
  pl <- gets _currentPlayer
  case fr of
    Nothing -> failRuleWithError
      "No rooms have been made, so cannot place the player."
    Just fr' -> do      m <- move pl fr'
-}
```
