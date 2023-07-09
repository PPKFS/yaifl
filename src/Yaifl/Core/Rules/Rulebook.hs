{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TupleSections #-}

module Yaifl.Core.Rules.Rulebook
  ( -- * Types
    Args(..)
  , UnverifiedArgs(..)
  , ParseArguments(..)
  , ArgumentParseResult
  , Rulebook(..)
  , StandardRulebook
  , ParseArgumentEffects

    -- * Helper functions
  , addRuleFirst
  , addRuleLast
  , blankRulebook
  , noRulebookArguments
  ) where


import Solitude

import Yaifl.Core.Metadata ( WithMetadata )
import Yaifl.Core.Objects.Query ( NoMissingObjects )
import Yaifl.Core.Rules.Args
import Yaifl.Core.Rules.Rule
import Breadcrumbs
import Yaifl.Core.Rules.RuleEffects

type ParseArgumentEffects wm es = (WithMetadata es, NoMissingObjects wm es, RuleEffects wm es)
-- | `ParseArguments` is the equivalent of Inform7's `set rulebook variables`.
newtype ParseArguments wm ia v = ParseArguments
  { runParseArguments :: forall es. (ParseArgumentEffects wm es, Refreshable wm v) => ia -> Eff es (ArgumentParseResult v)
  }

type ArgumentParseResult v = Either Text v
-- | A 'Rulebook' is a computation (ia -> m (Maybe r)) built out of an initialisation (ia -> Maybe v), a default `Maybe r`,
-- and component rules `[(Text, (v -> m (Maybe v, Maybe r))]`
data Rulebook wm ia v r = Rulebook
  { name :: Text
  , defaultOutcome :: Maybe r
  , parseArguments :: ParseArguments wm ia v
  , rules :: [Rule wm v r]
  } deriving stock (Generic)

noRulebookArguments :: ParseArguments wm v v
noRulebookArguments = ParseArguments (\x -> ignoreSpan >> return (Right x))

blankRulebook ::
  Text
  -> Rulebook wm v v r
blankRulebook n = Rulebook n Nothing noRulebookArguments []

-- | A `StandardRulebook` is one which expects to verify its own arguments.
type StandardRulebook wm v r = Rulebook wm (UnverifiedArgs wm) v r

makeFieldLabelsNoPrefix ''Rulebook

-- | Add a rule to a rulebook last.
addRuleLast ::
  Rule wm v r
  -> Rulebook wm ia v r
  -> Rulebook wm ia v r
addRuleLast r = #rules %~ (++ [r])

-- | Add a rule to a rulebook first.
addRuleFirst ::
  Rule wm v r
  -> Rulebook wm ia v r
  -> Rulebook wm ia v r
addRuleFirst r = #rules %~ (r :)

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
