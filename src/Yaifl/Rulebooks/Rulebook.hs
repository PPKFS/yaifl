{-|
Module      : Yaifl.Rulebooks.Rulebook
Description : The types for rulebooks; they are intended to be a mirror of Inform7's rulebooks, but
here they are implemented as monadic computations which are modular - sections of computation can be removed, added,
replaced, or reordered.
Copyright   : (c) Avery, 2022
License     : MIT
Maintainer  : ppkfs@outlook.com
Stability   : No
-}

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Yaifl.Rulebooks.Rulebook
  ( -- * Types
    Args(..)
  , UnverifiedArgs(..)
  , ParseArguments(..)
  , Rule(..)
  , Rulebook(..)
  , StandardRulebook

    -- * Helper functions
  , makeRule
  , makeRule'
  , notImplementedRule
  , failRuleWithError
  , addRuleFirst
  , addRuleLast

    -- * Evaluation
  , runRulebook
  , runRulebookAndReturnVariables

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
import {-# SOURCE #-} Yaifl.World
import qualified Data.Text as T
import Yaifl.Logger
import Yaifl.Objects.Missing
import Yaifl.Rulebooks.Args

-- | A 'Rule' is a wrapped function with a name, that modifies the world (potentially)
-- and any rulebook variables, and might return an outcome (Just) or not (Nothing).
data Rule wm v r = Rule
  { _ruleName :: Text
  , _runRule :: forall m. (NoMissingObjects m, MonadWorld wm m) => v -> m (v, Maybe r)
  }

-- | A helper for rules which are not implemented and therefore blank.
notImplementedRule ::
  Text
  -> Rule wm v r
notImplementedRule n = Rule n (\v -> do
    warn $ bformat (stext %! " needs implementing") n
    return (v, Nothing))

-- | Make a rule that does not modify the action arguments.
makeRule :: 
  Text -- ^ Rule name.
  -> (forall m. (NoMissingObjects m, MonadWorld wm m) => v -> m (Maybe r)) -- ^ Rule function.
  -> Rule wm v r
makeRule n f = Rule n (traverseToSnd f)

-- | Make a rule that does has no arguments. this is more convenient to avoid \() ->...
makeRule' :: 
  Text -- ^ Rule name.
  -> (forall m. (NoMissingObjects m, MonadWorld wm m) => m (Maybe r)) -- ^ Rule function.
  -> Rule wm v r
makeRule' n f = makeRule n (const f)

-- | `ParseArguments` is the equivalent of Inform7's `set rulebook variables`.
newtype ParseArguments wm ia v = ParseArguments
  { runParseArguments :: forall m. (NoMissingObjects m, MonadWorld wm m) => ia -> m (Maybe v)
  }

-- | A 'Rulebook' is a composition of functions ('Rule's) with short-circuiting (if
-- a Just value is returned) over an object universe `o`, input arguments `ia`, variables `v`
-- and returns an `r`.
data Rulebook wm ia v r = Rulebook
  { _rbName :: Text
  , _rbDefaultOutcome :: Maybe r
  , _rbParseArguments :: ParseArguments wm ia v
  , _rbRules :: [Rule wm v r]
  }

-- | A `StandardRulebook` is one which expects to verify its own arguments.
type StandardRulebook wm v r = Rulebook wm (UnverifiedArgs wm) v r

makeLenses ''Rule
makeLenses ''Rulebook

-- | Run a rulebook. Mostly this just adds some logging baggage and tidies up the return type.
runRulebook :: 
  NoMissingObjects m
  => MonadWorld wm m
  => Rulebook wm ia v re
  -> ia
  -> m (Maybe re)
runRulebook rb ia = runRulebookAndReturnVariables rb ia >>= (\mvre -> return $ mvre >>= snd)

-- | Run a rulebook and return possibly an outcome; the two levels of `Maybe` are for:
-- Nothing -> the rulebook arguments were not parsed correctly
-- Just (v, Nothing) -> the rulebook ran successfully, but had no definite outcome
-- Just (v, Just re) -> the rulebook ran successfully with outcome re
runRulebookAndReturnVariables :: 
  NoMissingObjects m
  => MonadWorld wm m
  => Rulebook wm ia v re
  -> ia
  -> m (Maybe (v, Maybe re))
runRulebookAndReturnVariables Rulebook{..} args = do
  -- ignore empty rulebooks to avoid logging spam
  unless (null _rbRules) $ debug $ bformat ("Running the " %! stext %! " rulebook") _rbName
  withContext (bformat stext _rbName) $ do
    runParseArguments _rbParseArguments args >>= \case
      Nothing -> 
        err (bformat ("Failed to parse rulebook arguments for " %! stext %! " rulebook") _rbName) 
          >> return Nothing
      Just a -> do
        -- run the actual rules
        res <- (fmap Just . processRuleList _rbRules) a
        return $ (\(v, r1) -> Just (v, r1 <|> _rbDefaultOutcome)) =<< res

-- | Mostly this is a very complicated "run a list of functions until you get
-- something that isn't a Nothing, or a default if you get to the end".
processRuleList :: 
  NoMissingObjects m
  => MonadWorld wm m
  => [Rule wm v re]
  -> v
  -> m (v, Maybe re)
processRuleList [] v = return (v, Nothing)
processRuleList (Rule{..} : xs) args = do
  (v, res) <- _runRule args
  -- if we hit nothing, continue; otherwise return
  case res of
    Nothing -> processRuleList xs v
    Just r -> do
      unless (T.empty == _ruleName)
        $ debug (bformat ("Succeeded after following the " %! stext %! " rule") _ruleName)
      return (v, Just r)

-- | Return a failure (Just False) from a rule and log a string to the
-- debug log.
failRuleWithError :: 
  Logger m
  => Text -- ^ Error message.
  -> m (Maybe Bool)
failRuleWithError t = do
  err (bformat stext t)
  return $ Just False

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

--TODO: add in specific places
--TODO: add in replacement
--TODO: reorder
{-

rulePass
  :: Monad m
  => m (Maybe a)
rulePass = return Nothing

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
> Rule wm () Bool
  -> m ()
addWhenPlayBegins r = whenPlayBegins %= addRule r

-}