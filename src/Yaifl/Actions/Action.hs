{-|
Module      : Yaifl.Actions.Action
Description : An action is a verb that is carried out by the player (or an NPC).
Copyright   : (c) Avery, 2022
License     : MIT
Maintainer  : ppkfs@outlook.com
Stability   : No
-}

module Yaifl.Actions.Action 
  ( Action(..)
  , parseAction
  , ActionParseArguments
  , ActionRulebook
  , ActionProcessing(..)
  , getAction
  , tryAction
  , addAction
  , makeActionRulebook
  ) where

import Solitude
import Yaifl.Rulebooks.Rulebook
import Yaifl.WorldInfo
import Yaifl.Common
import Yaifl.Logger
import Display
import qualified Data.Text as T
import qualified Data.Map as Map
import Yaifl.Rulebooks.Args
import Yaifl.Objects.Missing

-- | The type of argument parsing for actions. The important part here is that we
-- parse to `v` rather than to `Args s v` to better move between rulebooks.
type ActionParseArguments wm v = ParseArguments wm (UnverifiedArgs wm) v

-- | This can't be newtype'd because of hs-boot shenanigans :(
{- HLINT ignore "Use newtype instead of data" -}
data ActionProcessing wm = ActionProcessing (forall m. (MonadWorld wm m) => Action wm -> UnverifiedArgs wm -> m (Maybe Bool))

-- | An 'Action' is a command that the player types, or that an NPC chooses to execute.
-- Pretty much all of it is lifted directly from the Inform concept of an action,
-- except that set action variables is not a rulebook.
data Action (wm :: WorldModel) where
  Action ::
    { _actionName :: !Text
    , _actionUnderstandAs :: ![Text]
    , _actionMatching :: ![Text]
    , _actionParseArguments :: !(ActionParseArguments wm v)
    , _actionBeforeRules :: !(ActionRulebook wm v)
    , _actionCheckRules :: !(ActionRulebook wm v)
    , _actionCarryOutRules :: !(ActionRulebook wm v)
    , _actionReportRules :: !(ActionRulebook wm v)
    } -> Action wm


-- | 'ActionRulebook's run over specific arguments; specifically, they expect
-- their arguments to be pre-verified; this allows for the passing of state.
type ActionRulebook wm v = Rulebook wm (Args wm v) (Args wm v) Bool

-- | Lookup an action from the world. TODO: handle "did you mean", synonyms, etc.
getAction :: 
  MonadReader (World wm) m
  => Text
  -> UnverifiedArgs wm
  -> m (Maybe (Action wm))
getAction n _ = gview $ actions % at n

-- | Add an action to the registry.
addAction :: 
  Action wm 
  -> World wm 
  -> World wm 
addAction ac =
  actions % at (_actionName ac) ?~ ac

parseAction ::
  MonadWorld wm m 
  => Text
  -> m (Either Text Bool)
parseAction t = failHorriblyIfMissing $ do
  --find the verb, which will be the first N words
  possVerbs <- findVerb t
  case possVerbs of
    [] -> return $ Left "I have no idea what you meant."
    [(r, x)] -> findSubjects (T.strip r) x
    xs -> return $ Left $ "Did you mean " <> prettyPrintList (map (displayText . fst) xs) 

findVerb :: MonadWorld wm m => Text -> m [(Text, Action wm)]
findVerb cmd = do
  --we remove excess whitespace, then add 1 extra one
  let fixedCmd = T.strip cmd <> " "
  ac <- use actions
  return $ mapMaybe (\(t, v) -> T.stripPrefix t fixedCmd >>= (\r -> Just (r, v))) $ Map.toList ac

findSubjects :: 
  NoMissingObjects m
  => MonadWorld wm m 
  => Text 
  -> Action wm 
  -> m (Either Text Bool)
findSubjects "" a = do
  ua <- playerNoArgs
  Right <$> tryAction a ua
findSubjects _ _ = return $ Left "not implemented"

-- | Attempt to run an action from a text command (so will handle the parsing).
-- Note that this does require the arguments to be parsed out.
tryAction :: 
  MonadWorld wm m
  => Action wm -- ^ text of command
  -> (Timestamp -> UnverifiedArgs wm) -- ^ Arguments without a timestamp
  -> m Bool
tryAction an f = do
  ta <- getGlobalTime
  debug (bformat ("Trying to do the action '" %! stext %! "'" ) (_actionName an))
  let uva = f ta
  fromMaybe False <$> runAction uva an

-- | Run an action. This assumes that all parsing has been completed.
runAction :: 
  MonadWorld wm m
  => UnverifiedArgs wm
  -> Action wm 
  -> m (Maybe Bool)
runAction args act = do
  w <- use actionProcessing
  let (ActionProcessing ap) = w
  ap act args

-- | Helper function to make a rulebook of an action.
makeActionRulebook :: 
  Text
  -> [Rule o (Args o v) Bool]
  -> ActionRulebook o v
makeActionRulebook n = Rulebook n Nothing (ParseArguments $ \x -> return $ Just x)