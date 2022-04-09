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
  , ActionParseArguments
  , ActionRulebook
  , ActionProcessing(..)
  , getAction
  , tryAction
  ) where

import Solitude
import Yaifl.Rulebooks.Rulebook
import Yaifl.WorldInfo
import Yaifl.Common
import Yaifl.Logger

-- | The type of argument parsing for actions. The important part here is that we
-- parse to `v` rather than to `Args s v` to better move between rulebooks.
type ActionParseArguments wm v = forall s. ParseArguments wm (UnverifiedArgs s) v

data ActionProcessing wm = ActionProcessing (forall m s. (MonadWorld wm m) => Action wm -> UnverifiedArgs s -> m (Maybe Bool))

-- | An 'Action' is a command that the player types, or that an NPC chooses to execute.
-- Pretty much all of it is lifted directly from the Inform concept of an action,
-- except that set action variables is not a rulebook.
data Action (wm :: WorldModel) where
  Action ::
    { _actionName :: !Text
    , _actionUnderstandAs :: ![Text]
    , _actionParseArguments :: !(ActionParseArguments wm v)
    , _actionBeforeRules :: !(ActionRulebook wm v)
    , _actionCheckRules :: !(ActionRulebook wm v)
    , _actionCarryOutRules :: !(ActionRulebook wm v)
    , _actionReportRules :: !(ActionRulebook wm v)
    } -> Action wm


-- | 'ActionRulebook's run over specific arguments; specifically, they expect
-- their arguments to be pre-verified; this allows for the passing of state.
type ActionRulebook wm v = forall s. Rulebook wm (Args s v) (Args s v) Bool

getAction
  :: MonadReader (World wm) m
  => Text
  -> UnverifiedArgs wm
  -> m (Maybe (Action wm))
getAction n _ = gview $ actions % at n


addAction :: 
  Action wm 
  -> World wm 
  -> World wm 
addAction ac =
  actions % at (_actionName ac) ?~ ac

-- | Attempt to run an action from a text command (so will handle the parsing).
-- Note that this does require the arguments to be parsed out.
tryAction :: 
  MonadWorld wm m
  => Text -- ^ text of command
  -> (Timestamp -> UnverifiedArgs wm) -- ^ Arguments without a timestamp
  -> m Bool
tryAction an f = do
  ta <- getGlobalTime
  debug (bformat ("Trying to do the action '" %! stext %! "'" ) an)
  let uva = f ta
  ac <- getAction an uva
  case ac of
    Nothing -> err (bformat ("Couldn't find a matching action for '" %! stext %! "'") an) >> return False
    Just a -> fromMaybe False <$> runAction uva a

-- | Run an action. This assumes that all parsing has been completed.
runAction :: 
  MonadWorld wm m
  => UnverifiedArgs s
  -> Action wm 
  -> m (Maybe Bool)
runAction args act = do
  w <- use actionProcessing
  let (ActionProcessing ap) = w
  ap act args