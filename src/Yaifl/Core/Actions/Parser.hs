-- ~\~ language=Haskell filename=src/Yaifl/Core/Actions/Parser.hs
-- ~\~ begin <<lit/actions/parsing.md|src/Yaifl/Core/Actions/Parser.hs>>[0] project://lit/actions/parsing.md:4
module Yaifl.Core.Actions.Parser
  ( runActionHandlerAsWorldActions

  ) where

import Cleff.State ( State )
import Yaifl.Core.Actions.Action
import qualified Data.Text as T
import Yaifl.Core.Common
import Yaifl.Core.Rulebooks.Args ( playerNoArgs, UnverifiedArgs )
import Yaifl.Core.Objects.Query
import qualified Data.Map as Map
import Yaifl.Core.Logger ( debug, Log )
import Yaifl.Core.Say
import Yaifl.Core.Actions.Activity
import Text.Interpolation.Nyan


runActionHandlerAsWorldActions ::
  forall es wm.
  '[Log, State (WorldActions wm), Saying, ObjectLookup wm, ObjectUpdate wm, State (Metadata wm), State (ActivityCollection wm)] :>> es
  => ObjectTraverse wm :> es
  => Eff (ActionHandler : es)
  ~> Eff es
runActionHandlerAsWorldActions = interpret $ \case
  ParseAction t -> do
    --find the verb, which will be the first N words
    possVerbs <- findVerb t
    ac <- case possVerbs of
      [] -> return . Left $ "I have no idea what you meant by '" <> t <> "'."
      [(r, x)] -> runActionHandlerAsWorldActions $ findSubjects (T.strip r) x
      xs -> return $ Left $ "Did you mean " <> prettyPrintList (map (show . fst) xs) <> "?"
    whenLeft_ ac (noteError (const ()))
    return ac

findVerb ::
  State (WorldActions wm) :> es
  => Text
  -> Eff es [(Text, Action wm)]
findVerb cmd = do
  --we remove excess whitespace, then add 1 extra one
  let fixedCmd = T.strip cmd <> " "
  ac <- use actions
  return $ mapMaybe (\(t, v) -> T.stripPrefix t fixedCmd >>= (\r -> Just (r, v))) $ Map.toList ac

findSubjects :: 
  (ActionHandler :> es, ObjectLookup wm :> es, ObjectUpdate wm :> es, State (Metadata wm) :> es)
  => '[Log, State (WorldActions wm), State (ActivityCollection wm), Saying,ObjectTraverse wm] :>> es
  => Text
  -> Action wm
  -> Eff es (Either Text Bool)
findSubjects "" a = failHorriblyIfMissing $ do
  ua <- playerNoArgs
  Right <$> tryAction a ua
findSubjects _ _ = return $ Left "not implemented"

-- | Attempt to run an action from a text command (so will handle the parsing).
-- Note that this does require the arguments to be parsed out.
tryAction ::
  (NoMissingObjects wm es, ActionHandler :> es, ObjectTraverse wm :> es)
  => '[Log, State (WorldActions wm), State (ActivityCollection wm), Saying] :>> es
  => Action wm -- ^ text of command
  -> (Timestamp -> UnverifiedArgs wm) -- ^ Arguments without a timestamp
  -> Eff es Bool
tryAction an f = do
  ta <- getGlobalTime
  debug [int|t|Trying to do the action '#{_actionName an}'|]
  let uva = f ta
  fromMaybe False <$> runAction uva an

-- ~\~ end
