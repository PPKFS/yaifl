# Parsing

```haskell file=src/Yaifl/Core/Actions/Parser.hs

module Yaifl.Core.Actions.Parser 
  (

  ) where
import Solitude
import Cleff.State
import Yaifl.Core.World
import Yaifl.Core.Actions.Action
import qualified Data.Text as T

parseAction ::
  State (WorldActions wm) :> es
  => Text
  -> Eff es (Either Text Bool)
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
```
