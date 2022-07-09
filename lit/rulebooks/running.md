# Running a Rulebook

```haskell file=src/Yaifl/Core/Rulebooks/Run.hs
{-# LANGUAGE RecordWildCards #-}
module Yaifl.Core.Rulebooks.Run 
  ( runRulebook
  , runRulebookAndReturnVariables
  , failRuleWithError
  ) where
import Yaifl.Core.Objects.Query
import Yaifl.Core.Rulebooks.Rulebook

import Yaifl.Core.Rulebooks.Rule
import qualified Data.Text as T
import Yaifl.Core.Logger

-- | Run a rulebook. Mostly this just adds some logging baggage and tidies up the return type.
runRulebook :: 
  RuleEffects wm es
  => NoMissingObjects wm es
  => Rulebook wm ia v re
  -> ia
  -> Eff es (Maybe re)
runRulebook rb ia = runRulebookAndReturnVariables rb ia >>= (\mvre -> return $ mvre >>= snd)

-- | Run a rulebook and return possibly an outcome; the two levels of `Maybe` are for:
-- Nothing -> the rulebook arguments were not parsed correctly
-- Just (v, Nothing) -> the rulebook ran successfully, but had no definite outcome
-- Just (v, Just re) -> the rulebook ran successfully with outcome re
runRulebookAndReturnVariables :: 
  RuleEffects wm es
  => NoMissingObjects wm es
  => Rulebook wm ia v re
  -> ia
  -> Eff es (Maybe (v, Maybe re))
runRulebookAndReturnVariables Rulebook{..} args = do
  -- ignore empty rulebooks to avoid logging spam
  unless (null _rbRules) $ debug $ bformat ("Running the " %! stext %! " rulebook") _rbName
  withContext _rbName $ do
    runParseArguments _rbParseArguments args >>= \case
      Nothing -> 
        err (bformat ("Failed to parse rulebook arguments for " %! stext %! " rulebook") _rbName) >> return Nothing
      Just a -> do
        -- run the actual rules
        res <- (fmap Just . processRuleList _rbRules) a
        return $ res >>= (\(v, r1) -> Just (v, r1 <|> _rbDefaultOutcome))

-- | Mostly this is a very complicated "run a list of functions until you get
-- something that isn't a Nothing, or a default if you get to the end".
processRuleList :: 
  RuleEffects wm es
  => NoMissingObjects wm es
  => [Rule wm v re]
  -> v
  -> Eff es (v, Maybe re)
processRuleList [] v = return (v, Nothing)
processRuleList (Rule{..} : xs) args = do
  (v, res) <- _runRule args
  let newArgs = fromMaybe args v
  -- if we hit nothing, continue; otherwise return
  case res of
    Nothing -> processRuleList xs newArgs
    Just r -> do
      unless (T.empty == _ruleName)
        $ debug (bformat ("Succeeded after following the " %! stext %! " rule") _ruleName)
      return (newArgs, Just r)

-- | Return a failure (Just False) from a rule and log a string to the
-- debug log.
failRuleWithError :: 
  Log :> es
  => Text -- ^ Error message.
  -> Eff es (Maybe Bool)
failRuleWithError t = err (bformat stext t) >> (return $ Just False)

```
