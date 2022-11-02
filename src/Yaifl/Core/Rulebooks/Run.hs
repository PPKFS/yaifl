{-# LANGUAGE RecordWildCards #-}

module Yaifl.Core.Rulebooks.Run
  ( runRulebook
  , runRulebookAndReturnVariables
  , failRuleWithError
  ) where
import Yaifl.Core.Rulebooks.Rulebook ( Rulebook(..), ParseArguments(runParseArguments) )

import Yaifl.Core.Rulebooks.Rule ( Rule(..), RuleEffects, RuleCondition )
import qualified Data.Text as T
import Yaifl.Core.Logger ( Log, err, debug, withContext )
import Yaifl.Core.Rulebooks.Args ( Refreshable(..) )
import Yaifl.Core.Metadata
import Effectful
import Solitude
import Effectful.Error.Static

-- | Run a rulebook. Mostly this just adds some logging baggage and tidies up the return type.
runRulebook ::
  Refreshable wm v
  => RuleEffects wm es
  => Rulebook wm ia v re
  -> ia
  -> Eff es (Maybe re)
runRulebook rb ia = runRulebookAndReturnVariables rb ia >>= (\mvre -> return $ mvre >>= snd)

-- | Run a rulebook and return possibly an outcome; the two levels of `Maybe` are for:
-- Nothing -> the rulebook arguments were not parsed correctly
-- Just (v, Nothing) -> the rulebook ran successfully, but had no definite outcome
-- Just (v, Just re) -> the rulebook ran successfully with outcome re
runRulebookAndReturnVariables ::
  Refreshable wm v
  => RuleEffects wm es
  => Rulebook wm ia v re
  -> ia
  -> Eff es (Maybe (v, Maybe re))
runRulebookAndReturnVariables Rulebook{..} args = do
  -- ignore empty rulebooks to avoid logging spam
  unless (null _rbRules) $ debug [int|t|Running the #{_rbName} rulebook|]
  withContext _rbName $ do
    runParseArguments _rbParseArguments args >>= \case
      Left err' -> noteError (const Nothing) err'
      Right a -> do
        -- run the actual rules
        res <- (fmap Just . processRuleList _rbRules) a
        return $ res >>= (\(v, r1) -> Just (v, r1 <|> _rbDefaultOutcome))

-- | Mostly this is a very complicated "run a list of functions until you get
-- something that isn't a Nothing, or a default if you get to the end".
processRuleList ::
  Refreshable wm v
  => RuleEffects wm es
  => [Rule wm v re]
  -> v
  -> Eff es (v, Maybe re)
processRuleList [] v = return (v, Nothing)
processRuleList (Rule{..} : xs) args = do
  mbRes <- runError @RuleCondition $ _runRule args
  case mbRes of
    Left _ -> processRuleList xs args
    Right (v, res) -> do
      newArgs <- refreshVariables $ fromMaybe args v
      case res of
        Nothing -> processRuleList xs newArgs
        Just r -> do
          unless (T.empty == _ruleName) $ debug [int|t|Succeeded after following the #{_ruleName} rule|]
          return (newArgs, Just r)

  -- if we hit nothing, continue; otherwise return


-- | Return a failure (Just False) from a rule and log a string to the
-- debug log.
failRuleWithError ::
  Log :> es
  => Text -- ^ Error message.
  -> Eff es (Maybe Bool)
failRuleWithError t = err t >> return (Just False)
