{-# LANGUAGE RecordWildCards #-}

module Yaifl.Rules.Run
  ( runRulebook
  , runRulebookAndReturnVariables
  , failRuleWithError
  ) where

import Solitude

import Breadcrumbs
import Data.Text.Display ( Display, display )
import Yaifl.Rules.Args ( Refreshable(..) )
import Yaifl.Rules.Rule
import Yaifl.Rules.Rulebook ( Rulebook(..) )
import qualified Data.Text as T
import Yaifl.Rules.RuleEffects

-- | Run a rulebook. Mostly this just adds some logging baggage and tidies up the return type.
runRulebook ::
  HasCallStack
  => (Refreshable wm v, Display v, Display re)
  => RuleEffects wm es
  => Maybe SpanID
  -> Rulebook wm v re
  -> v
  -> Eff es (Maybe re)
runRulebook mbSpanId rb ia = do
  mvre <- runRulebookAndReturnVariables mbSpanId rb ia
  return $ mvre >>= snd

-- | Run a rulebook and return possibly an outcome; the two levels of `Maybe` are for:
-- Nothing -> the rulebook arguments were not parsed correctly
-- Just (v, Nothing) -> the rulebook ran successfully, but had no definite outcome
-- Just (v, Just re) -> the rulebook ran successfully with outcome re
runRulebookAndReturnVariables ::
  forall wm v es re.
  HasCallStack
  => (Refreshable wm v, Display v, Display re)
  => RuleEffects wm es
  => Maybe SpanID
  -> Rulebook wm v re
  -> v
  -> Eff es (Maybe (v, Maybe re))
runRulebookAndReturnVariables mbSpanId Rulebook{..} args =
  -- ignore empty rulebooks to avoid logging spam
  if null rules
    then pure Nothing
    else maybe (withSpan "rulebook" name) (\f x -> x f) mbSpanId $ \rbSpan -> do
      addTagToSpan rbSpan "arguments" $ display args
      -- run the actual rules
      (v, r1) <- processRuleList rbSpan rules args
      let outcome = (v, r1 <|> defaultOutcome)
      addTagTo (Just rbSpan) "outcome" (display $ snd outcome)
      return (Just outcome)

-- | Mostly this is a very complicated "run a list of functions until you get
-- something that isn't a Nothing, or a default if you get to the end".
processRuleList ::
  (Refreshable wm v, Display v, Display re)
  => RuleEffects wm es
  => SpanID
  -> [Rule wm v re]
  -> v
  -> Eff es (v, Maybe re)
processRuleList _ [] v = return (v, Nothing)
processRuleList rbSpan (Rule{..} : xs) args = do
  mbRes <- (if T.null name then id else withSpan' "rule" name) $ do
    reqsMet <- checkPreconditions args preconditions
    if reqsMet then Right <$> runRule args else pure (Left ())
  case mbRes of
    -- we failed the precondition
    Left _ ->  pure (args, Nothing)
    Right (v, res) -> do
      whenJust v (\v' -> addAnnotationTo (Just rbSpan) $ "Updated rulebook variables to " <> display v')
      newArgs <- refreshVariables $ fromMaybe args v
      case res of
        Nothing -> processRuleList rbSpan xs newArgs
        Just r -> do
          addAnnotationTo (Just rbSpan) $ "Finished rulebook with result " <> display r
          return (newArgs, Just r)

checkPreconditions :: RuleEffects wm es => v -> [Precondition wm v] -> Eff es Bool
checkPreconditions v = allM (\p -> inject $ p `checkPrecondition` v)

-- | Return a failure (Just False) from a rule and log a string to the
-- debug log.
failRuleWithError ::
  Breadcrumbs :> es
  => Text -- ^ Error message.
  -> Eff es (Maybe Bool)
failRuleWithError = const (return (Just False)) <=< addAnnotation
