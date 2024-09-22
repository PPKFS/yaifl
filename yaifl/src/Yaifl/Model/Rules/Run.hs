{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid lambda" #-}

module Yaifl.Model.Rules.Run
  ( runRulebook
  , runRulebookAndReturnVariables
  , failRuleWithError
  ) where

import Yaifl.Prelude

import Breadcrumbs
import Yaifl.Model.Actions.Args ( Refreshable(..) )
import Yaifl.Model.Rules.Rulebook
import qualified Data.Text as T
import Yaifl.Model.Rules.RuleEffects
import Yaifl.Text.Print (modifyBuffer)

-- | Run a rulebook. Mostly this just adds some logging baggage and tidies up the return type.
runRulebook ::
  HasCallStack
  => (Refreshable wm v, Display v, Display re)
  => RuleEffects wm es
  => x es
  => Maybe SpanID
  -> Bool
  -> Rulebook wm x v re
  -> v
  -> Eff es (Maybe re)
runRulebook mbSpanId skipParagraphBreaks rb ia = do
  mvre <- runRulebookAndReturnVariables mbSpanId skipParagraphBreaks rb ia
  return $ mvre >>= snd

-- | Run a rulebook and return possibly an outcome; the two levels of `Maybe` are for:
-- Nothing -> the rulebook arguments were not parsed correctly
-- Just (v, Nothing) -> the rulebook ran successfully, but had no definite outcome
-- Just (v, Just re) -> the rulebook ran successfully with outcome re
runRulebookAndReturnVariables ::
  forall wm x v es re.
  HasCallStack
  => (Refreshable wm v, Display v, Display re)
  => x es
  => RuleEffects wm es
  => Maybe SpanID
  -> Bool
  -> Rulebook wm x v re
  -> v
  -> Eff es (Maybe (v, Maybe re))
runRulebookAndReturnVariables mbSpanId skipParagraphBreaks Rulebook{..} args =
  -- ignore empty rulebooks to avoid logging spam
  if null rules
    then pure Nothing
    else maybe (withSpan "rulebook" name) (\f x -> x f) mbSpanId $ \rbSpan -> do
      addTagToSpan rbSpan "arguments" $ display args
      -- run the actual rules
      (v, r1) <- processRuleList rbSpan skipParagraphBreaks rules args
      let outcome = (v, r1 <|> defaultOutcome)
      addTagTo (Just rbSpan) "outcome" (display $ snd outcome)
      return (Just outcome)

-- | Mostly this is a very complicated "run a list of functions until you get
-- something that isn't a Nothing, or a default if you get to the end".
processRuleList ::
  (Refreshable wm v, Display v, Display re)
  => RuleEffects wm es
  => x es
  => SpanID
  -> Bool
  -> [Rule wm x v re]
  -> v
  -> Eff es (v, Maybe re)
processRuleList _ _ [] v = return (v, Nothing)
processRuleList rbSpan skipParagraphBreaks (Rule{..} : xs) args = do
  mbRes <- (if T.null name then withSpan' "rule" "some unnamed rule" else withSpan' "rule" name) $ do
    reqsMet <- checkPreconditions args preconditions
    if reqsMet then do
      if skipParagraphBreaks
      then
        Right <$> runRule args
      else do
        mb <- modifyBuffer id
        modifyBuffer (#ruleContext .~ name)
        r <- Right <$> runRule args
        void $ modifyBuffer (#ruleContext .~ view #ruleContext mb)
        pure r
    else pure (Left ())
  case mbRes of
    -- we failed the precondition, so we just keep going
    Left _ -> processRuleList rbSpan skipParagraphBreaks xs args
    Right (v, res) -> do
      whenJust v (\v' -> addAnnotationTo (Just rbSpan) $ "Updated rulebook variables to " <> display v')
      newArgs <- refreshVariables $ fromMaybe args v
      case res of
        Nothing -> processRuleList rbSpan skipParagraphBreaks xs newArgs
        Just r -> do
          addAnnotationTo (Just rbSpan) $ "Finished rulebook with result " <> display r
          return (newArgs, Just r)

checkPreconditions :: RuleEffects wm es => v -> [Precondition wm v] -> Eff es Bool
checkPreconditions v conds = do
  failedConds <- filterM (\p -> fmap not $ inject $ p `checkPrecondition` v) conds
  if null failedConds then pure True else do
      ns <- mapM (\c -> inject $ preconditionName c) failedConds
      addAnnotation $ mconcat $ ["failed to meet preconditions: "] <> ns
      pure False

-- | Return a failure (Just False) from a rule and log a string to the
-- debug log.
failRuleWithError ::
  Breadcrumbs :> es
  => Text -- ^ Error message.
  -> Eff es (Maybe Bool)
failRuleWithError = const (return (Just False)) <=< addAnnotation
