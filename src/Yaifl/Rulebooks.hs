module Yaifl.Rulebooks
(
    makeBlankRule, runRulebook, makeRulebook, tryAction, makeRulebookWithVariables
    , runRulebookEx, ignoreArgs, singleArg, addRulebook
     , whenPlayBeginsName, whenPlayBeginsRules, introText,defaultActionProcessingRules
) where

import Yaifl.Prelude
import Yaifl.Components
import Yaifl.Common
import Yaifl.Utils
import Colog
import qualified Data.Text as T
import qualified Data.Text.Prettyprint.Doc.Render.Terminal as PPTTY

ignoreArgs :: (Int -> Bool, [Entity] -> Maybe ())
ignoreArgs = ((==0), const $ Just ())

singleArg :: (Int -> Bool, [Entity] -> Maybe Entity)
singleArg = ((==1), \case
    [] -> Nothing 
    [x] -> Just x
    _:_ -> Nothing)

-- | Create a placeholder rule (no rulebook variables).
makeBlankRule :: Text -> Rule w () a
makeBlankRule n = Rule n (do
    logError $ n <> " needs implementing"
    return Nothing)

-- | Create a placeholder rule (with rulebook variables).
makeBlankRuleWithVariables :: Text -> Rule w v a
makeBlankRuleWithVariables n = RuleWithVariables n (do
    logError $ n <> " needs implementing"
    return Nothing)

-- | Create a rulebook (no rulebook variables) from a name and a list of rules.
makeRulebook :: Text -> [Rule w () a] -> Rulebook w () a
makeRulebook n = Rulebook n Nothing

-- | Create a rulebook (with rulebook variables) from a name and a list of rules. 
makeRulebookWithVariables :: Text -> [Rule w v a] -> v -> Rulebook w v a
makeRulebookWithVariables n rs v = RulebookWithVariables n Nothing (return $ Just v) rs

logRulebookName :: WithGameData w m => Text -> m ()
logRulebookName n = unless (n == "") (do logDebug $ "Following the " <> n; pass)

addRulebook :: WithGameData w m => Rulebook w v RuleOutcome -> m ()
addRulebook r = do
    rulebookStore . at (rulebookName r) ?= BoxedRulebook r

runRulebookEx :: (Show v, WithGameData w m) => Rulebook w s v -> m (Maybe s, Maybe v)
runRulebookEx (RulebookWithVariables n def i r) = if null r then pure (Nothing, def) else
        do
            logRulebookName n
            --attempt to set the rulebook variables
            iv <- liftWorld i
            res <- maybe --if we were not able to set them
                    (do
                        logError "Could not successfully parse arguments"
                        return def)
                    (processRuleList r def) iv
            logDebug $ "Finished following the " <> n <> " with result " <> maybe "nothing" show res
            let r' = res <|> def
            return (iv, r')
runRulebookEx (Rulebook n def r) = if null r then pure (Nothing, def) else
    do
        logRulebookName n
        res <- doUntilJustM (\case
                Rule _ rf -> do
                    --unless (rn == "") (logDebug $ "Following thee " <> rn)
                    liftWorld rf
                RuleWithVariables _ _ -> do
                    logError "Hit argument rule in rulebook without args"
                    return def) r
        logDebug $ "Finished following the " <> n <> " with result " <> maybe "nothing" show res
        let r' = res <|> def
        return (Just (), r')

runRulebook :: (Show v, WithGameData w m) => Rulebook w s v -> m (Maybe v)
runRulebook r = fmap snd (runRulebookEx r)

processRuleList :: WithGameData w m => [Rule w v a] -> Maybe a -> v -> m (Maybe a)
processRuleList [] _ _ = return Nothing
processRuleList (x:xs) def args  = case x of
            Rule _ _ -> do
                logError "Hit argumentless rule in rulebook with args"
                return def
            RuleWithVariables _ (RuleVarsT rf) -> do
                --unless (rn == "") (logInfo $ "Following the " <> rn)
                let v = runStateT rf args
                (res, s) <- liftWorld v
                maybe (processRuleList xs def s) (return . Just) res

whenPlayBeginsName :: Text
whenPlayBeginsName = "when play begins rules"



whenPlayBeginsRules :: (HasStore w Enclosing, HasStore w Player, HasThing w) => Rulebook w () RuleOutcome 
whenPlayBeginsRules = makeRulebook whenPlayBeginsName [
            Rule "display banner rule" (do
                sayIntroText
                return Nothing),
            Rule "position player in model world rule" (do
                fr <- use firstRoom
                v <- maybe (return False) movePlayer fr
                unless v (logError "The first room was never set.")
                return Nothing),
            Rule "initial room description rule" (do
                tryAction "looking" []
                return Nothing)
        ]

introText :: Text -> [Text]
introText w = [longBorder<>"\n", shortBorder <> " " <> w <> " " <> shortBorder <> "\n", longBorder<>"\n\n"]
            where shortBorder = "------"
                  longBorder = foldr (<>) "" $ replicate (2 * T.length shortBorder + T.length w + 2) ("-" :: Text)

sayIntroText :: WithGameData w m => m ()
sayIntroText = do
    setStyle (Just (PPTTY.color PPTTY.Green <> PPTTY.bold))
    t <- use title
    mapM_ say (introText t) --replace w/say
    --setStyle Nothing
    pass

actionProcessingRulebookName :: Text
actionProcessingRulebookName = "action processing rulebook"

tryAction :: (HasStore w Player, WithGameData w m) => Text -> [Entity] -> m Bool
tryAction action args = do
    ac <- use $ actionStore . at action
    ap <- use actionProcessing
    res <- maybe (do
        logError $ "Couldn't find the action called " <> action
        return False)
        (\act -> do
        logDebug $ "Running action called " <> action
        p <- getPlayer
        currentActionVars .= (p, args)
        liftWorld $ ap act args
        ) ac
    logDebug $ "Action completed with result " <> show res
    return res

defaultActionProcessingRules :: WithGameData w m => BoxedAction w -> [Entity] -> m RuleOutcome
defaultActionProcessingRules (BoxedAction a) args = do
    res <- runRulebook (actionProcessingRulebookImpl a args)
    return $ fromMaybe False res

actionProcessingRulebookImpl :: Show v => Action w v -> [Entity] -> Rulebook w v RuleOutcome
actionProcessingRulebookImpl a args = RulebookWithVariables
    actionProcessingRulebookName
    (Just True)
    (if _appliesTo a (length args) then runRulebook (_setActionVariables a args) else return Nothing)
    (actionProcessingRules a)

actionProcessingRules :: Action w v -> [Rule w v Bool]
actionProcessingRules a = [makeBlankRuleWithVariables "before stage rule",
        makeBlankRuleWithVariables "carrying requirements rule",
        makeBlankRuleWithVariables "basic visibility rule",
        makeBlankRuleWithVariables "instead stage rule",
        makeBlankRuleWithVariables "requested actions require persuasion rule",
        makeBlankRuleWithVariables "carry out requested actions rule",
        makeBlankRuleWithVariables "investigate player awareness rule",
        makeBlankRuleWithVariables "check stage rule",
        RuleWithVariables "carry out stage rule" (do
            v <- getRulebookVariables
            runRulebook (_carryOutActionRules a v)),
        makeBlankRuleWithVariables "after stage rule",
        makeBlankRuleWithVariables "investigate player awareness after rule",
        makeBlankRuleWithVariables "report stage rule",
        makeBlankRuleWithVariables "clean actions rule"]
