module Yaifl.Rulebooks
(
    makeBlankRule, runRulebook, makeRulebook, tryAction, makeRulebookWithVariables
  , whenPlayBeginsName, whenPlayBeginsRules, introText,defaultActionProcessingRules
) where

import Yaifl.Prelude
import Yaifl.Components
import Yaifl.Common
import Yaifl.Utils
import Colog
import qualified Data.Text as T
import qualified Data.Text.Prettyprint.Doc.Render.Terminal as PPTTY

-- | Create a placeholder rule (no rulebook variables).
makeBlankRule :: Monad m => Text -> Rule w () m a
makeBlankRule n = Rule n (do
    logError $ n <> " needs implementing"
    return Nothing)

-- | Create a placeholder rule (with rulebook variables).
makeBlankRuleWithVariables :: WithGameLog w m => Text -> Rule w v m a
makeBlankRuleWithVariables n = RuleWithVariables n (do
    logError $ n <> " needs implementing"
    return Nothing)

-- | Create a rulebook (no rulebook variables) from a name and a list of rules.
makeRulebook :: Text -> [Rule w () m a] -> Rulebook w () m a
makeRulebook n = Rulebook n Nothing

-- | Create a rulebook (with rulebook variables) from a name and a list of rules. 
makeRulebookWithVariables :: Monad m => Text -> [Rule w v m a] -> v -> Rulebook w v m a
makeRulebookWithVariables n rs v = RulebookWithVariables n Nothing (return $ Just v) rs

logRulebookName :: (MonadReader env m, WithLog env Message m) => Text -> m ()
logRulebookName n = unless (n == "") (do logDebug $ "Following the " <> n; pass)

addRulebook :: Monad m => Rulebook w v m RuleOutcome -> World w m ()
addRulebook r = do
    rulebookStore . at (rulebookName r) ?= BoxedRulebook r

runRulebook :: (Show v, WithGameLog w m) => Rulebook w s m v -> World w m (Maybe v)
runRulebook (RulebookWithVariables n def i r) = if null r then pure def else
        do
            logRulebookName n
            --attempt to set the rulebook variables
            iv <- i
            res <- maybe --if we were not able to set them
                    (do
                        logError "Could not successfully parse arguments"
                        return def)
                    (processRuleList r def) iv
            logDebug $ "Finished following the " <> n <> " with result " <> maybe "nothing" show res
            return $ res <|> def
            
                
runRulebook (Rulebook n def r) = if null r then pure def else
    do
        logRulebookName n
        res <- doUntilJustM (\case
                Rule rn rf -> do
                    --unless (rn == "") (logDebug $ "Following thee " <> rn)
                    rf
                RuleWithVariables _ _ -> do
                    logError "Hit argument rule in rulebook without args"
                    return def) r
        logDebug $ "Finished following the " <> n <> " with result " <> maybe "nothing" show res
        return $ res <|> def
processRuleList :: Monad m => [Rule w v m a] -> Maybe a -> v ->  World w m (Maybe a)
processRuleList [] _ _ = return Nothing
processRuleList (x:xs) def args  = case x of
            Rule _ _ -> do
                logError "Hit argumentless rule in rulebook with args"
                return def
            RuleWithVariables rn (RuleVarsT rf) -> do
                --unless (rn == "") (logInfo $ "Following the " <> rn)
                let v = runStateT rf args
                (res, s) <- v
                maybe (processRuleList xs def s) (return . Just) res
whenPlayBeginsName :: Text
whenPlayBeginsName = "when play begins rules"

whenPlayBeginsRules :: (HasStore w Enclosing, HasStore w Player, HasThing w, WithGameLog w m) => PlainRulebook w m
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

sayIntroText :: WithGameLog w m => World w m ()
sayIntroText = do
    setStyle (Just (PPTTY.color PPTTY.Green <> PPTTY.bold))
    t <- use title
    mapM_ say (introText t) --replace w/say
    --setStyle Nothing
    pass

actionProcessingRulebookName = "action processing rulebook"

tryAction :: (HasStore w Player, Monad m) => Text -> [Entity] -> World w m Bool
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
        ap act args
        ) ac
    logDebug $ "Action completed with result " <> show res
    return res

defaultActionProcessingRules :: Monad m => BoxedAction w m -> [Entity] -> World w m RuleOutcome
defaultActionProcessingRules (BoxedAction a) args = do
    res <- runRulebook (actionProcessingRulebookImpl a args)
    return $ fromMaybe False res

actionProcessingRulebookImpl :: (Show v, Monad m) => Action w v m -> [Entity] -> Rulebook w v m RuleOutcome
actionProcessingRulebookImpl a args = RulebookWithVariables
    actionProcessingRulebookName
    (Just True)
    (if length args == _appliesTo a then runRulebook (_setActionVariables a args) else return Nothing)
    (actionProcessingRules a)

actionProcessingRules :: (WithGameLog w m, Monad m) => Action w v m -> [Rule w v m Bool]
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
            lift $ runRulebook (_carryOutActionRules a v)),
        makeBlankRuleWithVariables "after stage rule",
        makeBlankRuleWithVariables "investigate player awareness after rule",
        makeBlankRuleWithVariables "report stage rule",
        makeBlankRuleWithVariables "clean actions rule"]
