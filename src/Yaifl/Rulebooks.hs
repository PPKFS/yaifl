module Yaifl.Rulebooks
(
    makeBlankRule, compileRulebook
  , whenPlayBeginsName, whenPlayBeginsRules, introText
) where

import Yaifl.Prelude
import Yaifl.Common
import Yaifl.Utils
import Colog
import qualified Data.Text as T
import qualified Data.Text.Prettyprint.Doc     as PP
import qualified Data.Text.Prettyprint.Doc.Render.Terminal as PPTTY

makeBlankRule :: Monad m => Text -> PlainRule w m
makeBlankRule n = Rule n (do
    logInfo $ n <> " needs implementing"
    return Nothing)



makeBlankRuleWithVariables :: WithGameLog w m => Text -> Rule w v m a
makeBlankRuleWithVariables n = RuleWithVariables n (do
    lift $ logInfo $ n <> " needs implementing"
    return Nothing)

makeRulebook :: Text -> [Rule w () m a] -> Rulebook w () m a
makeRulebook n = Rulebook n Nothing

logRulebookName :: (MonadReader env m, WithLog env Message m) => Text -> m ()
logRulebookName n = do
    unless (n == "") $ logInfo $ "Following the " <> n <> " rulebook"
    --todo: add context?
    unless (n == "") pass

compileRulebook :: WithGameLog w m => Rulebook w s m Bool -> World w m (Maybe Bool)
compileRulebook (RulebookWithVariables n def i r) = if null r then pure def else
        do
            logRulebookName n
            iv <- i
            maybe (return (Just False)) (\args -> do
                let f = doUntilJustM (\case 
                        RuleWithVariables rn rf -> do
                            unless (rn == "") (lift $ logInfo $ "Following the " <> rn)
                            rf
                        Rule _ _ -> do
                            lift $ logError "Hit argumentless rule in rulebook with args"
                            return def) r
                res <- evalStateT (unwrapRuleVars f) args 
                logInfo $ "Finished following the " <> n <> " with result " <> maybe "nothing" show res
                return $ res <|> def) iv

compileRulebook (Rulebook n def r) = if null r then pure def else
    do
        logRulebookName n
        res <- doUntilJustM (\case 
                Rule rn rf -> do
                    unless (rn == "") (logInfo $ "Following the " <> rn)
                    rf
                RuleWithVariables _ _ -> do
                    logError "Hit argument rule in rulebook without args"
                    return def) r
        logInfo $ "Finished following the " <> n <> " with result " <> maybe "nothing" show res
        return $ res <|> def

whenPlayBeginsName :: Text
whenPlayBeginsName = "when play begins rules"

whenPlayBeginsRules :: Monad m => PlainRulebook w m
whenPlayBeginsRules = makeRulebook whenPlayBeginsName [
            Rule "display banner rule" (do
                sayIntroText
                return Nothing),
            Rule "position player in model world rule" (do
                w <- get
                maybe (logError "first room never set.") (move (getPlayer' w)) (w ^. gameInfo . firstRoom)
                return Nothing){-,
            Rule "initial room description rule" (do
                tryAction lookingActionName []
                return Nothing)-}
        ]

introText :: Text -> [Text]
introText w = [longBorder<>"\n", shortBorder <> " " <> w <> " " <> shortBorder <> "\n", longBorder<>"\n\n"]  
            where shortBorder = "------"
                  longBorder = foldr (<>) "" $ replicate (2 * T.length shortBorder + T.length w + 2) ("-" :: Text)
--TODO; TYPE ALIAS THIS WHOLE LOGGING, MONAD M THING
sayIntroText :: WithGameLog w m => World w m ()
sayIntroText = do
    setStyle (Just (PPTTY.color PPTTY.Green <> PPTTY.bold))
    t <- use title
    mapM_ say (introText t) --replace w/say
    --setStyle Nothing
    pass
{-
compileRulebook' :: HasMessageBuffer w => UncompiledRulebook w r -> RulebookDebugPrinting -> Rulebook w
compileRulebook' r db = CompiledRulebook (do
    let (CompiledRulebook cr) = compileRulebook r db
    w <- get
    let iv = _rulebookInit r w
    zoomOut cr iv)

compileRulebook :: HasMessageBuffer w => UncompiledRulebook w r -> RulebookDebugPrinting -> Rulebook (w, r)
compileRulebook (Rulebook n initVars rs) db = CompiledRulebook (
    if null rs then return Nothing else (do
    (w, _) <- get
    unless (n == "" || db == Silent) $ sayDbgLn $ "Following the " <> n
    indentDbg True
    let iv = initVars w
    _2 .= iv
    result <- doUntilJustM (\r1 -> do
        unless (fst r1 == "" || db == Silent) $ sayDbgLn $ "Following the " <> fst r1
        snd r1
        ) rs
    indentDbg False
    unless (db == Silent) $ sayDbgLn $ "Finished following the " <> n <> " with result " <> maybe "nothing" show result
    return result))
-}