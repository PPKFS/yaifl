module Yaifl.Rulebooks
(
    makeRulebook', makeRule',
    whenPlayBeginsName, whenPlayBeginsRules, introText
) where

import Yaifl.Prelude
import Yaifl.PolysemyOptics
import Yaifl.Say
import Yaifl.Common
import Yaifl.Utils
import Polysemy.State
import qualified Data.Text as Text
import qualified Data.Text.Prettyprint.Doc.Render.Terminal as PPTTY

makeRule' :: Text -> RuleEvaluation w -> PlainRule w
makeRule' = Rule

makeRulebook' :: Text -> [PlainRule r] -> PlainRulebook r
makeRulebook' n = Rulebook n Nothing
{-

data Rule w v r = Rule Text (Sem (State v ': SemWorldList w) (Maybe r))

-- | a rulebook runs in a monadic context m with rulebook variables v and returns a value r, which is normally a success/fail
data Rulebook w v r = Rulebook
    {
    -- | printed name of the rulebook
      _rulebookName :: Text
    , _defaultOutcome :: Maybe r
    -- | a way to construct the initial rulebook variables
    ,  _rulebookInit :: SemWorld w v
    -- | the list of rules and their name
    , _rules :: [Rule w v r]
    }

type PlainRule w = Rule w () RuleOutcome
-}

compileRulebook :: Rulebook w v RuleOutcome -> RuleEvaluation w
compileRulebook (RulebookWithVariables n def i r) = if null r then pure def else
    do
        unless (n == "") $ logMsg Info $ "Following the " <> n <> " rulebook"
        unless (n == "") $ addContext n (Just $ PPTTY.color PPTTY.Blue)
        iv <- i
        res <- evalState iv (doUntilJustM (\case 
                RuleWithVariables rn rf -> do
                    unless (rn == "") (logMsg Info $ "Following the " <> rn)
                    rf
                Rule _ _ -> do
                    logMsg Error "Hit argumentless rule in rulebook with args"
                    return def) r)
        logMsg Info $ "Finished following the " <> n <> " with result " <> maybe "nothing" show res
        return $ res <|> def
compileRulebook (Rulebook n def r) = if null r then pure def else
    do
        unless (n == "") $ logMsg Info $ "Following the " <> n <> " rulebook"
        unless (n == "") $ addContext n (Just $ PPTTY.color PPTTY.Blue)
        res <- doUntilJustM (\case 
                Rule rn rf -> do
                    unless (rn == "") (logMsg Info $ "Following the " <> rn)
                    rf
                RuleWithVariables _ _ -> do
                    logMsg Error "Hit argument rule in rulebook without args"
                    return def) r
        logMsg Info $ "Finished following the " <> n <> " with result " <> maybe "nothing" show res
        return $ res <|> def

    {-CompiledRulebook (do
    let (CompiledRulebook cr) = compileRulebook r db
    w <- get
    let iv = _rulebookInit r w
    zoomOut cr iv)-}

whenPlayBeginsName :: Text
whenPlayBeginsName = "when play begins rules"

whenPlayBeginsRules :: PlainRulebook w
whenPlayBeginsRules = makeRulebook' whenPlayBeginsName [
            makeRule' "display banner rule" (do
                sayIntroText
                return Nothing){-,
            makeRule' "position player in model world rule" (do
                w <- get
                _ <- maybe (error "first room never set.") (move (getPlayer' w)) (w ^. gameInfo . firstRoom)
                return Nothing),
            makeRule' "initial room description rule" (do
                tryAction lookingActionName []
                return Nothing)-}
        ]

introText :: Text -> [Text]
introText w = [longBorder<>"\n", shortBorder <> " " <> w <> " " <> shortBorder<>"\n",
                longBorder<>"\n\n"]
            where shortBorder = "------"
                  totalLength = 2 * Text.length shortBorder + Text.length w + 2
                  longBorder = foldr (<>) "" $ replicate totalLength ("-" :: Text)

sayIntroText :: HasGameSettings w r => Sem r ()
sayIntroText = do
    setStyle (Just (PPTTY.color PPTTY.Green <> PPTTY.bold))
    t <- use title
    mapM_ say (introText t)
    setStyle Nothing
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