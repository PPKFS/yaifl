module Yaifl.Rulebooks
(
    makeRulebook', makeRulebook, makeRule', makeRule, makeBlankRule,
    compileRulebook', compileRulebook
) where

import Relude
import Yaifl.Components
import Control.Lens
import Yaifl.Say
import Yaifl.Utils
import Yaifl.Common
import qualified Data.Text as Text
import qualified Data.Text.Prettyprint.Doc.Render.Terminal as PPTTY

makeRule' :: Text -> RuleEvaluation w -> (Text, RuleEvaluation (w, ()))
makeRule' n f = makeRule n (zoom _1 f)

makeRule :: Text -> RuleEvaluation (w, r) -> (Text, RuleEvaluation (w, r))
makeRule = (,)

makeBlankRule :: Text -> (Text, RuleEvaluation (w, r))
makeBlankRule n = makeRule n (return Nothing)

makeRulebook' :: Text -> [(Text, RuleEvaluation (w, ()))] -> UncompiledRulebook w ()
makeRulebook' n r = makeRulebook n () r--(map (second $ zoom _1) r)

makeRulebook :: Text -> r -> [(Text, RuleEvaluation (w, r))] -> UncompiledRulebook w r
makeRulebook n ini = Rulebook n (const ini)

compileRulebook' :: HasMessageBuffer w => UncompiledRulebook w r -> Rulebook w
compileRulebook' r = CompiledRulebook (do
    let (CompiledRulebook cr) = compileRulebook r
    w <- get
    let iv = _rulebookInit r w
    zoomOut cr iv)

compileRulebook :: HasMessageBuffer w => UncompiledRulebook w r -> Rulebook (w, r)
compileRulebook (Rulebook n initVars rs) = CompiledRulebook (
    if null rs then return Nothing else (do
    (w, _) <- get
    unless (n == "") $ sayDbgLn $ "Following the " <> n
    indentDbg True
    let iv = initVars w
    _2 .= iv
    result <- doUntilJustM (\r1 -> do
        unless (fst r1 == "") $ sayDbgLn $ "Following the " <> fst r1
        snd r1
        ) rs
    indentDbg False
    sayDbgLn $ "Finished following the " <> n <> " with result " <> maybe "nothing" show result
    return result))

whenPlayBeginsName :: Text
whenPlayBeginsName = "when play begins rules"

whenPlayBeginsRulesImpl :: (HasMessageBuffer w, HasWorld' w, Has w Physical, Has w Object,
                                Has w Player, Has w Enclosing) => UncompiledRulebook w ()
whenPlayBeginsRulesImpl = makeRulebook' whenPlayBeginsName [
            makeRule' "display banner rule" (do
                sayIntroText
                return Nothing),
                
            makeRule' "position player in model world rule" (do
                w <- get
                _ <- maybe (error "first room never set.") (move (getPlayer' w)) (w ^. gameInfo . firstRoom)
                return Nothing),
            makeRule' "initial room description rule" (do
                tryAction lookingActionName []
                return Nothing)
        ]

introText :: Text -> [Text]
introText w = [longBorder<>"\n", shortBorder <> " " <> w <> " " <> shortBorder<>"\n", 
                longBorder<>"\n\n"]
            where shortBorder = "------" 
                  totalLength = 2 * Text.length shortBorder + Text.length w + 2
                  longBorder = foldr (<>) "" $ replicate totalLength ("-" :: Text)

sayIntroText :: (HasMessageBuffer u, HasGameInfo u w) => System u ()
sayIntroText = do
    w <- get
    setStyle (Just (PPTTY.color PPTTY.Green <> PPTTY.bold))
    mapM_ say (introText $ w ^. gameInfo . title)
    setStyle Nothing
    pass