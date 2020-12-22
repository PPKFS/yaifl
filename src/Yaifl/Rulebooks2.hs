module Yaifl.Rulebooks2
(
    makeRulebook', makeRule', 
    whenPlayBeginsName, whenPlayBeginsRules, introText,

    --whenPlayBeginsRulesImpl
) where

import Yaifl.Prelude
import Yaifl.Components2
import Yaifl.PolysemyOptics
import Yaifl.Say2
import Yaifl.Common2
import Polysemy.State
import Yaifl.World
import qualified Data.Text as Text
import qualified Data.Text.Prettyprint.Doc.Render.Terminal as PPTTY
{-
makeRule' :: Text -> RuleEvaluation w -> (Text, RuleEvaluation (w, ()))
makeRule' n f = makeRule n (zoom _1 f)



makeBlankRule :: Text -> (Text, RuleEvaluation (w, r))
makeBlankRule n = makeRule n (return Nothing)
-}


makeRule' :: Text -> RuleEvaluation r -> PlainRule r
makeRule' = (. NormalRule) . Rule

makeRulebook' :: Text -> [PlainRule r] -> PlainRulebook r
makeRulebook' n = Rulebook n Nothing pass

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
