module Yaifl.Rulebooks (
   whenPlayBeginsRules
,  addWhenPlayBegins
, defaultActionProcessingRules
, introText
, runRulebook
, noArgs
) where

import Yaifl.Common
import Relude
import Control.Lens
import qualified Data.Text.Prettyprint.Doc.Render.Terminal as PPTTY
import qualified Data.Text as T
{-
whenPlayBeginsRules :: (HasStore w Enclosing, HasStore w Player, HasThing w) => Rulebook w () RuleOutcome
whenPlayBeginsRules =
    makeRulebook
        whenPlayBeginsName
        [ Rule
            "display banner rule"
            ( do
                sayIntroText
                return Nothing
            )
        , Rule
            "position player in model world rule"
            ( do
                fr <- use firstRoom
                v <- maybe (return False) movePlayer' fr
                unless v (logError "The first room was never set.")
                return Nothing
            )
        , Rule
            "initial room description rule"
            ( do
                tryAction "looking" []
                return Nothing
            )
        ]
instance LoggableFailure (Either Text a) where
    logErrorToBool e r = runState $ (either (\l -> do
        modify $ sayLn l
        return False) (return True) e) r
-}


defaultActionProcessingRules :: Action t r c -> UnverifiedArgs t r c -> World t r c -> (Maybe Text, World t r c)
defaultActionProcessingRules a u w = error ""

tryAction :: Text -> (Timestamp -> UnverifiedArgs t r c) -> World t r c -> (Bool, World t r c)
tryAction an a = runState $ do
    ta <- gets (a . getGlobalTime)
    ac <- gets (getAction an ta)
    res <- either (return . Just) (state . runAction ta) ac
    maybe (return True) (\l -> do
        modify $ sayLn l
        return False) res

getAction :: Text -> UnverifiedArgs t r c -> World t r c -> Either Text (Action t r c)
getAction = error "not implemented"

runAction :: UnverifiedArgs t r c -> Action t r c -> World t r c -> (Maybe Text, World t r c)
runAction args act = runState $ do
    ap <- gets _actionProcessing
    state $ ap act args

runRulebook :: Rulebook t ro c v re -> (Timestamp -> UnverifiedArgs t ro c) -> World t ro c -> (Maybe re, World t ro c)
runRulebook Rulebook{..} args w = runState (do
    ta <- gets (args . getGlobalTime)
    let parsedArgs = _parseRulebookArguments ta w
    res <- either (\l -> do
        modify $ sayLn l
        return Nothing) (state . processRuleList _rules ) parsedArgs
    return $ res <|> _defaultOutcome) w

processRuleList :: [Rule t ro c v re] -> v -> World t ro c -> (Maybe re, World t ro c)
processRuleList [] _ w = (Nothing, w)
processRuleList (x : xs) args w = runState (do
        unless (_ruleName x == "") (modify $ sayLn $ "Following the " <> _ruleName x)
        (v, res) <- state $ _runRule x args
        state (\w' -> maybe (processRuleList xs v w') (\r -> (Just r, w')) res)) w
{-
    ac <- use $ actionStore . at action
    ap <- use actionProcessing
    res <-
        maybe
            ( do
                logError $ "Couldn't find the action called " <> action
                return False
            )
            ( \act -> do
                logDebug $ "Running action called " <> action
                p <- getPlayer'
                currentActionVars .= (p, args)
                liftWorld $ ap act args
            )
            ac
    logDebug $ "Action completed with result " <> show res
    return res
-}
whenPlayBeginsName = "when play begins rules"
whenPlayBeginsRules :: Rulebook t r c () Text
whenPlayBeginsRules = Rulebook whenPlayBeginsName Nothing (const $ const $ Right ()) [
      makeRule "display banner rule" $ ruleEnd . sayIntroText
    , makeRule "position player in world rule" positionPlayer
    , makeRule "initial room description rule" $ first (const Nothing) . tryAction "looking" playerNoArgs]

playerNoArgs :: Timestamp -> UnverifiedArgs t ro c
playerNoArgs = withPlayerSource <$> noArgs

noArgs :: Timestamp -> UnverifiedArgs t ro c
noArgs = Args Nothing []

withPlayerSource :: Args t r c v -> Args t r c v
withPlayerSource = error "not implemented"
positionPlayer :: World t r c -> (Maybe Text, World t r c)
positionPlayer = error "not implemented"

sayIntroText :: World t r c -> World t r c
sayIntroText = execState $ do
    modify $ setStyle (Just (PPTTY.color PPTTY.Green <> PPTTY.bold))
    t <- gets _title
    modify (say $ introText t)
    modify $ setStyle Nothing
    pass

introText :: Text -> Text
introText w = fold [longBorder <> "\n", shortBorder <> " " <> w <> " " <> shortBorder <> "\n", longBorder <> "\n\n"]
  where
    shortBorder = "------"
    longBorder = foldr (<>) "" $ replicate (2 * T.length shortBorder + T.length w + 2) ("-" :: Text)

addWhenPlayBegins :: Rule t r c () Text -> World t r c -> World t r c
addWhenPlayBegins = over whenPlayBegins . addRule

addRule :: Rule t ro c v r -> Rulebook t ro c v r -> Rulebook t ro c v r
addRule r = rules %~ (++ [r])
{-
actionProcessingRulebookImpl :: Action w v -> [Entity] -> Rulebook w v RuleOutcome
actionProcessingRulebookImpl a args =
    RulebookWithVariables
        actionProcessingRulebookName
        (Just True)
        (if _appliesTo a (length args) then runRulebook (_setActionVariables a args) else return Nothing)
        (actionProcessingRules a)

actionProcessingRules :: Action w v -> [Rule w v Bool]
actionProcessingRules a =
    [ makeBlankRuleWithVariables "before stage rule"
    , makeBlankRuleWithVariables "carrying requirements rule"
    , makeBlankRuleWithVariables "basic visibility rule"
    , makeBlankRuleWithVariables "instead stage rule"
    , makeBlankRuleWithVariables "requested actions require persuasion rule"
    , makeBlankRuleWithVariables "carry out requested actions rule"
    , makeBlankRuleWithVariables "investigate player awareness rule"
    , makeBlankRuleWithVariables "check stage rule"
    , RuleWithVariables
        "carry out stage rule"
        (
            getRulebookVariables >>= runRulebook . _carryOutActionRules a
        )
    , makeBlankRuleWithVariables "after stage rule"
    , makeBlankRuleWithVariables "investigate player awareness after rule"
    , makeBlankRuleWithVariables "report stage rule"
    , makeBlankRuleWithVariables "clean actions rule"
    ]
-}