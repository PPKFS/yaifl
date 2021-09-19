module Yaifl.Rulebooks
  ( whenPlayBeginsRules,
    addWhenPlayBegins,
    defaultActionProcessingRules,
    introText,
    runRulebook,
    noArgs,

    makeRule,
    ruleEnd,
  )
where

import Control.Lens
import qualified Data.Text as T
import qualified Data.Text.Prettyprint.Doc.Render.Terminal as PPTTY
import Relude
import Yaifl.Common
import Yaifl.Messages
import Yaifl.Objects

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
-- | Rule smart constructor for rules that do not have any arguments (else you'd have
-- to write rule bodies with a _ -> prefixed on).
makeRule
  :: Text -- ^ Rule name.
  -> (World t ro c -> (Maybe r, World t ro c)) -- ^ Rule function.
  -> Rule t ro c () r
makeRule n f = Rule n (\_ w -> first ((),) $ f w)

-- | Append this to the end of a rule to remove any unnecessary return values
ruleEnd
  :: World t0 r0 c0
  -> (Maybe r, World t0 r0 c0)
ruleEnd = (Nothing,)

defaultActionProcessingRules
  :: Action t r c
  -> UnverifiedArgs t r c
  -> World t r c
  -> (Maybe Bool, World t r c)
defaultActionProcessingRules _ _ = (Nothing,)

-- | Attempt to run an action from a text command (so will handle the parsing).
-- Note that this does require the arguments to be parsed out.
tryAction
  :: Text -- ^ text of command
  -> (Timestamp -> UnverifiedArgs t r c) -- ^ Arguments without a timestamp
  -> World t r c
  -> (Bool, World t r c)
tryAction an a = runState $ do
  ta <- gets (a . getGlobalTime)
  ac <- gets (getAction an ta)
  -- either run the action if we parsed it successfully, or
  -- pipe through the error message
  res <- maybe (return Nothing) (state . runAction ta) ac
  unless (isJust res) (modify $ logError ("Couldn't find a matching action for " <> an))
  return $ isNothing res

getAction
  :: Text
  -> UnverifiedArgs t r c
  -> World t r c
  -> Maybe (Action t r c)
getAction _ _ _ = Nothing

-- | Run an action. This assumes that all parsing has been completed.
runAction
  :: UnverifiedArgs t r c
  -> Action t r c
  -> World t r c
  -> (Maybe Bool, World t r c)
runAction args act w = _actionProcessing w act args w

-- | Run a rulebook. Mostly this just adds some logging baggage.
runRulebook
  :: Rulebook t ro c v re
  -> (Timestamp -> UnverifiedArgs t ro c)
  -> World t ro c
  -> (Maybe re, World t ro c)
runRulebook Rulebook{..} args = runState $ do
  ta <- gets (args . getGlobalTime)
  modify $ logInfo $ "Following the " <> _rbName <> " rulebook"
  modify $ addLogContext _rbName
  w <- get
  let argParse = _rbParseArguments ta w
  -- TODO: logging
  res <- maybe (return Nothing) (state . processRuleList _rbRules) argParse
  modify $ logInfo $ "Finished the " <> _rbName <> " rulebook"
  modify popLogContext
  return $ res <|> _rbDefaultOutcome

-- | Mostly this is a very complicated "run a list of functions until you get
-- something that isn't a Nothing, or a default if you get to the end".
processRuleList
  :: [Rule t ro c v re]
  -> v
  -> World t ro c
  -> (Maybe re, World t ro c)
processRuleList [] _ = (Nothing,)
processRuleList (x : xs) args = runState $ do
        unless (_ruleName x == "")
          (modify $ logVerbose $ "Following the " <> _ruleName x)
        (v, res) <- state $ _runRule x args
        -- if we hit nothing, continue; otherwise return
        whenJust res (const $ modify $ logVerbose $ "Finishing rulebook on rule " <> _ruleName x)
        state (\w' -> maybe
          (processRuleList xs v w')
          (\r -> (Just r, w')) res)

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
whenPlayBeginsName :: Text
whenPlayBeginsName = "When Play Begins"

-- | The rulebook that runs at the start of the game.
whenPlayBeginsRules :: Rulebook t r c () Bool
whenPlayBeginsRules = Rulebook
    whenPlayBeginsName
    Nothing
    (const $ const (Just ()))
    [ makeRule "display banner rule" $ ruleEnd . sayIntroText
    , makeRule "position player in world rule" positionPlayer
    , makeRule "initial room description rule" initRoomDescription
    ]

initRoomDescription
  :: World t r c
  -> (Maybe a, World t r c)
initRoomDescription = first (const Nothing) . tryAction "looking" playerNoArgs

-- | No Arguments, player source.
playerNoArgs
  :: Timestamp
  -> UnverifiedArgs t ro c
playerNoArgs = withPlayerSource <$> noArgs

-- | No Arguments, no source.
noArgs
  :: Timestamp
  -> UnverifiedArgs t ro c
noArgs = Args Nothing []

withPlayerSource
  :: Args t r c v
  -> Args t r c v
withPlayerSource = error "not implemented"

positionPlayer
  :: World t r c
  -> (Maybe Bool, World t r c)
positionPlayer w = case _firstRoom w of
  Nothing -> failRuleWithError "No rooms have been made, so cannot place the player." w
  Just fr -> move (getPlayer w) fr w

-- | Return a failure (Just False) from a rule and log a string to the
-- debug log.
failRuleWithError
  :: Text -- ^ Error message.
  -> World t r c
  -> (Maybe Bool, World t r c)
failRuleWithError t w = (Just False, logError t w)

getPlayer
  :: World t r c
  -> Entity
getPlayer = error "not implemented"

sayIntroText
  :: World t r c
  -> World t r c
sayIntroText = execState $ do
  modify $ setSayStyle (Just (PPTTY.color PPTTY.Green <> PPTTY.bold))
  t <- gets _title
  modify (say $ introText t)
  modify $ setSayStyle Nothing
  pass

introText
  :: Text
  -> Text
introText w = fold
  [ longBorder <> "\n"
  , shortBorder <> " " <> w <> " " <> shortBorder <> "\n"
  , longBorder <> "\n\n"
  ]
  where
    shortBorder = "------"
    longBorder = mconcat $ replicate
      (2 * T.length shortBorder + T.length w + 2) "-"

addWhenPlayBegins
  :: Rule t r c () Bool
  -> State (World t r c) ()
addWhenPlayBegins r = whenPlayBegins %= addRule r

addRule
  :: Rule t ro c v r
  -> Rulebook t ro c v r
  -> Rulebook t ro c v r
addRule r = rbRules %~ (++ [r])

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
