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

import qualified Data.Text as T
import qualified Data.Text.Prettyprint.Doc.Render.Terminal as PPTTY
import Yaifl.Prelude
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
  -> (World o -> (Maybe r, World o)) -- ^ Rule function.
  -> Rule o () r
makeRule n f = Rule n (\_ w -> first ((),) $ f w)

-- | Append this to the end of a rule to remove any unnecessary return values
ruleEnd
  :: World o
  -> (Maybe r, World o)
ruleEnd = (Nothing,)

defaultActionProcessingRules
  :: Action o
  -> UnverifiedArgs o
  -> World o
  -> (Maybe Bool, World o)
defaultActionProcessingRules Action{..} u = runRulebook (Rulebook 
  "Action Processing" 
  (Just True) 
  _actionParseArguments
  -- v -> World s -> ((v, Maybe r), World s)
  [
    Rule "before stage rule" (\a w -> ((a, Nothing), w))

  ]) (const u)

{-

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
-- | Attempt to run an action from a text command (so will handle the parsing).
-- Note that this does require the arguments to be parsed out.
tryAction
  :: Text -- ^ text of command
  -> (Timestamp -> UnverifiedArgs o) -- ^ Arguments without a timestamp
  -> World o
  -> (Bool, World o)
tryAction an a = runState $ do
  ta <- gets (a . getGlobalTime)
  ac <- gets (getAction an ta)
  unless (isJust ac) (logError ("Couldn't find a matching action for " <> an))
  -- either run the action if we parsed it successfully, or
  -- pipe through the error message
  res <- maybe (return Nothing) (state . runAction ta) ac
  return $ isNothing res

getAction
  :: Text
  -> UnverifiedArgs o
  -> World o
  -> Maybe (Action o)
getAction n _ w = w ^. actions % at n

-- | Run an action. This assumes that all parsing has been completed.
runAction
  :: UnverifiedArgs o
  -> Action o
  -> World o
  -> (Maybe Bool, World o)
runAction args act w = _actionProcessing w act args w

-- | Run a rulebook. Mostly this just adds some logging baggage.
runRulebook
  :: Rulebook o ia v re
  -> (Timestamp -> ia)
  -> World o
  -> (Maybe re, World o)
runRulebook Rulebook{..} args = runState $ do
  ta <- gets (args . getGlobalTime)
  modify $ addLogContext _rbName
  logInfo $ "Following the " <> _rbName <> " rulebook"
  argParse <- gets $ _rbParseArguments ta
  -- TODO: logging
  res <- maybe (return Nothing) (state . processRuleList _rbRules) argParse
  logInfo $ "Finished the " <> _rbName <> " rulebook"
  modify popLogContext
  return $ res <|> _rbDefaultOutcome

-- | Mostly this is a very complicated "run a list of functions until you get
-- something that isn't a Nothing, or a default if you get to the end".
processRuleList
  :: [Rule o v re]
  -> v
  -> World o
  -> (Maybe re, World o)
processRuleList [] _ = (Nothing,)
processRuleList (x : xs) args = runState $ do
        unless (_ruleName x == "")
          (logVerbose $ "Following the " <> _ruleName x)
        (v, res) <- state $ _runRule x args
        -- if we hit nothing, continue; otherwise return
        whenJust res (const $ logVerbose $ "Finishing rulebook on rule " <> _ruleName x)
        state (\w' -> maybe
          (processRuleList xs v w')
          (\r -> (Just r, w')) res)

whenPlayBeginsName :: Text
whenPlayBeginsName = "When Play Begins"

-- | The rulebook that runs at the start of the game.
whenPlayBeginsRules
  :: HasProperty s Enclosing
  => Rulebook s () () Bool
whenPlayBeginsRules = Rulebook
    whenPlayBeginsName
    Nothing
    (const $ const (Just ()))
    [ makeRule "display banner rule" $ ruleEnd . sayIntroText
    , makeRule "position player in world rule" (runState positionPlayer)
    , makeRule "initial room description rule" initRoomDescription
    ]

initRoomDescription
  :: World o
  -> (Maybe a, World o)
initRoomDescription = first (const Nothing) . tryAction "looking" playerNoArgs

-- | No Arguments, player source.
playerNoArgs
  :: Timestamp
  -> UnverifiedArgs o
playerNoArgs = withPlayerSource <$> noArgs

-- | No Arguments, no source.
noArgs
  :: Timestamp
  -> UnverifiedArgs o
noArgs = Args Nothing []

withPlayerSource
  :: Args o v
  -> Args o v
withPlayerSource = error "not implemented"

positionPlayer
  :: HasProperty s Enclosing
  => State (World s) (Maybe Bool)
positionPlayer = do
  fr <- gets _firstRoom
  pl <- gets getPlayer
  case fr of
    Nothing -> state $ failRuleWithError 
      "No rooms have been made, so cannot place the player."
    Just fr' -> do
      m <- move pl fr'
      if m then return Nothing else state $ failRuleWithError "Failed to move the player."

-- | Return a failure (Just False) from a rule and log a string to the
-- debug log.
failRuleWithError
  :: Text -- ^ Error message.
  -> World o
  -> (Maybe Bool, World o)
failRuleWithError t w = (Just False, execState (logError t) w)

getPlayer
  :: World o
  -> Entity
getPlayer = _currentPlayer

sayIntroText
  :: World o
  -> World o
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
  :: Rule o () Bool
  -> State (World o) ()
addWhenPlayBegins r = whenPlayBegins %= addRule r

addRule
  :: Rule o v r
  -> Rulebook o ia v r
  -> Rulebook o ia v r
addRule r = rbRules %~ (++ [r])