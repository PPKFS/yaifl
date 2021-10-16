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

blankRule
  :: Text
  -> Rule o v r
blankRule n = Rule n (\a w -> ((a, Nothing), execState (logVerbose (n <> " needs implementing")) w))

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
defaultActionProcessingRules Action{..} = runRulebook (Rulebook
  "Action Processing"
  (Just True)
  (\uv w -> (\x -> fmap (const x) uv) <$> _actionParseArguments uv w)
  -- v -> World s -> ((v, Maybe r), World s)
  [ blankRule "Before stage rule"
  , blankRule "carrying requirements rule"
  , blankRule "basic visibility rule"
  , blankRule "instead stage rule"
  , blankRule "requested actions require persuasion rule"
  , blankRule "carry out requested actions rule"
  , blankRule "investigate player awareness rule"
  , blankRule "check stage rule"
  --v -> World s -> ((v, Maybe r), World s)
  -- Rulebook o (Args o v) (Args o v) RuleOutcome -> Args o v -> World o -> (Maybe RuleOutcome, World o)
  , Rule "carry out stage rule"
        ( \v w -> first (fromMaybe (v, Nothing)) $ runRulebookAndReturnVariables _actionCarryOutRules v w)
  , blankRule "after stage rule"
  , blankRule "investigate player awareness after rule"
  , blankRule "report stage rule"
  , blankRule "clean actions rule"
  ])

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
  -> ia
  -> World o
  -> (Maybe re, World o)
runRulebook rb ia w = first (snd =<<) $ runRulebookAndReturnVariables rb ia w

runRulebookAndReturnVariables
  :: Rulebook o ia v re
  -> ia
  -> World o
  -> (Maybe (v, Maybe re), World o)
runRulebookAndReturnVariables Rulebook{..} args = runState $ do
  modify $ addLogContext _rbName
  logInfo $ "Following the " <> _rbName <> " rulebook"
  argParse <- gets $ _rbParseArguments args
  -- TODO: logging
  res <- maybe (return Nothing) (\x -> do
    (v, r1) <- (state . processRuleList _rbRules) x
    return $ Just (v, r1)) argParse
  logInfo $ "Finished the " <> _rbName <> " rulebook"
  modify popLogContext
  return $ (\(v, r1) -> Just (v, r1 <|> _rbDefaultOutcome)) =<< res

-- | Mostly this is a very complicated "run a list of functions until you get
-- something that isn't a Nothing, or a default if you get to the end".
processRuleList
  :: [Rule o v re]
  -> v
  -> World o
  -> ((v, Maybe re), World o)
processRuleList [] v = ((v, Nothing),)
processRuleList (x : xs) args = runState $ do
        unless (_ruleName x == "")
          (logVerbose $ "Following the " <> _ruleName x)
        (v, res) <- state $ _runRule x args
        -- if we hit nothing, continue; otherwise return
        whenJust res (const $ logVerbose $ "Finishing rulebook on rule " <> _ruleName x)
        state (\w' -> maybe
          (processRuleList xs v w')
          (\r -> ((v, Just r), w')) res)

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
initRoomDescription w = (Nothing, snd $ tryAction "looking" (playerNoArgs w) w)

-- | No Arguments, player source.
playerNoArgs
  :: World s
  -> Timestamp
  -> UnverifiedArgs s
playerNoArgs w = withPlayerSource w <$> noArgs

-- | No Arguments, no source.
noArgs
  :: Timestamp
  -> UnverifiedArgs o
noArgs = Args Nothing []

withPlayerSource
  :: World s
  -> Args s v
  -> Args s v
withPlayerSource w = argsSource .~ (toAny <$> getPlayer w)

positionPlayer
  :: HasProperty s Enclosing
  => State (World s) (Maybe Bool)
positionPlayer = do
  fr <- gets _firstRoom
  pl <- gets _currentPlayer
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
  -> Maybe (Thing o)
getPlayer w = getThing' (_currentPlayer w) w

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