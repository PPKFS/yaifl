module Yaifl.Rulebooks
  ( whenPlayBeginsRules,
    addWhenPlayBegins,
    defaultActionProcessingRules,
    introText,
    runRulebook,
    runRulebookAndReturnVariables,

    makeRule,
    makeRule',
    rulePass
  )
where

import qualified Data.Text as T
import qualified Data.Text.Prettyprint.Doc.Render.Terminal as PPTTY
import Yaifl.Prelude
import Yaifl.Common
import Yaifl.Properties
import Yaifl.ObjectLookup
import Yaifl.Messages
import Data.Text.Lazy.Builder (fromText)

-- | Rule smart constructor for rules that do not have any arguments (else you'd have
-- to write rule bodies with a _ -> prefixed on).
makeRule'
  :: Text -- ^ Rule name.
  -> (forall m. NoMissingObjects s m => MonadWorld s m => m (Maybe r)) -- ^ Rule function.
  -> Rule s v r
makeRule' n f = makeRule n (const f)

makeRule
  :: Text -- ^ Rule name.
  -> (forall m. NoMissingObjects s m => MonadWorld s m => v -> m (Maybe r)) -- ^ Rule function.
  -> Rule s v r
makeRule n f = Rule n (\v -> do
  r <- f v
  return (v,r))

blankRule :: Text -> Rule s v r
blankRule = blank

defaultActionProcessingRules
  :: MonadWorld s m
  => Action s
  -> UnverifiedArgs s
  -> m (Maybe Bool)
defaultActionProcessingRules Action{..} u = withoutMissingObjects (runRulebook (Rulebook
  "Action Processing"
  (Just True)
  -- I have no idea how this works
  (ParseArguments (\uv -> do
    rs <- runParseArguments _actionParseArguments uv
    case rs of
      Nothing -> return Nothing
      Just x -> return $ Just $ fmap (const x) (unArgs uv)))
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
        ( \v -> do
          r <- runRulebookAndReturnVariables _actionCarryOutRules v
          return (fromMaybe (v, Nothing) r))
  , blankRule "after stage rule"
  , blankRule "investigate player awareness after rule"
  , blankRule "report stage rule"
  , blankRule "clean actions rule"
  ]) u) (handleMissingObject "" (Just False))

-- | Attempt to run an action from a text command (so will handle the parsing).
-- Note that this does require the arguments to be parsed out.
tryAction
  :: MonadWorld s m
  => Text -- ^ text of command
  -> (Timestamp -> UnverifiedArgs s) -- ^ Arguments without a timestamp
  -> m Bool
tryAction an f = do
  ta <- getGlobalTime
  debug (bformat ("Trying to do the action '" %! stext %! "'" ) an)
  let uva = f ta
  ac <- getAction an uva
  case ac of
    Nothing -> err (bformat ("Couldn't find a matching action for '" %! stext %! "'") an) >> return False
    Just a -> withContext (fromText $ _actionName a) $ fromMaybe False <$> runAction uva a

getAction
  :: MonadWorldRO s m
  => Text
  -> UnverifiedArgs s
  -> m (Maybe (Action s))
getAction n _ = gview $ actions % at n

-- | Run an action. This assumes that all parsing has been completed.
runAction
  :: MonadWorld s m
  => UnverifiedArgs s
  -> Action s
  -> m (Maybe Bool)
runAction args act = do
  w <- get
  let ap = _actionProcessing w
  ap act args

-- | Run a rulebook. Mostly this just adds some logging baggage.
runRulebook
  :: NoMissingObjects s m
  => MonadWorld s m
  => Rulebook s ia v re
  -> ia
  -> m (Maybe re)
runRulebook rb ia = runRulebookAndReturnVariables rb ia >>= (\mvre -> return $ mvre >>= snd)

runRulebookAndReturnVariables
  :: NoMissingObjects s m
  => MonadWorld s m
  => Rulebook s ia v re
  -> ia
  -> m (Maybe (v, Maybe re))
runRulebookAndReturnVariables Rulebook{..} args = do
  debug $ bformat ("Running the " %! stext %! " rulebook") _rbName
  withContext (bformat stext _rbName) $ do

    argParse <- runParseArguments _rbParseArguments args
    case argParse of
      Nothing -> err (bformat ("Failed to parse rulebook arguments for " %! stext %! " rulebook") _rbName) >> return Nothing
      Just a -> do
        debug $ bformat ("Successfully parsed rulebook arguments for " %! stext %! " rulebook") _rbName
        res <- (fmap Just . processRuleList _rbRules) a
        debug $ bformat ("Finished the " %! stext %! " rulebook") _rbName
        return $ (\(v, r1) -> Just (v, r1 <|> _rbDefaultOutcome)) =<< res

-- | Mostly this is a very complicated "run a list of functions until you get
-- something that isn't a Nothing, or a default if you get to the end".
processRuleList
  :: NoMissingObjects s m
  => MonadWorld s m
  => [Rule s v re]
  -> v
  -> m (v, Maybe re)
processRuleList [] v = return (v, Nothing)
processRuleList (x : xs) args = do
        unless (_ruleName x == "") (debug $ bformat ("Following the " %! stext %! " rule") (_ruleName x))
        (v, res) <- withContext (bformat (stext %! " rule") $ _ruleName x) (_runRule x args)
        -- if we hit nothing, continue; otherwise return
        case res of
          Nothing -> processRuleList xs v
          Just r -> debug (bformat ("Succeeded after following the " %! stext %! " rule") (_ruleName x)) >> return (v, Just r)

whenPlayBeginsName :: Text
whenPlayBeginsName = "When Play Begins"

-- | The rulebook that runs at the start of the game.
whenPlayBeginsRules
  :: HasProperty s Enclosing
  => Rulebook s () () Bool
whenPlayBeginsRules = Rulebook
    whenPlayBeginsName
    Nothing
    (ParseArguments (const $ return (Just ())) )
    [ makeRule' "Display banner" $ sayIntroText >> rulePass
    , makeRule' "Position player in world" positionPlayer
    , makeRule' "Initial room description" initRoomDescription
    ]

rulePass
  :: Monad m
  => m (Maybe a)
rulePass = return Nothing

initRoomDescription
  :: NoMissingObjects s m
  => MonadWorld s m
  => m (Maybe a)
initRoomDescription = do
  ua <- playerNoArgs
  tryAction "looking" ua >> rulePass

-- | No Arguments, player source.
playerNoArgs
  :: forall s m. NoMissingObjects s m
  => MonadWorld s m
  => m (Timestamp -> UnverifiedArgs s)
playerNoArgs = do
  ua <- withPlayerSource blank
  return (\ts -> ua & coercedTo @(Args s [AnyObject s]) % argsTimestamp .~ ts)

withPlayerSource
  :: forall s m. NoMissingObjects s m
  => MonadWorld s m
  => UnverifiedArgs s
  -> m (UnverifiedArgs s)
withPlayerSource u = do
  p <- getPlayer
  return $ u & coercedTo @(Args s [AnyObject s]) % argsSource ?~ toAny p

positionPlayer
  :: MonadWorld s m
  => HasProperty s Enclosing
  => m (Maybe Bool)
positionPlayer = do
  fr <- gets _firstRoom
  pl <- gets _currentPlayer
  case fr of
    Nothing -> failRuleWithError
      "No rooms have been made, so cannot place the player."
    Just fr' -> do
      m <- move pl fr'
      if m then return Nothing else failRuleWithError "Failed to move the player."

-- | Return a failure (Just False) from a rule and log a string to the
-- debug log.
failRuleWithError
  :: Logger m
  => Text -- ^ Error message.
  -> m (Maybe Bool)
failRuleWithError t = do
  err (bformat stext t)
  return $ Just False

getPlayer
  :: NoMissingObjects s m
  => MonadWorld s m
  => m (Thing s)
getPlayer = do
  cp <- gets _currentPlayer
  getThing cp

sayIntroText
  :: MonadWorld s m
  => m ()
sayIntroText = do
  setSayStyle (Just (PPTTY.color PPTTY.Green <> PPTTY.bold))
  t <- gets _title
  say $ introText t
  setSayStyle Nothing
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
  :: MonadWorld s m
  => Rule s () Bool
  -> m ()
addWhenPlayBegins r = whenPlayBegins %= addRule r

addRule
  :: Rule o v r
  -> Rulebook o ia v r
  -> Rulebook o ia v r
addRule r = rbRules %~ (++ [r])