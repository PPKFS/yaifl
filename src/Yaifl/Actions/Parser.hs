module Yaifl.Actions.Parser
  ( runActionHandlerAsWorldActions
  ) where

import Solitude

import Breadcrumbs
import Data.Text.Display
import Effectful.Dispatch.Dynamic
import Effectful.Optics ( use )
import Yaifl.Actions.Action
import Yaifl.Text.AdaptiveNarrative (AdaptiveNarrative)
import Yaifl.Model.Direction ( HasDirectionalTerms(..) )
import Yaifl.Metadata ( Metadata, noteError, getGlobalTime )
import Yaifl.Model.Objects.Query
import Yaifl.Text.Print ( Print, printLn )
import Yaifl.Rules.Args
import Yaifl.Model.WorldModel ( WMDirection, WMSayable )
import qualified Data.Map as Map
import qualified Data.Text as T
import Yaifl.Rules.RuleEffects
import Data.List.Split
import Data.List (lookup)
import Effectful.Error.Static
import Yaifl.Model.Objects.Effects
import Data.Char (isSpace)
import qualified Data.Set as S
import Yaifl.Model.Object
import Yaifl.Text.Say

-- | Run an action. This assumes that all parsing has been completed.
runAction ::
  forall wm es goesWith resps v.
  Refreshable wm v
  => State (WorldActions wm) :> es
  => RuleEffects wm es
  => UnverifiedArgs wm goesWith
  -> Action wm resps goesWith v
  -> Eff es (Maybe Bool)
runAction uArgs act = withSpan "run action" (act ^. #name) $ \aSpan -> do
  mbArgs <- (\v -> fmap (const v) (unArgs uArgs)) <$$> runParseArguments (act ^. #parseArguments) uArgs
  case mbArgs of
    Left err -> do
      addAnnotation err
      pure (Just False)
    Right args -> do
      -- running an action is simply evaluating the action processing rulebook.
      (ActionProcessing ap) <- use @(WorldActions wm) #actionProcessing
      ap aSpan act args

runActionHandlerAsWorldActions ::
  forall es wm a.
  State (WorldActions wm) :> es
  => (Ord (WMDirection wm), Enum (WMDirection wm), Bounded (WMDirection wm), HasDirectionalTerms wm)
  => Breadcrumbs :> es
  => Display (WMSayable wm)
  => ObjectLookup wm :> es
  => ObjectTraverse wm :> es
  => ObjectUpdate wm :> es
  => Print :> es
  => SayableValue (WMSayable wm) wm
  => State (ActivityCollector wm) :> es
  => State (AdaptiveNarrative wm) :> es
  => State (ResponseCollector wm) :> es
  => State Metadata :> es
  => Eff (ActionHandler wm : es) a
  -> Eff es a
runActionHandlerAsWorldActions = interpret $ \_ -> \case
  ParseAction actionOpts additionalArgs t -> withSpan' "action" t $ do
    -- print the prompt
    unless (silently actionOpts || hidePrompt actionOpts) $ printLn $ "\n>" <> t
    --we assume that the verb is the first thing in the command
    possVerbs <- findVerb t
    ac <- case possVerbs of
      [] -> return . Left $ "I have no idea what you meant by '" <> t <> "'."
      xs:x:_ -> return $ Left $ "Did you mean one of" <> prettyPrintList (map (show . view _1) [xs, x]) <> "?"
      [(matched, r, Interpret (InterpretAs x params))] -> do
        addAnnotation $ "Matched " <> matched <> " and interpreting this as " <> x
        runActionHandlerAsWorldActions $ parseAction (actionOpts { hidePrompt = True }) params (x <> r)
      -- we've successfully resolved it into an action
      [(matched, r, RegularAction x@(WrappedAction a))] -> do
        addAnnotation $ "Action parse was successful; going with the verb " <> view actionName a <> " after matching " <> matched
        runActionHandlerAsWorldActions $ findSubjects (T.strip r) additionalArgs x
      [(matched, _, OtherAction (OutOfWorldAction name runIt))] -> do
        addAnnotation $ "Action parse was successful; going with the out of world action " <> name <> " after matching " <> matched
        runActionHandlerAsWorldActions $ failHorriblyIfMissing runIt
        pure $ Right True

    whenLeft_ ac (\t' -> do
      noteError (const ()) $ "Failed to parse the command " <> t <> " because " <> t'
      runActionHandlerAsWorldActions $ failHorriblyIfMissing $ sayLn t')
    return ac

findVerb ::
  (State (WorldActions wm) :> es)
  => Text
  -> Eff es [(Text, Text, ActionPhrase wm)]
findVerb cmd = do
  let cmd' = T.toLower cmd
  ac <- use #actionsMap
  let possVerbs = mapMaybe (\case
        (_, RegularAction a@(WrappedAction (Action{understandAs}))) ->
          case mapMaybe (\ua -> (ua,) <$> ua `T.stripPrefix` cmd') understandAs
          of
            [] -> Nothing
            (ua, x):_ -> Just (ua, x, RegularAction a)
        (e, Interpret i@(InterpretAs _ _)) -> case e `T.stripPrefix` cmd' of
          Nothing -> Nothing
          Just r -> Just (e, r, Interpret i)
        (e, OtherAction o@(OutOfWorldAction _ _)) -> case e `T.stripPrefix` cmd' of
          Nothing -> Nothing
          Just r -> Just (e, r, OtherAction o)) (Map.toList ac)
      removePartialInMiddleOfWord (_matchPart, "", _) = True
      removePartialInMiddleOfWord (_matchPart, x, _) = case T.uncons x of
        Nothing -> True
        Just (a, _b) -> isSpace a
  return $ filter removePartialInMiddleOfWord possVerbs

findSubjects ::
  forall wm es.
  ActionHandler wm :> es
  => (Ord (WMDirection wm), Enum (WMDirection wm), Bounded (WMDirection wm), HasDirectionalTerms wm)
  => Breadcrumbs :> es
  => Display (WMSayable wm)
  => ObjectLookup wm :> es
  => ObjectTraverse wm :> es
  => ObjectUpdate wm :> es
  => Print :> es
  => SayableValue (WMSayable wm) wm
  => State (ActivityCollector wm) :> es
  => State (AdaptiveNarrative wm) :> es
  => State (ResponseCollector wm) :> es
  => State (WorldActions wm) :> es
  => State Metadata :> es
  => Text
  -> [NamedActionParameter wm]
  -> WrappedAction wm
  -> Eff es (Either Text Bool)
findSubjects cmd actionArgs (WrappedAction (a :: Action wm resps goesWith v)) = runErrorNoCallStack $ failHorriblyIfMissing $ do
  --TODO: handle other actors doing things#
  actor <- getPlayer
  ts <- getGlobalTime
  --we attempt to either match some matching word from the action (e.g. go through <door>)
  --otherwise, we try to find a match of objects with increasingly large radii
  --todo: properly expand the search; consider the most likely items and then go off that
  --but for now, we'll just check everything.
  --ua <- playerArgs actionArgs
  let isMatchWord = flip elem (map fst $ matches a)
      parts = (split . whenElt) isMatchWord (words cmd)
  (goesWithPart, parsedArgs) <- case parts of
    [] -> pure (NoParameter, [])
    cmdArgWords:matchedWords -> do
      cmdArgs' <- parseArgumentType @wm (goesWithA (Proxy @goesWith)) (unwords cmdArgWords)
      cmdArgs <- either throwError pure cmdArgs'
      -- then for each run of matching words we want to try and parse the rest of the list
      matchWords <- mapM (\case
        [] -> error "impossible"
        (matchWord:args) -> do
          let v = fromMaybe (error "impossible") $ lookup matchWord (matches a)
          arg <- parseArgumentType @wm v (unwords args)
          either throwError (pure . (matchWord,)) arg) matchedWords
      pure (cmdArgs, matchWords)
  let v = tryParseArguments (Proxy @goesWith) (S.fromList $ filter (/= NoParameter) $ goesWithPart:actionArgs)
  case v of
    Nothing -> throwError (("Argument mismatch because we got " <> show (S.fromList $ goesWithPart:actionArgs) <> " and we expected " <> show (goesWithA @goesWith Proxy)) :: Text)
    Just v' -> tryAction a (UnverifiedArgs $ Args { actionOptions = ActionOptions False False, timestamp = ts, source = actor, variables = (v', parsedArgs) })

parseArgumentType ::
  forall wm es.
  (Enum (WMDirection wm), Bounded (WMDirection wm), HasDirectionalTerms wm)
  => RuleEffects wm es
  => ActionParameterType
  -> Text
  -> Eff es (Either Text (NamedActionParameter wm))
parseArgumentType TakesDirectionParameter t = pure $ maybe
  (Left $ "expected a direction but instead found " <> t) (Right . DirectionParameter) $ parseDirection (Proxy @wm) t
parseArgumentType TakesNoParameter "" = pure $ Right NoParameter
parseArgumentType (TakesOneOf ap1 ap2) t = do
  mbRes <- parseArgumentType ap1 t
  case mbRes of
    Left err -> do
      addAnnotation err
      parseArgumentType ap2 t
    Right res -> pure $ Right res
parseArgumentType (Optionally a) t = do
  mbRes <- parseArgumentType a t
  case mbRes of
    Left _err -> pure $ Right NoParameter
    Right r -> pure $ Right r
parseArgumentType TakesObjectParameter t = tryFindingAnyObject t
parseArgumentType TakesThingParameter t = do
  o <- tryFindingObject t
  case fromAny <$> o of
    Left err -> pure $ Left err
    Right Nothing -> pure $ Left "got room not thing"
    Right (Just x) -> pure $ Right $ ThingParameter x
parseArgumentType a t = pure $ Left $ "not implemented yet" <> show a <> " " <> t

tryFindingAnyObject ::
  RuleEffects wm es
  => Text
  -> Eff es (Either Text (NamedActionParameter wm))
tryFindingAnyObject t = do
  o <- tryFindingObject t
  pure $ ObjectParameter <$> o

tryFindingObject ::
  RuleEffects wm es
  => Text
  -> Eff es (Either Text (AnyObject wm))
tryFindingObject t = failHorriblyIfMissing $ do
  pl <- getCurrentPlayer
  playerLoc <- getLocation pl
  allItems <- getAllObjectsInRoom IncludeScenery IncludeDoors playerLoc
  let scores = zip (map (scoreParserMatch t) allItems) allItems
  threshold <- use @Metadata #parserMatchThreshold
  match <- filterM (\(f, _) -> f >>= \x -> pure (x > threshold)) scores
  case match of
    [] -> pure $ Left $ "I can't see anything called \"" <> t <> "\"."
    [x] -> pure $ Right (toAny $ snd x)
    (_x:_xs) -> pure $ Left "I saw too many things that could be that."

scoreParserMatch ::
  RuleEffects wm es
  => Text -> Thing wm -> Eff es Double
scoreParserMatch phrase thing = do
  let phraseSet = S.fromList . words $ phrase
  -- a total match between the phrase and either the thing's name or any of the thing's understand as gives 1
  -- otherwise, we see how many of the words of the phrase are represented in the above
  -- then the match is how many words of the phrase were successfully matched
  matchingAgainst <- (:(toList $ thing ^. #understandAs)) . S.fromList . words <$> sayText (thing ^. #name)
  -- for each set, keep only the words that match
  let filterSets = S.unions $ map (S.intersection phraseSet) matchingAgainst
  pure (fromIntegral (S.size filterSets) / fromIntegral (S.size phraseSet))


parseDirection ::
  forall wm.
  (Enum (WMDirection wm), Bounded (WMDirection wm), HasDirectionalTerms wm)
  => Proxy wm
  -> Text
  -> Maybe (WMDirection wm)
parseDirection p cmd =
  let allDirs = universe
      cleanedCmd = T.toLower $ T.strip cmd
  in
    find (\x -> cleanedCmd `elem` toTextDir p x) allDirs

-- | Attempt to run an action from a text command (so will handle the parsing).
-- Note that this does require the arguments to be parsed out.
tryAction ::
  NoMissingObjects wm es
  => Refreshable wm v
  => ActionHandler wm :> es
  => ObjectTraverse wm :> es
  => SayableValue (WMSayable wm) wm
  => State (WorldActions wm) :> es
  => State (ActivityCollector wm) :> es
  => State (ResponseCollector wm) :> es
  => State (AdaptiveNarrative wm) :> es
  => Print :> es
  => Action wm resps goesWith v -- ^ text of command
  -> UnverifiedArgs wm goesWith -- ^ Arguments without a timestamp
  -> Eff es Bool
tryAction a f = do
  addAnnotation $ "Trying to do the action '"<> view actionName a <> "'"
  fromMaybe False <$> runAction f a
