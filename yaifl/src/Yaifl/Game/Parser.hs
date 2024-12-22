module Yaifl.Game.Parser
  ( runActionHandlerAsWorldActions
  , printPrompt
  ) where

import Yaifl.Prelude

import Breadcrumbs
import Effectful.Dispatch.Dynamic

import Yaifl.Model.Action
import Yaifl.Text.AdaptiveNarrative (AdaptiveNarrative)
import Yaifl.Model.Kinds.Direction ( HasDirectionalTerms(..) )
import Yaifl.Model.Metadata
import Yaifl.Model.Query
import Yaifl.Text.Print
import Yaifl.Model.Actions.Args
import Yaifl.Model.WorldModel ( WMDirection )
import qualified Data.Map as Map
import qualified Data.Text as T
import Yaifl.Model.Rules.RuleEffects
import Data.List.Split
import Data.List (lookup)
import Effectful.Error.Static
import Yaifl.Model.Effects
import Data.Char (isSpace)
import qualified Data.Set as S
import Yaifl.Model.Kinds.Object
import Yaifl.Text.Say
import Yaifl.Model.Kinds.Thing
import Yaifl.Text.ListWriter
import Yaifl.Model.Kinds.AnyObject
import Yaifl.Model.Input (waitForInput, Input)
import Yaifl.Model.Tag
import Yaifl.Game.Actions.Looking.Visibility
import Yaifl.Model.Kinds.Enclosing

-- | Run an action. This assumes that all parsing has been completed.
runAction ::
  forall wm es goesWith resps v.
  Refreshable wm v
  => State (WorldActions wm) :> es
  => RuleEffects wm es
  => ActionOptions wm
  -> UnverifiedArgs wm goesWith
  -> Action wm resps goesWith v
  -> Eff es (Maybe Bool)
runAction opts uArgs act = withSpan "run action" (act ^. #name) $ \aSpan -> do
  mbArgs <- (\v -> fmap (const v) (unArgs uArgs)) <$$> runParseArguments (act ^. #parseArguments) uArgs
  case mbArgs of
    FailedParse err -> do
      addAnnotation err
      pure (Just False)
    ConversionTo newCommand args -> rightToMaybe <$> parseAction opts args newCommand
    SuccessfulParse args -> do
      -- running an action is simply evaluating the action processing rulebook.
      (ActionProcessing ap) <- use @(WorldActions wm) #actionProcessing
      ap aSpan act args

filterFirstM :: (Monad m, Foldable t) => Text -> t (m (Either Text a)) -> m (Either Text a)
filterFirstM def = foldlM fn (Left def)
  where
    fn memo action = case memo of
      Right _ -> pure memo
      Left err -> do
        x <- action
        case x of
          Left err2 -> pure $ Left $ err2 <> (if err == "." then "" else ", and ") <> err
          Right r -> pure (Right r)

runActionHandlerAsWorldActions ::
  forall es wm a.
  State (WorldActions wm) :> es
  => IOE :> es
  => (Ord (WMDirection wm), Enum (WMDirection wm), Bounded (WMDirection wm), HasDirectionalTerms wm)
  => Breadcrumbs :> es
  => ObjectLookup wm :> es
  => ObjectTraverse wm :> es
  => ObjectUpdate wm :> es
  => Print :> es
  => HasLookingProperties wm
  => State (ActivityCollector wm) :> es
  => State (AdaptiveNarrative wm) :> es
  => State (ResponseCollector wm) :> es
  => State Metadata :> es
  => Input :> es
  => Eff (ActionHandler wm : es) a
  -> Eff es a
runActionHandlerAsWorldActions = interpret $ \_ -> \case
  ParseAction actionOpts additionalArgs t -> withSpan' "action" t $ failHorriblyIfMissing $ do
    addPostPromptSpacing
    --we assume that the verb is the first thing in the command
    possVerbs <- findVerb t
    actor <- getPlayer
    ts <- getGlobalTime
    let verbAc :: (Text, Text, ActionPhrase wm) -> Eff es (Either Text Bool)
        verbAc ac = failHorriblyIfMissing $ case ac of
          (matched, r, Interpret (InterpretAs x params)) -> do
            addAnnotation $ "Matched " <> matched <> " and interpreting this as " <> x
            runActionHandlerAsWorldActions $ parseAction (actionOpts { hidePrompt = True }) params (x <> r)
          (matched, r, RegularAction (WrappedAction (a :: Action wm resps goesWith v))) -> do
            addAnnotation $ "Action parse was successful; going with the verb " <> view actionName a <> " after matching " <> matched
            runActionHandlerAsWorldActions $ do
              -- attempt to work out our nouns
              -- TODO: handle more additional args
              nouns <- parseNouns (Proxy @goesWith) (matches a) (listToMaybe additionalArgs) (T.strip r)
              let actuallyRunIt parsedArgs match = failHorriblyIfMissing $ do
                    addAnnotation $ show match <> show additionalArgs
                    let v = tryParseArguments (Proxy @goesWith) (S.fromList $ filter (/= NoParameter) $ match:additionalArgs)
                    case v of
                      Nothing -> do
                        addAnnotation $ (("Argument mismatch because we got " <> show (S.fromList $ match:additionalArgs) <> " and we expected " <> show (goesWithA @goesWith Proxy)) :: Text)
                        return $ Left (("Argument mismatch because we got " <> show (S.fromList $ match:additionalArgs) <> " and we expected " <> show (goesWithA @goesWith Proxy)) :: Text)
                      Just v' -> Right <$> tryAction actionOpts a (UnverifiedArgs $ Args { actionOptions = ActionOptions False False, timestamp = ts, source = actor, variables = (v', parsedArgs) })
              case nouns of
                Left ex -> do
                  addAnnotation ex
                  pure (Left ex)
                Right (PluralParameter xs, parsedArgs) -> do
                  addAnnotation $ "Running a set of plural actions..." <> matched
                  rs <- sequence <$> forM xs (\x -> do
                    let acName = a ^. #name
                    n <- failHorriblyIfMissing $ sayParameterName x
                    failHorriblyIfMissing $ [saying|({acName} {n}) |]
                    runOnParagraph
                    actuallyRunIt parsedArgs x)
                  pure $ second and rs
                Right (match, parsedArgs) -> do
                  addAnnotation $ "matched " <> show match <> " and parsed " <> show parsedArgs
                  actuallyRunIt parsedArgs match
          (matched, _, OtherAction (OutOfWorldAction name runIt)) ->  do
            addAnnotation $ "Action parse was successful; going with the out of world action " <> name <> " after matching " <> matched
            runActionHandlerAsWorldActions $ failHorriblyIfMissing $ runIt
            pure $ Right True
    ac <- case possVerbs of
      [] -> return $ Left ("I have no idea what you meant by '" <> t <> "'.")
      _ -> do
        filterFirstM "." $ map (inject . verbAc) possVerbs
    whenLeft_ ac (\t' -> do
      noteError (const ()) $ "Failed to parse the command " <> t <> " because " <> t'
      runActionHandlerAsWorldActions $ failHorriblyIfMissing $ say t')
    return ac

parseNouns ::
  forall wm es goesWith.
  ActionHandler wm :> es
  => (Enum (WMDirection wm), Bounded (WMDirection wm), HasDirectionalTerms wm)
  => GoesWith goesWith
  => Breadcrumbs :> es
  => ObjectLookup wm :> es
  => ObjectTraverse wm :> es
  => ObjectUpdate wm :> es
  => Print :> es
  => Input :> es
  => HasLookingProperties wm
  => State (ActivityCollector wm) :> es
  => State (AdaptiveNarrative wm) :> es
  => State (ResponseCollector wm) :> es
  => State Metadata :> es
  => Proxy (goesWith :: ActionParameterType)
  -> [(Text, ActionParameterType)]
  -> Maybe (NamedActionParameter wm)
  -> Text
  -> Eff es (Either Text (NamedActionParameter wm, [(Text, NamedActionParameter wm)]))
parseNouns _ wordsToMatch mbParameter command = runErrorNoCallStack $ failHorriblyIfMissing $ do
  let isMatchWord = flip elem (map fst wordsToMatch)
      parts = (split . whenElt) isMatchWord (words command)
  case parts of
    -- no matches at all
    [] -> pure (fromMaybe NoParameter mbParameter, [])
    [[]] -> pure (fromMaybe NoParameter mbParameter, [])
    -- one "vanilla" word (e.g. pet cat) and optionally some match words (e.g. pet cat with brush)
    cmdArgWords:matchedWords -> do
      addAnnotation $ "split is " <> show cmdArgWords <> " and " <> show matchedWords <> " from " <> show parts
      cmdArgs <- parseArgumentType @wm (goesWithA (Proxy @goesWith)) (unwords cmdArgWords)
      -- then for each run of matching words we want to try and parse the rest of the list
      matchWords <- mapM (\case
        [] -> error "impossible"
        (matchWord:args) -> do
          let v = fromMaybe (error "impossible") $ lookup matchWord wordsToMatch
          arg' <- parseArgumentType @wm v (unwords args)
          either throwError (pure . (matchWord,)) arg') matchedWords
      either throwError pure cmdArgs >>= \c -> pure (c, matchWords)

addPostPromptSpacing ::
  Print :> es
  => Eff es ()
addPostPromptSpacing = void $ modifyBuffer (\b -> b & #lastMessageContext % #shouldPrintPbreak .~ True)

printPrompt ::
  Print :> es
  => ActionOptions wm
  -> Eff es ()
printPrompt actionOpts = do
  leftoverLookingSpace <- lastPrint . lastMessageContext <$> modifyBuffer id
  modifyBuffer (\b -> case ("\n\n" `T.isSuffixOf` leftoverLookingSpace, "\n" `T.isSuffixOf` leftoverLookingSpace) of
    (True, _) -> b
    (False, True) -> b & #lastMessageContext % #shouldPrintLinebreak .~ True
    (False, False) -> b & #lastMessageContext % #shouldPrintPbreak .~ True
    )
  modifyBuffer (\b -> b & #lastMessageContext % #runningOnLookingParagraph .~ False)
  unless (silently actionOpts || hidePrompt actionOpts) $ printText ">"

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

parseArgumentType ::
  forall wm es.
  (Enum (WMDirection wm), Bounded (WMDirection wm), HasDirectionalTerms wm)
  => HasLookingProperties wm
  => RuleEffects wm es
  => ActionParameterType
  -> Text
  -> Eff es (Either Text (NamedActionParameter wm))
parseArgumentType TakesDirectionParameter t = pure $ maybe
  (Left $ "expected a direction but instead found " <> t) (Right . DirectionParameter) $ parseDirection (Proxy @wm) t
parseArgumentType TakesNoParameter str = if T.null str then pure $ Right NoParameter else pure $ Left $ "expected nothing but got " <> str
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
  case bimapF (mapM fromAny) fromAny o of
    Left err -> pure $ Left err
    Right (Right (Just x)) -> pure $ Right $ ThingParameter x
    Right (Left (Just ts)) -> pure $ Right $ PluralParameter (map ThingParameter ts)
    _ -> pure $ Left "got room not thing"
parseArgumentType a t = pure $ Left $ "not implemented yet" <> show a <> " " <> t

tryFindingAnyObject ::
  forall wm es.
  HasLookingProperties wm
  => RuleEffects wm es
  => Text
  -> Eff es (Either Text (NamedActionParameter wm))
tryFindingAnyObject t = do
  o <- tryFindingObject t
  case o of
    Right (Left plurals) -> pure $ Right $ PluralParameter (map ObjectParameter plurals)
    Right (Right o') -> pure $ Right $ ObjectParameter o'
    Left err -> pure $ Left err

tryFindingObject ::
  forall wm es.
  RuleEffects wm es
  => HasLookingProperties wm
  => Text
  -> Eff es (Either Text (Either [AnyObject wm] (AnyObject wm)))
tryFindingObject t = failHorriblyIfMissing $ do
  pl <- getCurrentPlayer
  let getUppermostDomain :: (AnyObject wm, Enclosing) -> Eff (Error MissingObject : es) (AnyObject wm, Enclosing)
      getUppermostDomain (obj, enc) = do
        asThingOrRoom
          (\t' -> do
            vs <- getVisibleLevels t'
            case vs of
              [] -> pure (obj, enc)
              x:xs -> let c = last (x :| xs) in return $ maybe (obj, enc) (c,) $ getEnclosingMaybe c
            )
          (const $ return $ (obj, enc))
          obj

  (playerLocObj, domainEnc) <- getUppermostDomain =<< getEnclosingObject (thingContainedBy pl)
  -- okay, if we bother doing a proper scanning loop it'll go here
  -- but for now, we just want to consider anything recursively present. that'll do.
  allItems <- getAllObjectsInEnclosing IncludeScenery IncludeDoors (tag domainEnc playerLocObj)
  findObjectsFrom t allItems True


findObjectsFrom ::
  forall wm es.
  WithListWriting wm
  => RuleEffects wm es
  => Text
  -> [Thing wm]
  -> Bool
  -> Eff es (Either Text (Either [AnyObject wm] (AnyObject wm)))
findObjectsFrom t allItems considerAmbiguity = do
  let phraseSet = S.fromList . words $ t
  -- the scores here are likelihood that it's matching as a singular or part of a plural
  scores <- zip allItems <$> mapM (scoreParserMatch phraseSet) allItems
  threshold <- use @Metadata #parserMatchThreshold
  let singularMatches = filter ((> threshold) . fst . snd) scores
  let pluralMatches = filter ((> threshold) . snd . snd) scores
  case (singularMatches, pluralMatches) of
    ([], []) -> pure $ Left $ "I can't see anything called \"" <> t <> "\""
    -- just one singular thing
    ([x], []) -> pure . Right . Right $ toAny (fst x)
    ([], xs) -> pure . Right . Left $ map (toAny @wm . fst) xs
    (xs, []) -> if considerAmbiguity
      then handleAmbiguity (map fst xs)
      else pure $ Left "I still didn't know what you meant"
    (_x, _xs) -> pure $ Left "I saw both a single thing and plural things that could be that"

handleAmbiguity ::
  WithListWriting wm
  => RuleEffects wm es
  => [Thing wm]
  -> Eff es (Either Text (Either [AnyObject wm] (AnyObject wm)))
handleAmbiguity ls = do
  names <- mapM (sayText . view #name) ls
  let phrase = case names of
        [] -> error "no objects to be ambiguous between"
        [x] -> x
        [x, y] -> x <> " or " <> y
        l -> maybe (error "impossible") (\(i, ls') -> T.intercalate ", " i <> ", or " <> ls') (unsnoc l)
  say $ "Which did you mean: " <> phrase <> "?"
  -- TODO: this should be an activity
  printPrompt (ActionOptions False False)
  i <- waitForInput
  printText i
  addPostPromptSpacing
  findObjectsFrom i ls False

scoreParserMatch ::
  RuleEffects wm es
  => S.Set Text
  -> Thing wm
  -> Eff es (Double, Double)
scoreParserMatch phraseSet thing = do
  -- a total match between the phrase and either the thing's name or any of the thing's understand as gives 1
  -- otherwise, we see how many of the words of the phrase are represented in the above
  -- then the match is how many words of the phrase were successfully matched
  matchingAgainst <- (:(toList $ thing ^. #understandAs)) . S.fromList . words <$> sayText (thing ^. #name)
  -- and also get the matches of its *kind*
  kindSynonyms <- map (S.fromList . words) . mconcat . S.toList <$> mapKindsOf thing (view #understandAs)
  kindPluralSynonyms <- map (S.fromList . words) . mconcat . S.toList <$> mapKindsOf thing (view #pluralUnderstandAs)
  -- for each set, keep only the words that match
  let filterSets = S.unions $ map (S.intersection phraseSet . S.map T.toLower) (matchingAgainst <> kindSynonyms)
      filterPluralSets = S.unions $ map (S.intersection phraseSet . S.map T.toLower) kindPluralSynonyms
  pure
    ( fromIntegral (S.size filterSets) / fromIntegral (S.size phraseSet)
    , fromIntegral (S.size filterPluralSets) / fromIntegral (S.size phraseSet)
    )

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
  => WithListWriting wm
  => Input :> es
  => Refreshable wm v
  => ActionHandler wm :> es
  => ObjectTraverse wm :> es
  => State (WorldActions wm) :> es
  => State (ActivityCollector wm) :> es
  => State (ResponseCollector wm) :> es
  => State (AdaptiveNarrative wm) :> es
  => Print :> es
  => ActionOptions wm
  -> Action wm resps goesWith v -- ^ text of command
  -> UnverifiedArgs wm goesWith -- ^ Arguments without a timestamp
  -> Eff es Bool
tryAction opts a f = do
  addAnnotation $ "Trying to do the action '"<> view actionName a <> "'"
  fromMaybe False <$> runAction opts f a
