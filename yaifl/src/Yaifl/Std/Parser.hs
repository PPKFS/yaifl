module Yaifl.Std.Parser
  ( printPrompt
  , parseAction
  , runActionHandlerAsWorldActions
  ) where

import Yaifl.Prelude

import Breadcrumbs
import Effectful.Dispatch.Dynamic
import Effectful.Error.Static

import Data.Char (isSpace)
import Data.List (lookup )
import Data.List.Split
import Yaifl.Core.Effects
import Yaifl.Core.Kinds.AnyObject
import Yaifl.Core.Kinds.Enclosing
import Yaifl.Core.Kinds.Object
import Yaifl.Core.Kinds.Thing
import Yaifl.Core.Metadata
import Yaifl.Core.Query.Enclosing
import Yaifl.Core.Query.Object
import Yaifl.Core.Refreshable
import Yaifl.Core.Rules.RuleEffects
import Yaifl.Core.Tag
import Yaifl.Std.Actions.Imports
import Yaifl.Std.Actions.Looking.Visibility
import Yaifl.Std.Kinds.Direction ( HasDirectionalTerms(..) )
import Yaifl.Std.Kinds.Person
import Yaifl.Std.Rulebooks.ActionProcessing
import Yaifl.Text.AdaptiveNarrative (AdaptiveNarrative)
import Yaifl.Text.ListWriter
import Yaifl.Text.Print
import Yaifl.Std.Create.Rule
import qualified Data.Map as Map
import qualified Data.Set as S
import qualified Data.Text as T

type ActionHandlerConstraints es wm =
  (Ord (WMDirection wm)
  , Enum (WMDirection wm)
  , Bounded (WMDirection wm)
  , HasDirectionalTerms wm
  , Breadcrumbs :> es
  , HasLookingProperties wm
  , IOE :> es
  , Input :> es
  , ObjectLookup wm :> es
  , ObjectTraverse wm :> es
  , ObjectUpdate wm :> es
  , Print :> es
  , State (ActivityCollector wm) :> es
  , State (AdaptiveNarrative wm) :> es
  , State (ResponseCollector wm) :> es
  , State Metadata :> es
  , State (WorldActions wm) :> es
  )

-- | The main action handling routine.
runActionHandlerAsWorldActions ::
  forall es wm a.
  ActionHandlerConstraints es wm
  => Eff (ActionHandler wm : es) a
  -> Eff es a
runActionHandlerAsWorldActions = interpret $ \_ -> \case
  ParseAction actionOpts additionalArgs t -> withSpan' "action" t $ failHorriblyIfMissing $ do
    addPostPromptSpacing
    --we assume that the verb is the first thing in the command;
    --TODO: ask other people for things
    possVerbs <- findVerb t

    ac <- case possVerbs of
      [] -> return $ Left ("I have no idea what you meant by '" <> t <> "'.")
      _ -> do
        filterFirstStringM "." $ map (inject . handleVerbAction actionOpts additionalArgs) possVerbs
    whenLeft_ ac (\t' -> do
      noteError (const ()) $ "Failed to parse the command " <> t <> " because " <> t'
      runActionHandlerAsWorldActions $ failHorriblyIfMissing $ say t')
    return ac

handleVerbAction ::
  forall es wm.
  ActionHandlerConstraints es wm
  => ActionOptions wm
  -> [NamedActionParameter wm]
  -> (Text, Text, ActionPhrase wm)
  -> Eff es (Either Text Bool)
handleVerbAction actionOpts additionalArgs = \case
  -- this phrase should be interpreted as a different action
  (matched, r, Interpret (InterpretAs x params)) -> do
    addAnnotation $ "Matched " <> matched <> " and interpreting this as " <> x
    runActionHandlerAsWorldActions $ parseAction (actionOpts { hidePrompt = True }) params (x <> r)
  -- this is a meta action
  (matched, _, OtherAction (OutOfWorldAction name runIt)) ->  do
    addAnnotation $ "Action parse was successful; going with the out of world action " <> name <> " after matching " <> matched
    runActionHandlerAsWorldActions $ failHorriblyIfMissing runIt
    pure $ Right True
  -- the happy normal path. We have found a matching action and now can do the typed argument parsing.
  (matched, r, RegularAction (WrappedAction (a :: Action wm resps goesWith v))) -> do
    addAnnotation $ "Action parse was successful; going with the verb " <> view actionName a <> " after matching " <> matched
    runActionHandlerAsWorldActions $ do
      -- attempt to work out our nouns
      -- TODO: handle more additional args
      -- TODO: we should probably parse them once instead of per-action
      nouns <- parseNouns (Proxy @goesWith) (matches a) (listToMaybe additionalArgs) (T.strip r)

      let actuallyRunIt parsedArgs match = failHorriblyIfMissing $ do
            -- this is where the actual parsing happens
            case tryParseArguments (Proxy @goesWith) (S.fromList $ filter (/= NoParameter) $ match:additionalArgs) of
              Nothing -> do
                let errMsg = "Argument mismatch because we got " <> show (S.fromList $ match:additionalArgs) <> " and we expected " <> show (goesWithA @goesWith Proxy)
                addAnnotation errMsg
                return $ Left errMsg
              Just v' -> do
                ts <- getGlobalTime
                actor <- getPlayer
                Right <$> runAction actionOpts a (UnverifiedArgs $ Args { actionOptions = actionOpts, timestamp = ts, source = getTaggedObject actor, variables = (v', parsedArgs) })
      case nouns of
        Left ex -> do
          addAnnotation $ "noun parsing failed because " <> ex
          pure (Left ex)
        Right (PluralParameter xs, parsedArgs) -> do
          addAnnotation $ "Running a set of plural actions..." <> matched
          rs <- sequence <$> forM xs (\x -> do
            let acName = a ^. #name
            n <- failHorriblyIfMissing $ sayParameterName x
            failHorriblyIfMissing [saying|({acName} {n}) |]
            runOnParagraph
            actuallyRunIt parsedArgs x)
          pure $ second and rs
        Right (match, parsedArgs) -> do
          addAnnotation $ "matched " <> show match <> " and parsed " <> show parsedArgs
          actuallyRunIt parsedArgs match

filterFirstStringM :: (Monad m, Foldable t, Semigroup b, Eq b, IsString b) => b -> t (m (Either b a)) -> m (Either b a)
filterFirstStringM def = foldlM fn (Left def)
  where
    fn memo action = case memo of
      Right _ -> pure memo
      Left err -> do
        x <- action
        case x of
          Left err2 -> pure $ Left $ err2 <> (if err == "." then "" else ", and ") <> err
          Right r -> pure (Right r)

-- | given an expected noun type (which is done at the type level with goeswith) and a list of
-- individual matching words (TODO: also jam these onto the type level), attempt to break the string
-- into nicer parts where we know exactly what each part should be parsed as.
parseNouns ::
  forall wm es goesWith.
  ActionHandler wm :> es
  => GoesWith goesWith
  => ActionHandlerConstraints es wm
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
  (Print :> es, State Metadata :> es)
  => Eff es ()
addPostPromptSpacing = do
  usePostPrompt <- use @Metadata #usePostPromptPbreak
  void $ modifyBuffer (\b -> b & #lastMessageContext % (if usePostPrompt then #shouldPrintPbreak else #shouldPrintLinebreak) .~ True)

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

-- | Attempt to identify a verb/action from a command string.
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
          (const $ return (obj, enc))
          obj

  (playerLocObj, domainEnc) <- (\t' -> getUppermostDomain (getTaggedObject t', getEnclosing t')) =<< getEnclosingObject (thingContainedBy pl)
  -- okay, if we bother doing a proper scanning loop it'll go here
  -- but for now, we just want to consider anything recursively present. that'll do.
  allItems <- getAllObjectsInEnclosing IncludeScenery IncludeDoors Recurse (tagEntity domainEnc playerLocObj)
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
  -- first we drop articles because we don't want to be overly lenient
  -- then for how inform likes to treat understanding as, we also need every substring of the above
  let phraseTails = filter (\x -> (not . null $ x) && x /= ["the"] ) . concat . map inits . tails . words $ t
      phraseSet = S.fromList . map (T.intercalate " ") $ phraseTails
  -- the scores here are likelihood that it's matching as a singular or part of a plural
  scores <- zip allItems <$> mapM (scoreParserMatch phraseSet) allItems
  threshold <- use @Metadata #parserMatchThreshold
  let singularMatches = sortOn ((1-) . fst . snd) $ filter ((> threshold) . fst . snd) scores
  let pluralMatches = sortOn ((1-) . snd . snd) $ filter ((> threshold) . snd . snd) scores
  case (singularMatches, pluralMatches) of
    ([], []) -> pure $ Left $ "I can't see anything called \"" <> t <> "\""
    -- just one singular thing
    ([x], []) -> pure . Right . Right $ toAny (fst x)
    ([], xs) -> pure . Right . Left $ map (toAny @wm . fst) xs
    (x@(x1:x2:_), [])
      | (x1 ^. _2 % _1) == (x2 ^. _2 % _1) ->
          if considerAmbiguity
          then handleAmbiguity (map fst x)
          else pure $ Left "I still didn't know what you meant"
    (x1:_xs, []) -> pure . Right . Right $ toAny (fst x1)
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
  i <- waitForInput
  case i of
    Nothing -> return (Left "Alright then.")
    Just x -> do
      printPrompt (ActionOptions False False)
      withStyle (Just bold) $ printText x
      addPostPromptSpacing
      findObjectsFrom x ls False

scoreParserMatch ::
  RuleEffects wm es
  => SayableValue (WMText wm) wm
  => S.Set Text
  -> Thing wm
  -> Eff es (Double, Double)
scoreParserMatch phraseSet thing = do
  -- we break the name of the thing into words, and then amend this to the set of understanding.
  matchingAgainst <- (<>(toList $ thing ^. #understandAs)) . words <$> sayText (thing ^. #name)
  -- and also get the matches of its *kind*
  kindSynonyms <- mconcat . toList <$>  mapKindsOf thing (view #understandAs)
  kindPluralSynonyms <- mconcat . toList <$> mapKindsOf thing (view #pluralUnderstandAs)
  -- for each set, keep only the words that match
  let filterSets = (S.intersection phraseSet . S.map T.toLower) (S.fromList $ matchingAgainst <> kindSynonyms)
      filterPluralSets = (S.intersection phraseSet . S.map T.toLower) (S.fromList kindPluralSynonyms)
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
