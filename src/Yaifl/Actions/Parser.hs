{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}

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
import Yaifl.Metadata ( Timestamp, Metadata, noteError, getGlobalTime )
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

-- | Run an action. This assumes that all parsing has been completed.
runAction ::
  forall wm es.
  State (WorldActions wm) :> es
  => RuleEffects wm es
  => UnverifiedArgs wm
  -> WrappedAction wm
  -> Eff es (Maybe Bool)
runAction uArgs (WrappedAction act) = withSpan "run action" (act ^. #name) $ \aSpan -> do
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
  => (Enum (WMDirection wm), Bounded (WMDirection wm), HasDirectionalTerms wm)
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
  ParseAction actionOpts actionArgs t -> withSpan' "action" t $ do
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
        runActionHandlerAsWorldActions $ findSubjects (T.strip r) actionArgs x
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
  => (Enum (WMDirection wm), Bounded (WMDirection wm), HasDirectionalTerms wm)
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
  -> ActionParameter wm
  -> WrappedAction wm
  -> Eff es (Either Text Bool)
findSubjects "" actionArgs w = failHorriblyIfMissing $ do
  ua <- playerArgs actionArgs
  Right <$> tryAction w ua
findSubjects cmd actionArgs w@(WrappedAction a) = runErrorNoCallStack $ failHorriblyIfMissing $ do
  --TODO: handle other actors doing things
  --we attempt to either match some matching word from the action (e.g. go through <door>)
  --otherwise, we try to find a match of objects with increasingly large radii
  --todo: properly expand the search; consider the most likely items and then go off that
  --but for now, we'll just check everything.
  ua <- playerArgs actionArgs

  --we then go through, taking words until we hit either the end or a match word
  --then we try to work out what it was
  let isMatchWord = flip elem (map fst $ matches a)
      parts = (split . whenElt) isMatchWord (words cmd)
  (goesWithPart, parsedArgs) <- case parts of
    cmdArgWords:matchedWords -> do
      cmdArgs' <- parseArgumentType @wm (goesWith a) (unwords cmdArgWords)
      cmdArgs <- either throwError pure cmdArgs'
      -- then for each run of matching words we want to try and parse the rest of the list
      matchWords <- mapM (\case
        [] -> error "impossible"
        (matchWord:args) -> do
          let v = fromMaybe (error "impossible") $ lookup matchWord (matches a)
          arg <- parseArgumentType @wm v (unwords args)
          either throwError (pure . (matchWord,)) arg) matchedWords
      pure (cmdArgs, matchWords)
    xs -> error $ "impossible: should have at least one element of the command to parse " <> show xs
  tryAction w (\t -> let (UnverifiedArgs u) = ua t in UnverifiedArgs $ u { variables = (goesWithPart, parsedArgs) })

parseArgumentType ::
  forall wm es.
  (Enum (WMDirection wm), Bounded (WMDirection wm), HasDirectionalTerms wm)
  => Breadcrumbs :> es
  => ActionParameterType
  -> Text
  -> Eff es (Either Text (ActionParameter wm))
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
parseArgumentType a t = pure $ Left $ "not implemented yet" <> show a <> " " <> t

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
  => ActionHandler wm :> es
  => ObjectTraverse wm :> es
  => State (WorldActions wm) :> es
  => State (ActivityCollector wm) :> es
  => State (ResponseCollector wm) :> es
  => State (AdaptiveNarrative wm) :> es
  => SayableValue (WMSayable wm) wm
  => Print :> es
  => WrappedAction wm -- ^ text of command
  -> (Timestamp -> UnverifiedArgs wm) -- ^ Arguments without a timestamp
  -> Eff es Bool
tryAction an@(WrappedAction a) f = do
  ta <- getGlobalTime
  addAnnotation $ "Trying to do the action '"<> view actionName a <> "'"
  fromMaybe False <$> runAction (f ta) an
