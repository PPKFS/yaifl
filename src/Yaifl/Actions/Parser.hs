{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}

module Yaifl.Actions.Parser
  ( runActionHandlerAsWorldActions
  ) where

import Solitude

import Breadcrumbs
import Data.Text.Display
import Effectful.Dispatch.Dynamic ( interpret )
import Effectful.Optics ( use )
import Yaifl.Actions.Action
import Yaifl.Text.AdaptiveNarrative (AdaptiveNarrative)
import Yaifl.Model.Direction ( HasDirectionalTerms(..) )
import Yaifl.Metadata ( Timestamp, Metadata, noteError, getGlobalTime )
import Yaifl.Model.Objects.Query
import Yaifl.Text.Print ( Print, printLn )
import Yaifl.Rules.Args ( playerNoArgs, UnverifiedArgs (..), Args (..), ActionParameter (..) )
import Yaifl.Model.WorldModel ( WMDirection, WMSayable )
import qualified Data.Map as Map
import qualified Data.Text as T
import Yaifl.Rules.RuleEffects
import Data.List.Split
import Data.List (lookup)
import Effectful.Error.Static

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
  ParseAction actionOpts t -> do
    -- print the prompt
    unless (silently actionOpts) $ printLn $ "> " <> t
    --we assume that the verb is the first thing in the command
    possVerbs <- findVerb t
    ac <- case possVerbs of
      [] -> return . Left $ "I have no idea what you meant by '" <> t <> "'."
      xs:x:_ -> return $ Left $ "Did you mean " <> prettyPrintList (map (show . view _1) [xs, x]) <> "?"
      [(matched, r, Left (InterpretAs x))] -> do
        addAnnotation $ "Matched " <> matched <> " and interpreting this as " <> x
        runActionHandlerAsWorldActions $ parseAction (actionOpts { silently = True }) (x <> r)
      -- we've successfully resolved it into an action
      [(matched, r, Right x)] -> do
        addAnnotation $ "Action parse was successful; going with the verb " <> view actionName x <> " after matching " <> matched
        runActionHandlerAsWorldActions $ findSubjects (T.strip r) x

    whenLeft_ ac (\t' -> noteError (const ()) $ "Failed to parse the command " <> t <> " because " <> t')
    return ac

findVerb ::
  (State (WorldActions wm) :> es)
  => Text
  -> Eff es [(Text, Text, Either InterpretAs (Action wm))]
findVerb cmd = do
  let cmd' = T.toLower cmd
  ac <- use #actions
  --sayLn $ show $ (map (view (_2 % actionUnderstandAs)) (Map.toList ac))
  let possVerbs = mapMaybe (\case
        (_, Right a@Action{understandAs}) ->
          case mapMaybe (\ua -> (ua,) <$> ua `T.stripPrefix` cmd') understandAs
          of
            [] -> Nothing
            (ua, x):_ -> Just (ua, x, Right a)
        (e, Left i@(InterpretAs _)) -> case e `T.stripPrefix` cmd' of
          Nothing -> Nothing
          Just r -> Just (e, r, Left i)) (Map.toList ac)
  return possVerbs

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
  -> Action wm
  -> Eff es (Either Text Bool)
findSubjects "" a = failHorriblyIfMissing $ do
  ua <- playerNoArgs
  Right <$> tryAction a ua
findSubjects cmd a = runErrorNoCallStack $ failHorriblyIfMissing $ do
  --TODO: handle other actors doing things
  --we attempt to either match some matching word from the action (e.g. go through <door>)
  --otherwise, we try to find a match of objects with increasingly large radii
  --todo: properly expand the search; consider the most likely items and then go off that
  --but for now, we'll just check everything.
  ua <- playerNoArgs

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
  tryAction a (\t -> let (UnverifiedArgs u) = ua t in UnverifiedArgs $ u { variables = (goesWithPart, parsedArgs) })

parseArgumentType ::
  forall wm es.
  (Enum (WMDirection wm), Bounded (WMDirection wm), HasDirectionalTerms wm)
  => ActionParameterType
  -> Text
  -> Eff es (Either Text (ActionParameter wm))
parseArgumentType TakesDirectionParameter t = pure $ maybe
  (Left $ "expected a direction but instead found " <> t) (Right . DirectionParameter) $ parseDirection (Proxy @wm) t
parseArgumentType TakesNoParameter "" = pure $ Right NoParameter
parseArgumentType _ _ = error "not implemented yet"

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
  => Action wm -- ^ text of command
  -> (Timestamp -> UnverifiedArgs wm) -- ^ Arguments without a timestamp
  -> Eff es Bool
tryAction an f = do
  ta <- getGlobalTime
  addAnnotation $ "Trying to do the action '"<> view actionName an <> "'"
  fromMaybe False <$> runAction (f ta) an
