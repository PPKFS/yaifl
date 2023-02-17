{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}

module Yaifl.Core.Actions.Parser
  ( runActionHandlerAsWorldActions
  ) where

import Solitude

import Effectful.Dispatch.Dynamic ( interpret )
import Effectful.Optics ( use )

import Yaifl.Core.Actions.Action
import Yaifl.Core.Direction ( HasDirectionalTerms(..) )
import Yaifl.Core.Metadata ( Timestamp, Metadata, noteError, getGlobalTime )
import Yaifl.Core.Objects.Query
import Yaifl.Core.Rulebooks.Args ( playerNoArgs, UnverifiedArgs (..), Args (..), ArgSubject (..) )
import Yaifl.Core.Rulebooks.Rule ( ActionOptions(..), ActionHandler(..), parseAction, ActivityCollector, ResponseCollector, SayableValue )
import Yaifl.Core.Print ( Print, printLn )
import Yaifl.Core.WorldModel ( WMDirection, WMSayable )

import qualified Data.Map as Map
import qualified Data.Text as T
import Breadcrumbs
import Text.Interpolation.Nyan
import Data.Text.Display
import Yaifl.Core.AdaptiveNarrative (AdaptiveNarrative)

runActionHandlerAsWorldActions ::
  forall es wm a.
  State (WorldActions wm) :> es
  => Print :> es
  => ObjectLookup wm :> es
  => ObjectUpdate wm :> es
  => State Metadata :> es
  => State (ActivityCollector wm) :> es
  => ObjectTraverse wm :> es
  => State (ResponseCollector wm) :> es
  => State (AdaptiveNarrative wm) :> es
  => Display (WMSayable wm)
  => SayableValue (WMSayable wm) wm
  => (Enum (WMDirection wm), Bounded (WMDirection wm), HasDirectionalTerms wm)
  => Breadcrumbs :> es
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
        addAnnotation [int|t|Matched #{matched} and interpreting this as #{x}.|]
        runActionHandlerAsWorldActions $ parseAction (actionOpts { silently = True }) (x <> r)
      [(matched, r, Right x)] -> do
        addAnnotation [int|t|Action parse was successful; going with the verb #{actionName x} after matching #{matched}|]
        runActionHandlerAsWorldActions $ findSubjects (T.strip r) x

    whenLeft_ ac (\t' -> noteError (const ()) [int|t|Failed to parse the command #{t} because #{t'}.|])
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
  => ObjectUpdate wm :> es
  => State Metadata :> es
  => State (WorldActions wm) :> es
  => Print :> es
  => ObjectLookup wm :> es
  => State (ActivityCollector wm) :> es
  => State (AdaptiveNarrative wm) :> es
  => ObjectTraverse wm :> es
  => (Enum (WMDirection wm), Bounded (WMDirection wm), HasDirectionalTerms wm)
  => State (ResponseCollector wm) :> es
  => Display (WMSayable wm)
  => SayableValue (WMSayable wm) wm
  => Breadcrumbs :> es
  => Text
  -> Action wm
  -> Eff es (Either Text Bool)
findSubjects "" a = failHorriblyIfMissing $ do
  ua <- playerNoArgs
  Right <$> tryAction a ua
findSubjects cmd a = failHorriblyIfMissing $ do
  --TODO: handle other actors doing things
  --we attempt to either match some matching word from the action (e.g. go through <door>)
  --otherwise, we try to find a match of objects with increasingly large radii
  --todo: properly expand the search; consider the most likely items and then go off that
  --but for now, we'll just check everything.
  case parseDirection (Proxy @wm) cmd of
    Nothing -> return $ Left "No idea."
    Just x -> do
      ua <- playerNoArgs
      Right <$> tryAction a (\t -> let (UnverifiedArgs u) = ua t in UnverifiedArgs $ u { variables = [DirectionSubject x]})

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
  addAnnotation [int|t|Trying to do the action '#{actionName an}'|]
  let uva = f ta
  fromMaybe False <$> runAction uva an
