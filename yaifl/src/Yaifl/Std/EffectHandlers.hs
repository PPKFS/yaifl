module Yaifl.Std.EffectHandlers where

import Yaifl.Prelude hiding ( Reader, runReader )

import Breadcrumbs
import Effectful.Dispatch.Dynamic
import Yaifl.Std.Parser
import Yaifl.Core.Metadata
import Yaifl.Std.Kinds.Direction
import Yaifl.Core.Entity
import Yaifl.Core.Effects
import Yaifl.Core.WorldModel
import Yaifl.Text.AdaptiveNarrative
import Yaifl.Text.Print
import Yaifl.Std.World
import Yaifl.Std.Actions.Collection
import Effectful.Error.Static (Error, runError)
import Yaifl.Core.Store
import Yaifl.Std.Kinds.Region

import Yaifl.Std.Actions.Looking.Visibility
import Yaifl.Std.Rulebooks.ActionProcessing
import Yaifl.Core.Rules.RuleEffects
import Effectful.Provider.List (type (++))


type EffStack (wm :: WorldModel) = '[
  ActionHandler wm
  , State (AdaptiveNarrative wm)
  , State (ResponseCollector wm)
  , State (ActivityCollector wm)
  , Input
  , State (ActionCollection wm)
  , ObjectTraverse wm
  , ObjectUpdate wm
  , ObjectLookup wm
  , State Metadata
  , State (WorldActions wm)
  , Print
  , State (World wm)

  , Breadcrumbs
  , Error MissingObject
  ]

type Game wm = Eff (EffStack wm ++ '[IOE])

type UnderlyingEffStack wm = '[State (World wm), IOE]

zoomState ::
  (State whole :> es)
  => Lens' whole sub
  -> (Eff (State sub ': es)) a
  -> Eff es a
zoomState l = interpret $ \env -> \case
  Get      -> gets (view l)
  Put s    -> modify (set l s)
  State f  -> state (\s -> second (\x -> s & l .~ x) $ f (s ^. l))
  StateM f -> localSeqUnlift env $ \unlift -> stateM
    (\s -> do
      newSub <- unlift $ f (s ^. l)
      pure $ second (\x -> s & l .~ x) newSub )

convertToIO ::
  forall wm a.
  (Ord (WMDirection wm), Enum (WMDirection wm), Bounded (WMDirection wm), HasDirectionalTerms wm)
  -- => WithResponseSet wm An_Iso "listWriterResponses" (ListWriterResponses -> Response wm ())
  => HasLookingProperties wm
  => (forall es b. IOE :> es => State (World wm) :> es => Eff (Print : es) b -> Eff es b)
  -> (forall es b. IOE :> es => State Metadata :> es => Eff (Input : es) b -> Eff es b)
  -> World wm
  -> ActionCollection wm
  -> Eff (EffStack wm ++ '[IOE]) a
  -> IO (a, World wm)
convertToIO printHandler i w ac = runEff . convertToUnderlyingStack printHandler i w ac

convertToUnderlyingStack ::
  forall wm es' a.
  IOE :> es'
  => (Ord (WMDirection wm), Enum (WMDirection wm), Bounded (WMDirection wm), HasDirectionalTerms wm)
  -- => WithResponseSet wm An_Iso "listWriterResponses" (ListWriterResponses -> Response wm ())
  => HasLookingProperties wm
  => (forall es b. IOE :> es => State (World wm) :> es => Eff (Print : es) b -> Eff es b)
  -> (forall es b. IOE :> es => State Metadata :> es => Eff (Input : es) b -> Eff es b)
  -> World wm
  -> ActionCollection wm
  -> Eff (EffStack wm ++ es') a
  -> Eff es' (a, World wm)
convertToUnderlyingStack printHandler i w ac =
  fmap (either (error . show) id)
  . inject
  . runError
  . runBreadcrumbs Nothing
  . runStateShared w
  . printHandler
  . zoomState #actions
  . zoomState @(World wm) #metadata
  -- . runReader []
  . runQueryAsLookup
  . runTraverseAsLookup
  . evalStateShared ac
  . i
  . zoomState @(World wm) #activities
  . zoomState @(World wm) #responses
  . zoomState @(World wm) #adaptiveNarrative
  . runActionHandlerAsWorldActions

-- TODO: there's probably a much nicer way to make these traverse lens nicely
runTraverseAsLookup ::
  State (World wm) :> es
  => ObjectUpdate wm :> es
  => Eff (ObjectTraverse wm : es) a
  -> Eff es a
runTraverseAsLookup = interpret $ \env -> \case
  TraverseThings f -> do
    m <- use $ #stores % #things
    toList <$> mapM (\aT -> do
      r <- (\r -> localSeqUnlift env $ \unlift -> unlift $ f r) aT
      whenJust r setThing
      return (fromMaybe aT r)) m
  TraverseRooms f -> do
    m <- use $ #stores % #rooms
    toList <$> mapM (\aT -> do
      r <- (\r -> localSeqUnlift env $ \unlift -> unlift $ f r) aT
      whenJust r setRoom
      return (fromMaybe aT r)) m
  TraverseRegions f -> do
    m <- use $ #stores % #regions
    toList <$> mapM (\aT -> do
      r <- (\r -> localSeqUnlift env $ \unlift -> unlift $ f r) aT
      whenJust r setRegion
      return (fromMaybe aT r)) m

runQueryAsLookup ::
  HasCallStack
  => State (World wm) :> es
  => Eff (ObjectUpdate wm : ObjectLookup wm : es) a
  -> Eff es a
runQueryAsLookup = interpretLookup  . interpretUpdate

interpretLookup ::
  forall wm es a.
  HasCallStack
  => State (World wm) :> es
  => Eff (ObjectLookup wm : es) a
  -> Eff es a
interpretLookup = do
  let lookupHelper ::
        Entity
        -> Lens' (WorldStores wm) (Store wantedStore)
        -> Lens' (WorldStores wm) (Store b)
        -> Text
        -> Text
        -> Eff es (Either Text wantedStore)
      lookupHelper e l l' expected errTy = do
            let i = getID e
            mbObj <- use $ #stores % l % at i
            case mbObj of
              Nothing -> do
                mbRoom <- use $ #stores % l' % at i
                let (cs :: Text) = show callStack
                case mbRoom of
                  Nothing -> pure $ Left $ "Could not find the object " <> show i <> " as either a thing or room (Queried as a " <> show expected <> ")."
                  Just _ -> pure $ Left $ "Tried to lookup a " <> errTy <> " as a " <> show expected <> ":" <> show i <> ". (at: " <> show cs <> ")."
              Just ao -> pure $ Right ao
  interpret $ \_ -> \case
    LookupThing e -> lookupHelper (getID e) #things #rooms "thing" "room"
    LookupRoom e -> lookupHelper (getID e) #rooms #things "room" "thing"
    LookupRegion e -> do
      mbReg <- use $ #stores % #regions % at (unTag e)
      case mbReg of
        Nothing -> pure $ Left $ "could not find region with id " <> show e
        Just r -> pure $ Right r

interpretUpdate ::
  State (World wm) :> es
  => Eff (ObjectUpdate wm : es) a
  -> Eff es a
interpretUpdate = interpret $ \_ -> \case
  SetRoom r -> #stores % #rooms % at (getID r) %= updateIt r
  SetThing t -> #stores % #things % at (getID t) %= updateIt t
  SetRegion t -> #stores % #regions % at (unTag $ regionID t) %= updateIt t
  GenerateEntity bThing -> if bThing then
    (#stores % #entityCounter % _1) <<%= (Entity . (+1) . unID) else (#stores % #entityCounter % _2) <<%= (\x -> Entity $ unID x - 1)

updateIt :: a -> Maybe a -> Maybe a
updateIt newObj mbExisting = case mbExisting of
  Nothing -> Just newObj
  Just _ -> Just newObj

runGame ::
  forall wm a.
  (Ord (WMDirection wm), Enum (WMDirection wm), Bounded (WMDirection wm), HasDirectionalTerms wm)
  => HasLookingProperties wm
  => (forall es b. IOE :> es => State (World wm) :> es => Eff (Print : es) b -> Eff es b)
  -> (forall es b. IOE :> es => State Metadata :> es => Eff (Input : es) b -> Eff es b)
  -> World wm
  -> ActionCollection wm
  -> Eff (EffStack wm ++ '[IOE]) a
  -> IO (a, World wm)
runGame = convertToIO

runInputAsStdin ::
  Eff (Input : es) a
  -> Eff es a
runInputAsStdin = error "not implemented"

runInputAsBuffer ::
  State Metadata :> es
  => Eff (Input : es) a
  -> Eff es a
runInputAsBuffer = interpret $ \_ -> \case
  WaitForInput -> do
    buf' <- use #bufferedInput
    case buf' of
      [] -> return Nothing
      (x:xs) -> do
        #bufferedInput .= xs
        pure (Just x)

setInputBuffer ::
  State Metadata :> es
  => [Text]
  -> Eff es ()
setInputBuffer b = #bufferedInput .= b