module Yaifl.EffectHandlers where

import Solitude hiding ( Reader, runReader )

import Breadcrumbs
import Data.Text.Display
import Effectful.Dispatch.Dynamic
import Effectful.Optics
import Effectful.Reader.Static
import Yaifl.Actions.Action
import Yaifl.Actions.Parser
import Yaifl.Metadata
import Yaifl.Model.Direction
import Yaifl.Model.Entity
import Yaifl.Model.Objects.Effects
import Yaifl.Model.WorldModel
import Yaifl.Rules.RuleEffects
import Yaifl.Text.AdaptiveNarrative
import Yaifl.Text.Print
import Yaifl.World
import Yaifl.Actions.Collection
import Effectful.Error.Static (Error, runError)


type EffStack (wm :: WorldModel) = '[
  ActionHandler wm
  , State (AdaptiveNarrative wm)
  , State (ResponseCollector wm)
  , State (ActivityCollector wm)
  , State (ActionCollection wm)
  , ObjectTraverse wm
  , ObjectUpdate wm
  , ObjectLookup wm
  , Reader [Text]
  , State Metadata
  , State (WorldActions wm)
  , ObjectCreation wm
  , Print
  , State (World wm)
  , Breadcrumbs
  , Error MissingObject
  , IOE
  ]

type Game wm = Eff (EffStack wm)

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

convertToUnderlyingStack ::
  forall wm a.
  (Ord (WMDirection wm), Enum (WMDirection wm), Bounded (WMDirection wm), HasDirectionalTerms wm, Display (WMSayable wm), SayableValue (WMSayable wm) wm)
  => World wm
  -> ActionCollection wm
  -> Eff (EffStack wm) a
  -> IO (a, World wm)
convertToUnderlyingStack w ac =
  fmap (either (error . show) id)
  . runEff
  . runError
  . runBreadcrumbs Nothing
  . runStateShared w
  . runPrintPure @(World wm)
  . runCreationAsLookup
  . zoomState #actions
  . zoomState @(World wm) #metadata
  . runReader []
  . runQueryAsLookup
  . runTraverseAsLookup
  . evalStateShared ac
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
    mapM_ (\aT -> do
      r <- (\r -> localSeqUnlift env $ \unlift -> unlift $ f r) aT
      whenJust r setThing) m
  TraverseRooms f -> do
    m <- use $ #stores % #rooms
    mapM_ (\aT -> do
      r <- (\r -> localSeqUnlift env $ \unlift -> unlift $ f r) aT
      whenJust r setRoom) m

runCreationAsLookup ::
  State (World wm) :> es
  => Eff (ObjectCreation wm : es) a
  -> Eff es a
runCreationAsLookup = interpret $ \_ -> \case
  GenerateEntity bThing -> if bThing then
    (#stores % #entityCounter % _1) <<%= (+1) else (#stores % #entityCounter % _2) <<%= (\x -> x-1)
  AddRoomToWorld aRoom -> #stores % #rooms % at (getID aRoom) ?= aRoom
  AddThingToWorld aThing -> #stores % #things % at (getID aThing) ?= aThing

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

interpretUpdate ::
  State (World wm) :> es
  => Eff (ObjectUpdate wm : es) a
  -> Eff es a
interpretUpdate = interpret $ \_ -> \case
  SetRoom r -> #stores % #rooms % at (getID r) %= updateIt r
  SetThing t -> #stores % #things % at (getID t) %= updateIt t

updateIt :: a -> Maybe a -> Maybe a
updateIt newObj mbExisting = case mbExisting of
  Nothing -> Just newObj
  Just _ -> Just newObj

runGame ::
  (Ord (WMDirection wm), Enum (WMDirection wm), Bounded (WMDirection wm), HasDirectionalTerms wm, Display (WMSayable wm), SayableValue (WMSayable wm) wm)
  => World wm
  -> ActionCollection wm
  -> Eff (EffStack wm) a
  -> IO (a, World wm)
runGame = convertToUnderlyingStack