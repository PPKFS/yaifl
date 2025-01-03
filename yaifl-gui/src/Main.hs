module Main where

import Breadcrumbs
import Effectful.Dispatch.Dynamic
import Effectful.State.Dynamic
import Yaifl.Prelude

import Rogue.Colour
import Rogue.Config
import Rogue.Geometry.Rectangle
import Rogue.Rendering.Viewport
import Rogue.Window

import Rogue.Geometry.V2
import Yaifl
import Yaifl.Std.Create
import Yaifl.Core.Rules.RuleEffects
import Yaifl.Text.ResponseCollection
import Yaifl.Std.Rulebooks.ActionProcessing
import Yaifl.Std.EffectHandlers

import Yaifl.Core.Rules.Run

import qualified Data.Text as T

import Yaifl.Text.Print

import Yaifl.Core.Effects
import BearLibTerminal
import Rogue.Events

import DisenchantmentBay4
import Yaifl.Std.Actions.Imports


screenSize :: V2
screenSize = V2 230 80

topViewportRectangle :: Rectangle
topViewportRectangle = Rectangle (V2 0 0) (screenSize-V2 20 (view #y screenSize - 4))

bottomViewportRectangle :: Rectangle
bottomViewportRectangle = rectangleFromDimensions
  (V2 0 (bottomEdge topViewportRectangle))
  (V2 (view _1 screenSize - 20) (view #y screenSize - 4))

sideViewportRectangle :: Rectangle
sideViewportRectangle = rectangleFromDimensions
  (V2 (view _1 (bottomRight topViewportRectangle)) 0)
  (V2 20 (view _2 screenSize))

mapViewport :: Viewport ()
mapViewport = Viewport topViewportRectangle (Just (Colour 0xFF333333)) Nothing
bottomViewport :: Viewport ()
bottomViewport = Viewport bottomViewportRectangle (Just (Colour 0xFFFCF5E5)) (Just (unicodeBorders, Colour 0xFFDDDDDD))
sideViewport :: Viewport ()
sideViewport = Viewport sideViewportRectangle (Just (Colour 0xFF22AAFF)) (Just (unicodeBorders, Colour 0xFFFFFFFF))

data ConstructionOptions wm = ConstructionOptions
  { activityCollectionBuilder :: ActivityCollection wm -> ActivityCollector wm
  , responseCollectionBuilder :: ResponseCollection wm -> ResponseCollector wm
  }

defaultOptions :: ConstructionOptions PlainWorldModel
defaultOptions = ConstructionOptions ActivityCollector ResponseCollector

main :: IO ()
main = runWorld "Test" [] defaultOptions ex18

runWorld ::
  forall wm a b.
  HasStandardProperties wm
  => WMHasObjSpecifics wm
  => HasCallStack
  => Text
  -> [Text]
  -> ConstructionOptions wm
  -> (a, [Text], Game wm b)
  -> IO ()
runWorld fullTitle _ conOptions (_, actionsToDo, initWorld) = withWindow
    defaultWindowOptions { size = Just screenSize }
    (do
      terminalSetText "log: file='awa.log', level=trace;"
      terminalSetText "font: 'Iosevka-Term-02.ttf', codepage=437, size=16"
      terminalSetText "bold font: 'Iosevka-Term-Bold-02.ttf', codepage=437, size=16"
      -- perhaps a loading screen here
      terminalRefresh

      let emptyWorld = blankWorld (activityCollectionBuilder conOptions) (responseCollectionBuilder conOptions)
      (_, builtWorld) <- runGame runPrintPure runInputAsBuffer emptyWorld blankActionCollection $ do
        withSpan' "worldbuilding" fullTitle $ do
          newWorld
          initWorld
          -- this just moves the actions from the indexed, static, standard library collection
          -- into the dynamic collection
          -- we do it here because we need to copy over changes to actions and we can't modify WrappedActions directly
          addStandardActions
      pure builtWorld
    )
    (\w -> do
      void $ runGame runPrintPure runInputAsBuffer w blankActionCollection $ do
          withSpan' "run" fullTitle $ do
            wa <- get @(WorldActions wm)
            beginPlay wa
            --when I write a proper game loop, this is where it needs to go

            (w2 :: World wm) <- get
            let (x, _) = runPureEff $ runStateShared w2 $ do
                  -- take it down and flip it around
                  msgList <- gets (view $ #messageBuffer % #buffer % reversed)
                  return $ (mconcat . map show) msgList
            pure $ case w2 ^. #metadata % #errorLog of
              [] -> x <> "\n"
              xs -> x <> "\nEncountered the following errors:  \n" <> unlines (reverse xs)
          runLoop
    )
    pass
beginPlay ::
  HasStandardProperties wm
  => WMHasObjSpecifics wm
  => HasCallStack
  => WorldActions wm
  -> Game wm ()
beginPlay wa = do
  failHorriblyIfMissing (runRulebook Nothing False (wa ^. #whenPlayBegins) ())
  runOnce
  pass

{-}
runTurnsFromBuffer ::
  RuleEffects wm es
  => SayableValue (WMText wm) wm
  => State (WorldActions wm) :> es
  => Eff es ()
runTurnsFromBuffer = do
  b <- use @Metadata #bufferedInput
  unless (null b) $ runTurn >> runTurnsFromBuffer
-}

runOnce :: forall wm. Game wm Bool
runOnce = do
  msgList <- gets @(World wm)  (view $ #messageBuffer % #buffer % reversed)
  putStrLn $ show msgList
  renderBottomTerminal (show $ unwords $ map show msgList)
  terminalRefresh

  fmap (any id) $ handleEvents Blocking $ \case
    Keypress TkEsc -> return True
    Keypress kp -> putStrLn ("unknown keypress: " <> show kp) >> return False
    WindowEvent Resize -> pass >> return False
    WindowEvent WindowClose -> return True

runLoop :: Game wm ()
runLoop = do
  ifM runOnce pass runLoop

readTraceId :: IO TraceID
readTraceId = TraceID <$> readFileBS "traceid.temp"


renderBottomTerminal ::
  IOE :> es
  => Text
  -> Eff es ()
renderBottomTerminal t = do
  renderViewport bottomViewport $ do
    putStrLn $ show t
    viewportPrint (V2 1 1) Nothing (Colour 0xFF000000) $ t
  renderViewport sideViewport $ do
    viewportPrint (V2 3 3) Nothing (Colour 0xFF000000) "More info..."

{-}
makeAllViewshedsDirty :: (State World :> es, ObjectQuery Object :> es) => Eff es ()
makeAllViewshedsDirty = traverseObjects $ \t -> do
  let mbVs = getViewshedMaybe t
  whenJust mbVs $ const $ #dirtyViewsheds %= (getID t :)
  return Nothing

makeObject ::
  State World :> es
  => ObjectQuery Object :> es
  => Text
  -> ObjectKind
  -> V2
  -> Renderable
  -> ObjectSpecifics
  -> (Object -> Object)
  -> Eff es Entity
makeObject name kind pos renderable spec f = do
  t <- use #turn
  e <- generateEntity
  let obj = f $ Object
        { name
        , description = ""
        , objectId = e
        , objectType = kind
        , creationTime = t
        , modifiedTime = t
        , position = pos
        , renderable
        , objectData = spec
        , occupiesTile = True
        }
  setObject obj
  placeInWorld obj pos
  return e

playerRenderable :: Renderable
playerRenderable = Renderable '@' (fromRGB 0x75 0xa2 0xeb) (Colour 0x00000000)

playerData :: ObjectSpecifics
playerData = PlayerSpecifics $ Player { viewshed = Viewshed S.empty 20, combat = CombatStats 30 30 2 5 }

goblinRenderable :: Renderable
goblinRenderable = Renderable 'g' (fromRGB 255 30 30) (Colour 0x00000000)

orcRenderable :: Renderable
orcRenderable = Renderable 'o' (fromRGB 220 50 30) (Colour 0x00000000)

goblinData :: ObjectSpecifics
goblinData = MonsterSpecifics $ Monster { viewshed = Viewshed S.empty 20, combat = CombatStats 16 16 1 4 }

buildWorld ::
  State World :> es
  => ObjectQuery Object :> es
  => IOE :> es
  => Eff es ()
buildWorld = do
  rooms <- use @World (#tileMap % #rooms)
  let playerPos = case listToMaybe rooms of
        Nothing -> V2 20 15
        Just x -> centre x + V2 1 1
  p <- makeObject "player" (ObjectKind "player") playerPos playerRenderable playerData id
  #player .= p

  forM_ (zip [1..] rooms) $ \(i, room) -> do
    monsterChoice <- randomIO @Bool
    if monsterChoice then
      makeObject ("goblin #" <> show i) (ObjectKind "monster") (centre room) goblinRenderable goblinData id
    else
      makeObject ("orc #" <> show i) (ObjectKind "monster") (centre room) orcRenderable goblinData id
  makeAllViewshedsDirty
  updateViewsheds

defaultMetadata :: Metadata
defaultMetadata = Metadata False

data Direction = LeftDir | RightDir | UpDir | DownDir | UpLeftDir | DownRightDir | UpRightDir | DownLeftDir

playerId :: Entity
playerId = Entity 0

movementKeys :: M.Map Keycode Direction
movementKeys = M.fromList
  [ (TkA, LeftDir)
  , (TkS, DownDir)
  , (TkW, UpDir)
  , (TkD, RightDir)
  ]

asMovement :: Keycode -> Maybe Direction
asMovement k = k `M.lookup` movementKeys

data MoveArguments = MoveArguments
  { object :: Object
  , direction :: Direction
  }

instance Display MoveArguments where
  displayBuilder _ = "move args"

moveRulebook :: Rulebook Unconstrained MoveArguments Bool
moveRulebook = (blankRulebook "move rulebook")
  { rules =
      [ cantMoveIntoWalls
      , moveIt
      ]
  }

cantMoveIntoWalls :: Rule Unconstrained MoveArguments Bool
cantMoveIntoWalls = makeRule "can't walk into walls rule" [] $ \ma -> do
  let newLoc = calculateNewLocation (object ma) (direction ma)
  bm <- (!@ newLoc) <$> use @World (#tileMap % #walkableTiles)
  if bm then rulePass else return (Just False)

calculateNewLocation :: Object -> Direction -> V2
calculateNewLocation o dir = (o ^. #position) &
    (case dir of
      LeftDir -> _1 %~ subtract 1
      RightDir -> _1 %~ (+1)
      UpDir -> _2 %~ subtract 1
      DownDir -> _2 %~ (+1)
      UpRightDir -> (\(V2 x y) -> V2 (x+1) (y-1))
      DownRightDir -> (\(V2 x y) -> V2 (x+1) (y+1))
      UpLeftDir -> (\(V2 x y) -> V2 (x-1) (y-1))
      DownLeftDir -> (\(V2 x y) -> V2 (x-1) (y+1))
    )

moveIt :: Rule Unconstrained MoveArguments Bool
moveIt = makeRule "move rule" [] $ \ma -> do
  let newPos = simulateMove ma
  moveObject (object ma) newPos
  return (Just True)

runQueryAsState ::
  State World :> es
  => Eff (ObjectQuery Object : es) a
  -> Eff es a
runQueryAsState = interpret $ \env -> \case
  GenerateEntity -> #entityCounter <<%= (+1)
  SetObject r -> do
    #objects % at (getID r) %= updateIt r
    when (occupiesTile r) $ #tileMap % #walkableTiles %= \wt -> wt // (position r, False)
    --whenJust (getViewshedMaybe r) $ const (makeViewshedDirty . getID $ r)
  GetObject e -> do
    let i = getID e
    mbObj <- use $ #objects % at i
    case mbObj of
      Nothing -> error $ "Could not find object with id " <> show i
      Just x -> return x
  TraverseObjects f -> do
    m <- use #objects
    mapM_ (\aT -> do
      r <- (\r -> localSeqUnlift env $ \unlift -> unlift $ f r) aT
      whenJust r (\r' -> localSeqUnlift env $ \unlift -> unlift $ setObject r')) m

updateIt :: a -> Maybe a -> Maybe a
updateIt newObj mbExisting = case mbExisting of
  Nothing -> Just newObj
  Just _ -> Just newObj

moveObject ::
  ObjectQuery Object :> es
  => State World :> es
  => Object
  -> V2
  -> Eff es ()
moveObject o newPos = do
  removeFromWorld o
  modifyObject (getID o) (#position .~ newPos)
  placeInWorld o newPos
  -- at this point, we know the move will succeed




placeInWorld ::
  State World :> es
  => Object
  -> V2
  -> Eff es ()
placeInWorld o newPos = do
  #tileMap % #tileEntities % at newPos %= (Just . maybe (one (MobEntity (getID o))) ( S.insert (MobEntity (getID o))))
  #dirtyViewsheds %= (getID o:)

removeFromWorld ::
  State World :> es
  => Object -> Eff es ()
removeFromWorld o = do
  when (occupiesTile o) $ #tileMap % #walkableTiles %= \wt -> wt // (position o, True)
  #tileMap % #tileEntities % at (position o) % _Just %= S.delete (MobEntity (getID o))



determineMovementIntention ::
  IOE :> es
  => ObjectQuery Object :> es
  => State World :> es
  => State Metadata :> es
  => Breadcrumbs :> es
  => MoveArguments -> Eff es (Eff es ())
determineMovementIntention ma@(MoveArguments entity _) = do
  let newLoc = simulateMove ma
  mbThings <- filterMobTileEntities <$> getTileEntities newLoc
  case mbThings of
    [] -> return $ void $ runRulebook Nothing moveRulebook ma
    x:_ -> return $ void $ attack entity x

simulateMove :: MoveArguments -> V2
simulateMove (MoveArguments o dir) =
  position o &
    (case dir of
      LeftDir -> _1 %~ subtract 1
      RightDir -> _1 %~ (+1)
      UpDir -> _2 %~ subtract 1
      DownDir -> _2 %~ (+1)
      UpRightDir -> (\(V2 x y) -> V2 (x+1) (y-1))
      DownRightDir -> (\(V2 x y) -> V2 (x+1) (y+1))
      UpLeftDir -> (\(V2 x y) -> V2 (x-1) (y-1))
      DownLeftDir -> (\(V2 x y) -> V2 (x-1) (y+1))
    )

filterMobTileEntities :: S.Set TileEntity -> [Entity]
filterMobTileEntities = mapMaybe (preview _MobEntity) . S.toList

getTileEntities :: State World :> es => V2 -> Eff es (S.Set TileEntity)
getTileEntities pos = fromMaybe S.empty <$> use (#tileMap % #tileEntities % at pos)

attack :: IOE :> es => Object -> Entity -> Eff es ()
attack attacker defender = do
  print "from hell's heart I stab at thee"


getVisibleTiles ::
  ObjectQuery Object :> es
  => Eff es (S.Set V2)
getVisibleTiles = do
  pl <- getObject playerId
  let vs = fromMaybe (error "implement some proper fucking object tagging you dumb cunt") $ getViewshedMaybe pl
  return (vs ^. #visibleTiles)

renderBottomTerminal ::
  IOE :> es
  => Eff es ()
renderBottomTerminal = do
  renderViewport bottomViewport $ do
    viewportPrint (V2 4 5) Nothing (Colour 0xFF6644AA) "It is pitch black and you cannot see a thing."
  renderViewport sideViewport $ do
    viewportDrawTile (V2 3 3) Nothing (Colour 0xFF6644AA) '!'

renderMap ::
  State World :> es
  => IOE :> es
  => S.Set V2
  -> Eff es ()
renderMap vt = do
  w <- get
  renderViewport mapViewport $ do
    let es = w ^. #tileMap
    traverseArrayWithCoord_ (es ^. #revealedTiles) $ \p rev -> when rev $ whenInViewport mapViewport p $ do
      t <- use $ tile p
      let r = t ^. #renderable
      terminalColour (desaturate $ toGreyscale $ r ^. #foreground)
      terminalBkColour (desaturate $ toGreyscale $ r ^. #background)
      void $ withV2 p terminalPrintText (one $ r ^. #glyph)
    forM_ vt $ \a -> whenInViewport mapViewport a $ do
      t <- use $ tile a
      let r = t ^. #renderable
      terminalColour (r ^. #foreground)
      terminalBkColour (r ^. #background)
      --if (w ^. #tileMap % #walkableTiles) !@ a then terminalBkColour (Colour 0xFF0000FF) else terminalBkColour (Colour 0xFFFFFF44)
      void $ withV2 a terminalPrintText (one $ r ^. #glyph)
-}

