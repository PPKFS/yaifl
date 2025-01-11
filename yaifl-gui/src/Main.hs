{-# LANGUAGE RecordWildCards #-}
module Main where

import BearLibTerminal
import Breadcrumbs
import DisenchantmentBay4
import Effectful.Dispatch.Dynamic
import Effectful.State.Dynamic
import Rogue.Colour
import Rogue.Config
import Rogue.Events
import Rogue.Geometry.Rectangle
import Rogue.Geometry.V2
import Rogue.Rendering.Viewport
import Rogue.Window
import Yaifl
import Yaifl.Core.Effects
import Yaifl.Core.Rules.RuleEffects
import Yaifl.Core.Rules.Run
import Yaifl.Prelude
import Yaifl.Std.Create
import Yaifl.Std.EffectHandlers
import Yaifl.Std.Rulebooks.ActionProcessing
import Yaifl.Text.Print
import Yaifl.Text.ResponseCollection
import Prettyprinter
import qualified Data.Text.Lazy.Builder as TLB
import qualified Data.Text as T

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

main :: HasCallStack => IO ()
main = runWorld "Test" [] defaultOptions ex18

runWorld ::
  forall wm b a.
  HasStandardProperties wm
  => WMHasObjSpecifics wm
  => HasCallStack
  => Text
  -> [Text]
  -> ConstructionOptions wm
  -> (a, [Text], Game wm b)
  -> IO ()
runWorld fullTitle _ conOptions (_, _actionsToDo, initWorld) = withWindow
    defaultWindowOptions { size = Just screenSize }
    (do
      initialiseTerminal
      -- perhaps a loading screen here
      terminalRefresh
      makeWorld conOptions fullTitle initWorld
    )
    (\w -> do
      void $ runGame runPrintPure runInputFromGUI w blankActionCollection $ do
          withSpan' "run" fullTitle $ do
            setPostPromptSpacing False
            wa <- get @(WorldActions wm)
            beginPlay wa
            --when I write a proper game loop, this is where it needs to go
            runLoop
    )
    pass

runInputFromGUI ::
  IOE :> es
  => Eff (Input : es) a
  -> Eff es a
runInputFromGUI = interpret $ \_ -> \case
  WaitForInput -> let
    go = do
      do
        terminalLayer 5
        terminalReadStr 30 30 50
    in withViewport bottomViewport $ go


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

initialiseTerminal :: IO ()
initialiseTerminal = do
  terminalSetText "log: file='awa.log', level=trace;"
  terminalSetText "font: 'Iosevka-Term-02.ttf', codepage=437, size=16"
  terminalSetText "bold font: 'Iosevka-Term-Bold-02.ttf', codepage=437, size=16"
  pass

makeWorld ::
  HasStandardProperties wm
  => WMHasObjSpecifics wm
  => ConstructionOptions wm
  -> Text
  -> Game wm b
  -> IO (World wm)
makeWorld conOptions fullTitle initWorld = do
  let emptyWorld = blankWorld (activityCollectionBuilder conOptions) (responseCollectionBuilder conOptions)
  snd <$$> runGame runPrintPure runInputAsBuffer emptyWorld blankActionCollection $ do
    withSpan' "worldbuilding" fullTitle $ do
      newWorld
      initWorld
      -- this just moves the actions from the indexed, static, standard library collection
      -- into the dynamic collection
      -- we do it here because we need to copy over changes to actions and we can't modify WrappedActions directly
      addStandardActions

runOnce ::
  HasStandardProperties wm
  => WMHasObjSpecifics wm
  => Game wm Bool
runOnce = do
  renderAll
  terminalRefresh
  fmap (any id) $ handleEvents Blocking $ \case
    Keypress TkEsc -> return True
    Keypress kp -> runTurn >> return False
    WindowEvent Resize -> pass >> return False
    WindowEvent WindowClose -> return True

runLoop ::
  HasStandardProperties wm
  => WMHasObjSpecifics wm
  => Game wm ()
runLoop = do
  r <- runOnce
  if r then pass else runLoop

getMessageBuffer :: forall wm. Game wm [StyledDoc MessageAnnotation]
getMessageBuffer = gets @(World wm) (view $ #messageBuffer % #buffer)

renderAll :: forall wm. Game wm ()
renderAll = do
  msgList <- gets @(World wm) (view $ #messageBuffer % #buffer % reversed)
  putStrLn (show msgList)
  renderBottomTerminal msgList
  renderSideTerminal
  where
    renderSideTerminal = do
      renderViewport sideViewport $
        viewportPrint (V2 3 3) Nothing (Colour 0xFF000000) "More info..."

textSpaces :: Int -> Text
textSpaces n = T.replicate n (T.singleton ' ')

renderBottomTerminal ::
  IOE :> es
  => Text
  -> Eff es ()
renderBottomTerminal t = do
  print t
  renderViewport bottomViewport $ do
    viewportPrint (V2 1 1) Nothing (Colour 0xFF000000) $ t
  where

