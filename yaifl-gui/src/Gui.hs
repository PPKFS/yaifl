module Gui where

import BearLibTerminal
import Breadcrumbs
import Effectful.Dispatch.Dynamic
import Effectful.State.Dynamic
import Rogue.Colour
import Rogue.Events
import Rogue.Geometry.Rectangle
import Rogue.Geometry.V2
import Rogue.Rendering.Viewport
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
import qualified Data.Text as T
import MessageLog
import Data.Char (isPrint)


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

mapViewport :: Viewport a
mapViewport = Viewport topViewportRectangle (Just (Colour 0xFF333333)) Nothing
bottomViewport :: Viewport MainPart
bottomViewport = Viewport bottomViewportRectangle (Just (Colour 0xFFFCF5E5)) (Just (unicodeBorders, Colour 0xFFDDDDDD))
sideViewport :: Viewport SidePart
sideViewport = Viewport sideViewportRectangle (Just (Colour 0xFF22AAFF)) (Just (unicodeBorders, Colour 0xFFFFFFFF))

data ConstructionOptions wm = ConstructionOptions
  { activityCollectionBuilder :: ActivityCollection wm -> ActivityCollector wm
  , responseCollectionBuilder :: ResponseCollection wm -> ResponseCollector wm
  }

defaultOptions :: ConstructionOptions PlainWorldModel
defaultOptions = ConstructionOptions ActivityCollector ResponseCollector


data MainPart = MainPart
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)

instance AsLayer MainPart where
  toLayer = const 1

data SidePart = SidePart
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)

instance AsLayer SidePart where
  toLayer = const 2

data GuiState = GuiState
  { messageLog :: MessageLog MainPart
  , pendingQuit :: Bool
  } deriving stock (Show, Generic)

modifyMessageLog ::
  State GuiState :> es
  => (MessageLog MainPart -> MessageLog MainPart)
  -> Eff es ()
modifyMessageLog f = #messageLog %= f


runInputFromGUI ::
  IOE :> es
  => State GuiState :> es
  => Eff (Input : es) a
  -> Eff es a
runInputFromGUI = interpret $ \_ -> \case
  WaitForInput -> let
    go = do
      do
        terminalLayer 5
        s <- terminalReadStr 30 30 50
        --when (isNothing s) $ #pendingQuit .= True
        return Nothing --if (s == Just "" || (T.all isPrint <$?> s)) then return Nothing else return s
    in withViewport bottomViewport $ go


beginPlay ::
  YaiflEffects wm es
  => State GuiState :> es
  => WorldActions wm
  -> Eff es ()
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
  YaiflEffects wm es
  => State GuiState :> es
  => Eff es Bool
runOnce = do
  renderAll
  terminalRefresh
  runTurn
  fmap (any id) $ handleEvents NotBlocking $ \case
    Keypress TkEsc -> return True
    WindowEvent Resize -> pass >> return False
    WindowEvent WindowClose -> return True
    x -> print x >> return False

runLoop ::
  YaiflEffects wm es
  => State GuiState :> es
  => Eff es ()
runLoop = do
  r <- runOnce
  pend <- use @GuiState #pendingQuit
  if r || pend then pass else runLoop

getMessageBuffer :: forall wm. Game wm [StyledDoc MessageAnnotation]
getMessageBuffer = gets @(World wm) (view $ #messageBuffer % #buffer)

renderAll ::
  forall wm es.
  YaiflEffects wm es
  => State GuiState :> es
  => Eff es ()
renderAll = do
  msgList <- gets @(World wm) (view $ #messageBuffer % #buffer % reversed)
  modifyBuffer (#buffer .~ [])
  updateMessageLog msgList
  -- let msgLog = for_ msgList
  renderBottomTerminal
  renderSideTerminal
  where
    renderSideTerminal = do
      renderViewport sideViewport $
        viewportPrint (V2 3 3) Nothing (Colour 0xFF000000) "More info..."

updateMessageLog ::
  IOE :> es
  => State GuiState :> es
  => [StyledDoc MessageAnnotation]
  -> Eff es ()
updateMessageLog msgs = do
  gs <- get
  ml' <- addMessage msgs (messageLog gs)
  #messageLog .= ml'


textSpaces :: Int -> Text
textSpaces n = T.replicate n (T.singleton ' ')

renderBottomTerminal ::
  IOE :> es
  => State GuiState :> es
  => Eff es ()
renderBottomTerminal = do
  gs <- get
  renderMessageLog AnchorBottom (messageLog gs)
