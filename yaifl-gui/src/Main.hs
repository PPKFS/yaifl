{-# LANGUAGE RecordWildCards #-}
module Main where

import BearLibTerminal
import Breadcrumbs
import DisenchantmentBay4
import Effectful.Dispatch.Dynamic
import Effectful.State.Dynamic
import Rogue.Config
import Rogue.Window
import Yaifl
import Yaifl.Prelude
import Yaifl.Std.Create
import Yaifl.Std.Rulebooks.ActionProcessing
import MessageLog
import Gui
import EffectStack
import qualified Data.Text as T
import qualified Data.Text.Lazy.Builder as TLB

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
      void $ runEff $ evalStateLocal (GuiState (MessageLog bottomViewport []) False) $ convertStack w blankActionCollection $ do
          withSpan' "run" fullTitle $ do
            setPostPromptSpacing False
            modifyMessageLog id
            wa <- get @(WorldActions wm)
            beginPlay wa
            --when I write a proper game loop, this is where it needs to go
            runLoop
    )
    pass

main :: HasCallStack => IO ()
main = runWorld "Test" [] defaultOptions ex18