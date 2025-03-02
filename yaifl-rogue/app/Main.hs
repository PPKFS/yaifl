{-# LANGUAGE RecordWildCards #-}
module Main where

import BearLibTerminal
import Breadcrumbs
import Effectful.Dispatch.Dynamic
import Effectful.State.Dynamic
import Rogue.Config
import Rogue.Window
import Yaifl
import Yaifl.Prelude
import Yaifl.Std.Rulebooks.ActionProcessing
import MessageLog
import Gui
import EffectStack
import Yaifl.Std.Actions.Collection
import Yaifl ()
import Rooms

runWorld ::
  forall b a.
  HasCallStack
  => Text
  -> [Text]
  -> ConstructionOptions SpatialWorldModel
  -> Game SpatialWorldModel b
  -> IO ()
runWorld fullTitle _ conOptions initWorld = withWindow
    defaultWindowOptions { size = Just screenSize }
    (do
      initialiseTerminal
      -- perhaps a loading screen here
      terminalRefresh
      makeWorld conOptions fullTitle initWorld
    )
    (\w -> do
      void $ runEff $ evalStateLocal (GuiState (MessageLog (bottomViewport 60) []) False 60) $ convertStack w blankActionCollection $ do
          withSpan' "run" fullTitle $ do
            setPostPromptSpacing False
            modifyMessageLog id
            wa <- get @(WorldActions SpatialWorldModel)
            beginPlay wa
            --when I write a proper game loop, this is where it needs to go
            runLoop
    )
    pass

main :: HasCallStack => IO ()
main = runWorld "Test" [] defaultOptions world