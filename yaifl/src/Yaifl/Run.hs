module Yaifl.Run where

import Yaifl.Prelude

import Breadcrumbs
import Yaifl
import Yaifl.ActionCollection
import Yaifl.Effects.Interpreters
import Yaifl.Effects.ObjectQuery
import Yaifl.Effects.Print
import Yaifl.Effects.RuleEffects
import Yaifl.ObjectSpecifics
import Yaifl.Rulebooks.ActionProcessing
import Yaifl.Rulebooks.Run
import Yaifl.Text.ResponseCollection

data ConstructionOptions wm = ConstructionOptions
  { activityCollectionBuilder :: ActivityCollection wm -> ActivityCollector wm
  , responseCollectionBuilder :: ResponseCollection wm -> ResponseCollector wm
  , conValues :: WMValues wm
  }

defaultOptions :: (WMActivities wm ~ ActivityCollection wm, WMValues wm ~ (), WMResponses wm ~ ResponseCollection wm) => ConstructionOptions wm
defaultOptions = ConstructionOptions ActivityCollector ResponseCollector ()

gameHarness ::
  forall wm a.
  HasStandardProperties wm
  => WMHasObjSpecifics wm
  => HasCallStack
  => Text
  -> ConstructionOptions wm
  -> Game wm a
  -> [Text]
  -> IO Text
gameHarness fullTitle conOptions initWorld buffer = do
  fst <<$>> runGame (runPrintPure @(World wm)) runInputAsBuffer (blankWorld (conValues conOptions) (activityCollectionBuilder conOptions) (responseCollectionBuilder conOptions)) blankActionCollection $ do
      output <- withSpan' "game run" fullTitle $ do
        withSpan' "worldbuilding" fullTitle $ do
          newWorld
          initWorld
          -- this just moves the actions from the indexed, static, standard library collection
          -- into the dynamic collection
          -- we do it here because we need to copy over changes to actions and we can't modify WrappedActions directly
          addStandardActions
        --withSpan "world verification" fullTitle $ do
        let runWorld suffix = do
              withSpan' ("run " <> suffix) fullTitle $ do
                wa <- get @(WorldActions wm)
                unless (suffix == "") $ printLn suffix
                --when I write a proper game loop, this is where it needs to go
                failHorriblyIfMissing (runRulebook Nothing False (wa ^. #whenPlayBeginsRulebook) ())
                setInputBuffer buffer
                runTurnsFromBuffer
                (w2 :: World wm) <- get
                let (x, _) = runPureEff $ runStateShared w2 $ do
                      -- take it down and flip it around
                      msgList <- gets (view $ #messageBuffer % #buffer % reversed)
                      return $ (mconcat . map show) msgList
                pure $ case w2 ^. #metadata % #errorLog of
                  [] -> x <> "\n"
                  _ -> x <> "\n"
                  -- xs -> x <> "\nEncountered the following errors:  \n" <> unlines (reverse xs)
        runWorld ""
      flush
      pure output