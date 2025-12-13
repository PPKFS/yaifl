module Main where

import Yaifl.Prelude
import Yaifl
import Yaifl.Core.Action
import Yaifl.Core.Rules.Rulebook
import Yaifl.Text.ResponseCollection
import Breadcrumbs

import Yaifl.Core.Effects
import Yaifl.Text.Print
import Yaifl.Core.Rules.Run
import Yaifl.Text.SayQQ
import Yaifl.Std.Create.Rule
import Yaifl.Core.Rules.RuleEffects
import Yaifl.Text.SayableValue
import Yaifl.Gen.Plan
import Yaifl.Gen.City.ApartmentTower
import Yaifl.Std.Kinds.Direction

data ConstructionOptions wm = ConstructionOptions
  { activityCollectionBuilder :: ActivityCollection wm -> ActivityCollector wm
  , responseCollectionBuilder :: ResponseCollection wm -> ResponseCollector wm
  }

defaultOptions :: ConstructionOptions PlainWorldModel
defaultOptions = ConstructionOptions ActivityCollector ResponseCollector
game :: Game PlainWorldModel ()
game = do
  setTitle fullTitle
  a <- constructApartmentBuilding
  print a
  before (ActionRule #going) [] "before climbing rule" $ \_ -> do
    [saying|You climb up the stairs to the next floor.|]
    rulePass
  pass


fullTitle :: Name
fullTitle = "City"

main :: IO ()
main = do
  o <- runGame runInputAsBuffer (blankWorld (activityCollectionBuilder defaultOptions) (responseCollectionBuilder defaultOptions)) blankActionCollection $ do
      output <- withSpan' "test run" fullTitle $ do
        withSpan' "worldbuilding" fullTitle $ do
          newWorld
          addStaircases
          game
          -- this just moves the actions from the indexed, static, standard library collection
          -- into the dynamic collection
          -- we do it here because we need to copy over changes to actions and we can't modify WrappedActions directly
          addStandardActions
        --withSpan "world verification" fullTitle $ do
        let runWorld :: ServiceName -> Game PlainWorldModel Text
            runWorld suffix = do
              withSpan' ("run " <> suffix) fullTitle $ do
                wa <- get @(WorldActions PlainWorldModel)
                unless (suffix == "") $ printLn suffix
                --when I write a proper game loop, this is where it needs to go
                failHorriblyIfMissing (runRulebook Nothing False (wa ^. #whenPlayBegins) ())
                setInputBuffer $ "up" : "up" : "up" : "x staircase" : "down" : "x stairs" : []
                runTurnsFromBuffer
                (w2 :: World PlainWorldModel) <- get
                let (x, _) = runPureEff $ runStateShared w2 $ do
                      -- take it down and flip it around
                      msgList <- gets (view $ #messageBuffer % #buffer % reversed)
                      return $ (mconcat . map show) msgList
                pure $ case w2 ^. #metadata % #errorLog of
                  [] -> x <> "\n"
                  xs -> x <> "\nEncountered the following errors:  \n" <> unlines (reverse xs)
        runWorld ""
      flush
      pure output
  putStrLn $ toString (fst o)