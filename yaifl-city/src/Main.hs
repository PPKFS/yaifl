module Main where

import Solitude
import Yaifl
import Yaifl.Model.Action
import Yaifl.Game.World
import Yaifl.Model.Rules.RuleEffects
import Yaifl.Text.ResponseCollection
import Breadcrumbs
import Yaifl.Model.Input
import Yaifl.Model.Effects
import Yaifl.Text.Print
import Yaifl.Model.Rules.Run
import Yaifl.Model.Metadata
import Yaifl.Model.Kinds.Region
import Yaifl.Game.Create.Object
import Building
import Yaifl.Text.SayQQ
import Yaifl.Game.Create.Rule
import Yaifl.Model.Rules.Rulebook

data ConstructionOptions wm = ConstructionOptions
  { activityCollectionBuilder :: ActivityCollection wm -> ActivityCollector wm
  , responseCollectionBuilder :: ResponseCollection wm -> ResponseCollector wm
  }

defaultOptions :: ConstructionOptions PlainWorldModel
defaultOptions = ConstructionOptions ActivityCollector ResponseCollector
game :: Game PlainWorldModel ()
game = do
  setTitle fullTitle
 -- a <- runPlan apartmentBuildingPlan (1, 10000)
  --print a
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
                setInputBuffer $ take 190 $ repeat "up"
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