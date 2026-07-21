module Main where

import Yaifl.Prelude

import Breadcrumbs
import Yaifl.Actions.Imports
import Yaifl
import Yaifl.ActionCollection
import Yaifl.Combinators
import Yaifl.Effects.Interpreters
import Yaifl.Effects.ObjectQuery
import Yaifl.Effects.Print
import Yaifl.Effects.RuleEffects
import Yaifl.Object.Kind
import Yaifl.ObjectSpecifics
import Yaifl.Room.Create
import Yaifl.Rulebooks.ActionProcessing
import Yaifl.Rulebooks.Run
import Yaifl.Text.ResponseCollection
import Yaifl.Thing.Create as T
import Yaifl.Thing.Kind
import qualified Data.Text as T
import Yaifl.Create.Rule





data ConstructionOptions wm = ConstructionOptions
  { activityCollectionBuilder :: ActivityCollection wm -> ActivityCollector wm
  , responseCollectionBuilder :: ResponseCollection wm -> ResponseCollector wm
  }

defaultOptions :: ConstructionOptions PlainWorldModel
defaultOptions = ConstructionOptions ActivityCollector ResponseCollector

isBlankDescription ::
  SayableValue (WMText wm) wm
  => RuleEffects wm es
  => Thing wm
  -> Eff es Bool
isBlankDescription d = T.null <$> sayText (d ^. #description)

scoreAndRankRule :: Rule' wm () r
scoreAndRankRule = makeRule' "score and rank rule" $ rulePass
{-
This is the score and rank rule:
	say "Your score is [score] (total of 350 points), in [turn count] move[if turn count is not 1]s[end if].[line break]This gives you the rank of ";
	let current-rank be "Beginner";
	repeat through the Table of Rankings:
		if the score is at least the score entry:
			now current-rank is the rank entry;
	say "[current-rank].[line break]".

-}
zorkWorld :: Game PlainWorldModel ()
zorkWorld = do
  setTitle "Zork I - The Great Underground Empire"
  whenPlayBegins $ makeRule' "set status line" $ do
    setLeftHandStatusLine "[the player's surroundings] [if in darkness] [otherwise]   Score: [score]/[turn count][end if]"
    setRightHandStatusLine ""
    rulePass

  afterActivity' #printingTheBannerText [] "print the authors and copyright" $ do
    printLn "Current"
    printLn "Original by Marc Blank, Dave Lebling, Bruce Daniels, and Tim Anderson"
    [saying|Copyright (c) 1981-1986 Infocom, Inc. ZIL source released under MIT License.#{paragraphBreak}|]
    [saying|Translated to Yaifl by Avery Garnett, based on the Inform 7 translation by John Escobedo.|]

  afterActivity' #printingThePlayersObituary [] "score and rank" $ do
    printLn ""
    runRule scoreAndRankRule ()
    pass

{-
Carry out requesting the score:
	if the player-is-dead is true:
		say "You're dead! How can you think of your score?";
		stop the action;
	follow the score and rank rule;
	stop the action.
-}
  westOfHouse <- addRoom' "West of House"
  pass

main :: IO ()
main = do
  r <- testHarness "Zork" defaultOptions zorkWorld
  mapM_ putTextLn (lines r)

testHarness ::
  forall wm a.
  HasStandardProperties wm
  => WMHasObjSpecifics wm
  => HasCallStack
  => Text
  -> ConstructionOptions wm
  -> Game wm a
  -> IO Text
testHarness fullTitle conOptions initWorld = do
  fst <<$>> runGame (runPrintPure @(World wm)) runInputAsBuffer (blankWorld (activityCollectionBuilder conOptions) (responseCollectionBuilder conOptions)) blankActionCollection $ do
      output <- withSpan' "test run" fullTitle $ do
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
                setInputBuffer []
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
