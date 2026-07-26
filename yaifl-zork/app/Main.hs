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
import Yaifl.Run

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
    setLeftStatusBar "[the player's surroundings] [if in darkness] [otherwise]   Score: [score]/[turn count][end if]"
    setRightStatusBar ""
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
  r <- gameHarness "Zork" defaultOptions zorkWorld
  mapM_ putTextLn (lines r)
