module Yaifl.Zork.Scoring where

import Yaifl.Prelude
import Yaifl.Actions.Imports
import Yaifl
import Yaifl.Effects.Print
import Yaifl.Create.Rule
import Yaifl.Activities.PrintingThePlayersObituary

rankings :: [(Int, Text)]
rankings =
  [ (350, "Master Adventurer")
  , (330, "Wizard")
  , (300, "Master")
  , (200, "Adventurer")
  , (100, "Junior Adventurer")
  , (50, "Novice Adventurer")
  , (25, "Amateur Adventurer")
  ]

scoreToRank :: Int -> Text
scoreToRank score = fromMaybe "Beginner" $ snd <$> (find (\x -> score >= fst x) rankings)
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
Carry out requesting the score:
  if the player-is-dead is true:
    say "You're dead! How can you think of your score?";
    stop the action;
  follow the score and rank rule;
  stop the action.
-}


scoring :: WithPrintingThePlayersObituary wm => SayableValue (WMText wm) wm => Game wm ()
scoring = do
  (#score :: Lens' (Metadata wm) Score) % #maxScore .= Just 350
  afterActivity' #printingThePlayersObituary [] "score and rank" $ do
    printLn ""
    runRule scoreAndRankRule ()
    pass

{-
Chapter 7 - Trophy Case Scoring
The trophy-case-score is a number that varies. The trophy-case-score is 0.
Every turn (this is the trophy case scoring rule):
  let new-score be 0;
  repeat with item running through things in the trophy case:
    increase new-score by the treasure-value of the item;
    repeat with inner running through things enclosed by the item:
      increase new-score by the treasure-value of the inner;
  if new-score is not the trophy-case-score:
    let diff be new-score minus the trophy-case-score;
    increase the score by diff;
    now the trophy-case-score is new-score;
  if the score is at least 350 and the won-flag is false:
    now the won-flag is true;
    now the ancient map is zil-visible;
    say "[line break]An almost inaudible voice whispers in your ear, [quotation mark]Look to your treasures for the final secret.[quotation mark][line break]".
Chapter 8 - Treasure Values
A thing has a number called treasure-value.
A person can be defeated. A person is usually not defeated. The treasure-value of a thing is usually 0.
A thing has a number called point-value. The point-value of a thing is usually 0.
After taking something when the point-value of the noun is greater than 0 (this is the first-take scoring rule):
  increase the score by the point-value of the noun;
  now the point-value of the noun is 0;
  continue the action.
-}