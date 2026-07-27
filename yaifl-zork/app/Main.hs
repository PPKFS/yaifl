module Main where

import Yaifl.Prelude

import Yaifl.Actions.Imports
import Yaifl
import Yaifl.Effects.Print
import Yaifl.Room.Create
import Yaifl.Create.Rule
import Yaifl.Run
import Yaifl.Visibility
import Yaifl.Person.Query (getPlayer')
import Yaifl.Text.DynamicText (text, DynamicText)
import Yaifl.ObjectSpecifics
import Yaifl.Direction.Kind
import Yaifl.Text.ResponseCollection (ResponseCollection)
import Yaifl.Effects.RuleEffects
import Yaifl.Zork.Scoring


defaultZorkOptions :: ConstructionOptions ZorkWorldModel
defaultZorkOptions = ConstructionOptions ActivityCollector ResponseCollector defaultZorkValues

data ZorkData = ZorkData
  { trollFlag :: Bool
  , magicFlag :: Bool
  , cyclopsFlag :: Bool
  , domeFlag :: Bool
  , lldFlag :: Bool
  , lowTideFlag :: Bool
  , rainbowFlag :: Bool
  , wonFlag :: Bool
  , grateRevealed :: Bool
  , gateFlag :: Bool
  , gatesOpenFlag :: Bool
  , luckyFlag :: Bool

  , playerDeaths :: Int
  , playerIsDead :: Bool
  , alwaysLitMode :: Bool
  } deriving stock (Eq, Ord, Generic, Show)

type ZorkWorldModel = 'WorldModel ObjectSpecifics Direction ZorkData () () () ActivityCollection ResponseCollection DynamicText ActionCollection

defaultZorkValues :: ZorkData
defaultZorkValues = ZorkData
  { trollFlag = False
  , magicFlag = False
  , cyclopsFlag = False
  , domeFlag = False
  , lldFlag = False
  , lowTideFlag = False
  , rainbowFlag = False
  , wonFlag = False
  , grateRevealed = False
  , gateFlag = False
  , gatesOpenFlag = False
  , luckyFlag = True

  , playerDeaths = 0
  , playerIsDead = False
  , alwaysLitMode = False

  }
zorkWorld :: Game ZorkWorldModel ()
zorkWorld = do
  setTitle "Zork I - The Great Underground Empire"
  #metadata % #score % #maxScore .= Just 350
  whenPlayBegins $ makeRule' "set status line" $ do
    setLeftStatusBar $ text "left status bar" $ do
      surroundings <- getPlayerSurroundings
      p <- getPlayer'
      notDarkness <- not <$> isInDarkness p
      score <- getScore
      turnCount <- getTurnCount
      [sayingTell|{surroundings}{?if notDarkness}   Score: {score}/{turnCount}{?end if}|]
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
  r <- gameHarness "Zork" (defaultZorkOptions) zorkWorld
  mapM_ putTextLn (lines r)
