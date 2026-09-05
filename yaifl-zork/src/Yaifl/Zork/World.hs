module Yaifl.Zork.World where

{-
TODO
Part 2 - The World
Chapter 1 - Forest and Outside of House
Section 1 - Regions
House Exterior is a region.
The Forest Area is a region.
The House Interior is a region.
The Underground is a region.
-}
import Yaifl.Prelude

import Yaifl.Actions.Imports
import Yaifl
import Yaifl.Effects.Print
import Yaifl.Create.Rule
import Yaifl.Run
import Yaifl.Visibility
import Yaifl.Person.Query (getPlayer')
import Yaifl.Text.DynamicText (text')
import Yaifl.Effects.RuleEffects
import Yaifl.Zork.Scoring
import Effectful.Writer.Static.Local (execWriter)
import Yaifl.Zork.Metadata
import Yaifl.Zork.Specifics
import Yaifl.Zork.World.Forest
import Yaifl.Zork.World.House
import Yaifl.Entity
import Yaifl.Zork.Actions
import Yaifl.Effects.Interpreters (WorldConstruction)
import Yaifl.Region.Create
import Yaifl.Zork.World.Inside

defaultZorkOptions :: ConstructionOptions ZorkWorldModel
defaultZorkOptions = ConstructionOptions ActivityCollector ResponseCollector defaultZorkValues baseZorkActions

zorkWorld :: WorldConstruction ZorkWorldModel ()
zorkWorld = do
  setTitle "Zork I - The Great Underground Empire"
  scoring
  whenPlayBegins $ makeRule' "set status line" $ do
    setLeftStatusBar $ text' "left status bar" $ do
      surroundings <- execWriter getPlayerSurroundings
      p <- getPlayer'
      notDarkness <- not <$> isInDarkness p
      score <- getScore
      turnCount <- getTurnCount
      [sayingTell|{surroundings}{?if notDarkness}   Score: {score}/{turnCount}{?end if}|]
    setRightStatusBar ""
    rulePass

  afterActivity' #printingTheBannerText [] "print the authors and copyright" $ do
    printLn "Original by Marc Blank, Dave Lebling, Bruce Daniels, and Tim Anderson."
    [saying|Copyright (c) 1981-1986 Infocom, Inc. ZIL source released under the MIT License.#{paragraphBreak}|]
    [saying|Translated to Yaifl by PPK, based on the Inform 7 translation by John Escobedo.|]
  forestRegion <- addRegion "Forest Area"

  o <- roomsOutsideTheHouse forestRegion
  (InsideTheHouse _livingRoom magicBoat) <- theHouseInterior (view #kitchen o) (view #houseInterior o)
  (Forest _f) <- forest forestRegion magicBoat o
  (#firstRoom :: Lens' (Metadata ZorkWorldModel) RoomEntity) .= westOfHouse o
  acs <- gets @(ActionCollector ZorkWorldModel) actionCollection
  addAction (view #finding acs)
  addAction (view #reading acs)
  pass