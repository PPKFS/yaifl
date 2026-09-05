{-# LANGUAGE RecordWildCards #-}
module Yaifl.Zork.World.Forest where

import Yaifl.Prelude

import Yaifl.Text.Say
import Yaifl.Direction.Kind (Direction(..))
import Yaifl.Region.Kind
import Yaifl.Zork.Specifics
import Yaifl.Effects.Interpreters
import Yaifl.Region.Create (addRegion)
import Yaifl.Room.Create
import Yaifl.Region.Query
import Yaifl.Room.Query
import Yaifl.Entity
import Yaifl.Zork.World.House
import Yaifl.Create.Rule
import Yaifl.Preconditions
import Yaifl.Backdrop.Create

import qualified Yaifl.Create.CustomActionRule as E
import Yaifl.Vehicle.Kind
import Yaifl.Actions.Going
import Yaifl.Object.Query
import Yaifl.Zork.Actions

newtype Forest = Forest RegionEntity
forest :: RegionEntity -> VehicleEntity -> OutsideTheHouse -> WorldConstruction ZorkWorldModel Forest
forest forestRegion magicBoat OutsideTheHouse{..} = do

  forest1 <- addRoom "Forest" $ newRoom
    & #description .~ "This is a forest, with trees in all directions. To the east, there appears to be sunlight. A faint breeze stirs the branches overhead, carrying the earthy scent of decaying leaves and damp moss."
  forest1 `isInRegion` forestRegion
  forest1 `isWestOf` westOfHouse

  forest2 <- addRoom "Forest" $ newRoom
    & #description .~ "This is a dimly lit forest, with large trees all around. The canopy here is thick, allowing only thin shafts of light to reach the forest floor. A carpet of pine needles muffles your footsteps."
  forest2 `isInRegion` forestRegion

  mountains <- addRoom "Forest" $ newRoom
    & #description .~ "The forest thins out, revealing impassable mountains."

  forest3 <- addRoom "Forest" $ newRoom
    & #description .~ "This is a dimly lit forest, with large trees all around. Gnarled roots break through the soil underfoot, and the air is heavy with the smell of wet bark. Somewhere nearby, water drips steadily from the leaves."
  forest3 `isInRegion` forestRegion

  forest3 `isSouthOf` southOfHouse
  southOfHouse `isNorthWestOf` forest3
  forest1 `isWestOf` forest3
  forest3 `isSouthOf` forest1

  forestPath <- addRoom "Forest Path" $ newRoom
    & #description .~ "This is a path winding through a dimly lit forest. The path heads north-south here. One particularly large tree with some low branches stands at the edge of the path."
  forestPath `isInRegion` forestRegion

  forestPath `isNorthOf` northOfHouse
  northOfHouse `isSouthOf` forestPath
  forest2 `isEastOf` forestPath
  forest1 `isWestOf` forestPath
  forestPath `isEastOf` forest1

  -- Grating Clearing is a room. The printed name of Grating Clearing is "Clearing".

  -- Grating Clearing is in Forest Area.
  gratingClearing <- addRoom "Clearing" newRoom
  gratingClearing `isInRegion` forestRegion
  forest1 `isNorthOf` gratingClearing

  forest2 `isEastOf` mountains
  forest2 `isNorthOf` mountains
  forest2 `isSouthOf` mountains
  forest2 `isWestOf` mountains

  -- North of Forest2 is nowhere. South of Forest2 is Clearing. West of Forest2 is Forest Path. East of Forest2 is Mountains.
  forest2 `isNowhere` North

  clearing <- addRoom "Clearing" $ newRoom
    & #description .~ "You are in a small clearing in a well marked forest path that extends to the east and west."
  clearing `isInRegion` forestRegion
  clearing `isSouthOf` forest2
  forestPath `isWestOf` forest2
  mountains `isEastOf` forest2

  forest3 `isSouthOf` clearing
  forest3 `isNowhere` East

  insteadOf' #going [inDirection North, whenPlayerIsIn forest2] [saying|The forest becomes impenetrable to the north.|]

  insteadOf' #going [inDirection East, whenPlayerIsIn forest3] [saying|The rank undergrowth prevents eastward movement.|]

  insteadOf' #going [inDirection South, whenPlayerIsIn forest3] [saying|Storm-tossed trees block your way.|]

  insteadOf' #going [inDirection Up, whenPlayerIsIn forest1] [saying|There is no tree here suitable for climbing.|]

  insteadOf' #going [inDirection Up, whenPlayerIsIn forest2] [saying|There is no tree here suitable for climbing.|]

  insteadOf' #going [inDirection Up, whenPlayerIsIn forest3] [saying|There is no tree here suitable for climbing.|]

  mountainRange <- addBackdrop "mountains" $ newBackdrop (InRooms (mountains:|[]))
    & #description .~ "The mountains are impassable."
  mountainRange `isUnderstoodAs` ["mountain", "mountains", "range", "impassable", "flathead"]

  insteadOf' #climbing [theObject mountainRange] [saying|Don't you believe me? The mountains are impassable!|]

  insteadOf' #going [inDirection Up, whenPlayerIsIn mountains] [saying|The mountains are impassable.|]

  insteadOf' #going [inDirection East, whenPlayerIsIn mountains] [saying|The mountains are impassable.|]

  insteadOf' #going [inDirection West, whenPlayerIsIn forest1] [saying|You would need a machete to go further west.|]

  upATree <- addRoom "Up a Tree" $ newRoom
    & #description .~ "You are about 10 feet above the ground nestled among some large branches. The nearest branch above you is above your reach."
  upATree `isInRegion` forestRegion
  upATree `isAbove` forestPath

  insteadOf' #going [inDirection Up, whenPlayerIsIn upATree] [saying|You cannot climb any higher.|]

  -- After looking in Up a Tree:
  --   let item-list be a list of things;
  --   repeat with item running through things in Forest Path:
  --     if the item is not scenery and the item is not undescribed:
  --       add item to item-list;
  --   if the number of entries in item-list > 0:
  --     say "On the ground below you can see: [item-list with indefinite articles]."
  -- Note: This requires filtering objects in Forest Path that are not scenery and are described
  -- We'll implement this as a placeholder for now since it needs more complex logic
  -- Placeholder implementation:
  -- afterActivity #looking "up a tree look rule" [whenPlayerIsIn upATree] $ const $ do
  --   Get all objects in forestPath
  --   Filter out scenery and undescribed items
  --   For now, we'll skip the complex implementation
  -- Instead, we'll just add a placeholder rule
  insteadOf' #examining [whenPlayerIsIn upATree] [saying|You are in a tree. Below you can see the forest path.|]

  clearing `isNorthOf` forest2
  clearing `isSouthOf` forest3
  behindHouse `isWestOf` clearing

  insteadOf' #going [inDirection Up, whenPlayerIsIn clearing] [saying|There is no tree here suitable for climbing.|]

  forestPath `isSouthOf` gratingClearing
  gratingClearing `isEastOf` forest2
  gratingClearing `isWestOf` forest1
  forestPath `isNorthOf` gratingClearing

  insteadOf' #going [inDirection North, whenPlayerIsIn gratingClearing] [saying|The forest becomes impenetrable to the north.|]

  -- Instead of going down in Grating Clearing:
  --   if the grate is not visible:
  --     say "You can't go that way." instead;
  --   if the grate is open:
  --     say "(through the grating)[line break]";
  --     move the player to Grating Room instead;
  --   otherwise:
  --     say "The grating is closed!" instead.
  -- Placeholder implementation - we need to check grate state and create Grating Room
  -- For now, just block going down
  insteadOf' #going [inDirection Down, whenPlayerIsIn gratingClearing] [saying|The grating is closed!|]

  forestSongbird <- addBackdrop "songbird" $ newBackdrop (InRegions (forestRegion:|[]))
    & #description .~ "The songbird is not here but is probably nearby."
  forestSongbird `isUnderstoodAs` ["bird", "songbird", "song"]

  insteadOf' #taking [theObject forestSongbird] [saying|The songbird is not here but is probably nearby.|]

  insteadOf' #listening [theObject forestSongbird] [saying|You can't hear the songbird now.|]

  -- Every turn when the player is in the Forest Area (this is the songbird singing rule):
  --   if a random chance of 15 in 100 succeeds:
  --     play the sound of bird-sfx as sfx;
  --     say "You hear in the distance the chirping of a song bird.[line break]"
  everyTurn "songbird singing rule" [whenPlayerIsInRegion forestRegion] $ do
    -- 15% chance - for now, we'll always trigger
    -- For now, we'll skip the sound
    [saying|You hear in the distance the chirping of a song bird.|]

  forestPseudo <- addBackdrop "forest" $ newBackdrop (InRegions (forestRegion:|[]))
    & #description .~ "You cannot see the forest for the trees."
  forestPseudo `isUnderstoodAs` ["forest"] -- there's something about "when the player is in the forest area" but idk what that means

  E.insteadOf' #finding [theObject forestPseudo] [saying|You cannot see the forest for the trees.|]

  insteadOf' #listening [theObject forestPseudo] [saying|The pines and the hemlocks seem to be murmuring.|]

  insteadOf' #exiting [whenPlayerIsInRegion forestRegion, not_ (whenPlayerIsIn magicBoat)] [saying|You will have to specify a direction.|]

  -- Instead of following the forest-songbird: say "It can't be followed."
  -- insteadOf' #following [theObject forestSongbird] [saying|It can't be followed.|]


  forestTrees <- addBackdrop "trees" $ newBackdrop (InRegions (forestRegion:|[]))
    & #description .~ "The trees are tall and closely grown."
  forestTrees `isUnderstoodAs` ["tree", "trees", "branch", "large", "forest", "pines", "hemlocks"]

  insteadOf' #listening [theObject forestTrees] [saying|The pines and the hemlocks seem to be murmuring.|]

  insteadOf #climbing [theObject forestTrees, whenPlayerIsIn forestPath] $ \a -> tryAction "go" [TheDirection Up] a >> pass

  insteadOf #climbing [theObject forestTrees, whenPlayerIsIn upATree] $ \a -> tryAction "go" [TheDirection Up] a >> pass
  return $ coerce forestRegion
