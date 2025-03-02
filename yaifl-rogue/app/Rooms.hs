module Rooms where

import Yaifl.Prelude
import Yaifl
import Yaifl.Std.Create
import Yaifl.Std.Kinds.Direction
import Yaifl.Text.ResponseCollection
import Yaifl.Text.DynamicText
import Yaifl.Rogue.PositionData


import Yaifl.Std.Actions.Going
import Yaifl.Text.SayableValue
import Yaifl.Text.Say
import Yaifl.Core.ObjectLike
import Yaifl.Core.Rules.Rulebook
import Rogue.Geometry.V2
import Yaifl.Core.Kinds.Room
import Rogue.Array2D.Boxed
import qualified Data.Vector as V
import Rogue.Colour
import Rogue.Geometry.Rectangle
import Yaifl.Rogue.RoomGenerator

type SpatialWorldModel = 'WorldModel ObjectSpecifics Direction () ThingSpatialData (RoomSpatialData TileInfo) () ActivityCollection ResponseCollection DynamicText

data TileInfo = TileInfo
  { name :: Text
  , renderable :: Renderable
  , walkable :: Bool
  } deriving stock (Generic, Show, Eq, Ord)

instance Pointed TileInfo where
  identityElement = wall


floorTile :: TileInfo
floorTile = TileInfo "floor" (Renderable '.' (Colour 0xFF008888) (Colour 0x00000000)) True

wall :: TileInfo
wall = TileInfo "wall" (Renderable '#' (Colour 0xFF00FF00) (Colour 0x00000000)) False

data Tiles = Tiles
  { tileMap :: Array2D TileInfo

  } deriving stock (Eq, Ord, Show, Generic)

digRectangle ::
  Rectangle
  -> Array2D TileInfo
  -> Array2D TileInfo
digRectangle r = (\x -> x //@ map (,floorTile) (rectanglePoints Horizontal r))

digHorizontalTunnel ::
  V2
  -> Int
  -> Array2D TileInfo
  -> Array2D TileInfo
digHorizontalTunnel p l = (\x -> x //@ map (\i -> (p & _1 %~ (+ if l > 0 then i else -i),floorTile)) [0..(abs l)] )

digVerticalTunnel ::
  V2
  -> Int
  -> Array2D TileInfo
  -> Array2D TileInfo
digVerticalTunnel p l = (\x -> x //@ map (\i -> (p & _2 %~ (+ if l > 0 then i else -i), floorTile)) [0..(abs l)] )

world :: Game SpatialWorldModel ()
world = do
  setTitle "The Unbuttoned Elevator Affair"
  uh <- addRoom' "UNCLE Headquarters"
    "The steel nerve-center of the free world's battle against the Technological Hierarchy for the Removal of Undesirables and the Subjugation of Humanity. Being against technology, we have only a very simple elevator to the east."
    (buildHollowRoom wall floorTile (V2 5 5) (V2 5 5))
  dfts <- addRoom' "Del Floria's Tailor Shop"
    "Only trained anti-THRUSH agents recognise the booth in the east wall as a secret elevator."
    (buildHollowRoom wall floorTile (V2 5 5) (V2 30 30))
  tse <- addRoom' "The Secret Elevator" "" (buildHollowRoom wall floorTile (V2 5 5) (V2 5 5))
  tse `isEastOf` uh
  tse `isEastOf` dfts

  after (ActionRule #going) [toTheRoom tse] "ag1" $ const $ do
    [saying|The doors automatically close, there is a rush of motion, and they open again.|]
    tseR <- getRoom tse
    -- if UNCLE Headquarters is mapped west of the Secret Elevator
    if getMapConnection West tseR == Just uh
    then
      -- now Del Floria's Tailor Shop is mapped west of the Secret Elevator;
      isNowMapped dfts West tse
    else
      -- otherwise now UNCLE Headquarters is mapped west of the Secret Elevator;
      isNowMapped uh West tse
    rulePass
