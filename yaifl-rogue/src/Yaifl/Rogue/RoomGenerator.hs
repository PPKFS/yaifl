module Yaifl.Rogue.RoomGenerator where

import Yaifl.Prelude
import Rogue.Geometry.V2
import Rogue.Array2D.Boxed
import qualified Data.Vector as V
import Rogue.Colour
import Rogue.Geometry.Rectangle
import Yaifl.Core.Kinds.Room
import Yaifl.Rogue.PositionData (RoomSpatialData(..))
import Yaifl.Core.WorldModel


digRectangle ::
  a
  -> Rectangle
  -> Array2D a
  -> Array2D a
digRectangle a r = (\x -> x //@ map (,a) (rectanglePoints Horizontal r))

digHorizontalTunnel ::
  a
  -> V2
  -> Int
  -> Array2D a
  -> Array2D a
digHorizontalTunnel a p l = (\x -> x //@ map (\i -> (p & _1 %~ (+ if l > 0 then i else -i),a)) [0..(abs l)] )

digVerticalTunnel ::
  a
  -> V2
  -> Int
  -> Array2D a
  -> Array2D a
digVerticalTunnel a p l = (\x -> x //@ map (\i -> (p & _2 %~ (+ if l > 0 then i else -i), a)) [0..(abs l)] )

buildHollowRoom :: WMRoomData wm ~ RoomSpatialData a => a -> a -> V2 -> V2 -> Eff '[State (RoomData wm)] ()
buildHollowRoom wall f globalPos size = do
  let rectBlock = (Array2D (V.generate (size ^. #x * size ^. #y) (const wall), size))
      dugRoom = digRectangle f (rectangleFromDimensions (V2 1 1) (V2 (size ^. #x - 2) (size ^. #y - 2))) rectBlock
  #roomData .= RoomSpatialData globalPos dugRoom