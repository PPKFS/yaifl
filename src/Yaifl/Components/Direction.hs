module Yaifl.Components.Direction
(
    makeDirections, Direction(..)
  , directionComponent
  , north, northeast, east, southeast, south, southwest, west, northwest, up, down, insideDirection, outsideDirection
) where

import Yaifl.Common2
import Yaifl.Prelude
import Yaifl.World
import Yaifl.Say2
import Yaifl.Components.Object

newtype Direction = Direction { _opposite :: Entity } deriving (Eq, Show)

directionComponent :: Proxy Direction
directionComponent = Proxy
directionBlockIDs :: Entity
directionBlockIDs = -100

north :: Entity
north = directionBlockIDs
northeast :: Entity
northeast = directionBlockIDs + 1
east :: Entity
east = directionBlockIDs + 2
southeast :: Entity
southeast = directionBlockIDs + 3
south :: Entity
south = directionBlockIDs + 4
southwest :: Entity
southwest = directionBlockIDs + 5
west :: Entity
west = directionBlockIDs + 6
northwest :: Entity
northwest = directionBlockIDs + 7
up :: Entity
up = directionBlockIDs + 8
down :: Entity
down = directionBlockIDs + 9
insideDirection :: Entity
insideDirection = directionBlockIDs + 10
outsideDirection :: Entity
outsideDirection = directionBlockIDs + 11

makeDirection :: (WithLogging r, HasWorld w '[Direction] r) => Text -> Maybe Entity -> Sem r Entity
makeDirection n o = do
    e <- makeObject n "" "direction"
    whenJust o (setOpposite e)
    return e

setOpposite :: (HasWorld w '[Direction] r)  => Entity -> Entity -> Sem r ()
setOpposite e o = do
    addComponent e (Direction o)
    addComponent o (Direction e)

makeDirections :: (WithLogging r, HasWorld w '[Direction] r) => Sem r ()
makeDirections = do
    withEntityIDBlock (-100) (do
        n <- makeDirection "north" Nothing
        ne <- makeDirection "north-east" Nothing
        e <- makeDirection "east" Nothing
        se <- makeDirection "south-east" Nothing
        s <- makeDirection "south" $ Just n
        sw <- makeDirection "south-west" $ Just ne
        w <- makeDirection "west" $ Just e
        nw <- makeDirection "north-west" $ Just se
        u <- makeDirection "up" Nothing
        d <- makeDirection "down" $ Just u
        i <- makeDirection "inside" Nothing
        o <- makeDirection "outside" $ Just i
        pass)