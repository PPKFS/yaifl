module Yaifl.Directions where

import Yaifl.Common
import Yaifl.Prelude
import Yaifl.Properties (addObject, addThing)

newtype Direction = Direction
  { _directionName :: Text
  , _opposite :: Entity
  } deriving (Eq, Show)

directionBlockIDs :: Entity
directionBlockIDs = 2

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

makeDirection :: 
  Text 
  -> Maybe Entity 
  -> m Entity
makeDirection n o = do
  e <- addThing n Text ObjType (Maybe (Either ObjectSpecifics s)) (Maybe ThingData) (Maybe (ObjectUpdate s ThingData)) n "" "direction"
  whenJust o (setOpposite e)
  return e

setOpposite ::
  Text 
  -> Maybe Entity 
  -> m Entity
setOpposite e o = do
  addComponent e (Direction o)
  addComponent o (Direction e)

makeDirections :: m ()
makeDirections = do
  n <- makeDirection "north" Nothing
  ne <- makeDirection "north-east" Nothing
  e <- makeDirection "east" Nothing
  se <- makeDirection "south-east" Nothing
  _ <- makeDirection "south" $ Just n
  _ <- makeDirection "south-west" $ Just ne
  _ <- makeDirection "west" $ Just e
  _ <- makeDirection "north-west" $ Just se
  u <- makeDirection "up" Nothing
  _ <- makeDirection "down" $ Just u
  i <- makeDirection "inside" Nothing
  _ <- makeDirection "outside" $ Just i
  pass
