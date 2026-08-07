module Yaifl.Chapter3.Garibaldi
  ( ex22
  ) where

import Yaifl.Prelude

import Yaifl (PlainWorldModel)

import Yaifl.Object.Kind
import Yaifl.Effects.Interpreters
import Yaifl.Metadata
import Yaifl.Test.Common
import Yaifl.Direction.Kind
import Yaifl.Text.SayableValue
import Yaifl.Actions.Imports
import Yaifl.Entity
import Yaifl.Effects.ObjectQuery
import Yaifl.Door.Kind
import Yaifl.Openable.Kind
import Yaifl.Device.Create as D
import Yaifl.Create.Rule
import Yaifl.Person.Query
import Yaifl.Room.Create
import Yaifl.Door.Create as Door
import Yaifl.Device.Kind (Device(..), getDeviceMaybe)
import Yaifl.Thing.Kind (Thing)
import Yaifl.TH
import qualified Yaifl.Device.Kind as Device
import Yaifl.Combinators

ex22 :: (Text, [Text], WorldConstruction PlainWorldModel ())
ex22 = ("Garibaldi", escapeTestMeWith, garibaldiWorld)

garibaldiWorld :: WorldConstruction PlainWorldModel ()
garibaldiWorld = do
  setTitle "Garibaldi"
  sr <- addDevice "security readout" $ newDevice
    { D.description = "The screen is blank."
    }
  insteadOf #examining [theObjectWhenSwitchedOn sr] $ \_ -> do
    [saying|The screen reads:#{linebreak}|] -- missing a "[fixed letter spacing]"
    traverseThings_ $ \thing -> do
      whenJust (getDoorMaybe thing) $ \door -> do
        let frontSide = door ^. #frontSide
            backSide = door ^. #backSide
            locked = isLocked door
        [saying|#{linebreak} {thing} ({frontSide}/{backSide}): {?if locked}LOCKED{?else}UNLOCKED{?end if} |]
      rulePass
    [saying|#{paragraphBreak}|]
  p <- getPlayer
  sr `isNowCarriedBy` p

  tdb <- addRoom "Docking Bay" $ newRoom
    & makeRoomImproperlyNamed
  tz <- addRoom "Zocalo"$ newRoom
    & makeRoomImproperlyNamed
  s <- addRoom' "Space"
  ml <- addRoom' "Medlab"
  tia <- addDoor "inner airlock" $ newDoor (tdb, northOf) (tz, southOf)
  pass

theObjectWhenSwitchedOn :: forall wm a. (WMWithProperty wm Device, ArgsMightHaveMainObject a (Thing wm)) => ThingEntity -> Precondition PlainWorldModel (Args wm a)
theObjectWhenSwitchedOn th = Precondition (pure "the object when switched on") $ \args -> do
  let (mbT :: Maybe (Thing wm)) = args ^? #variables % argsMainObjectMaybe
  let d = mbT >>= getDeviceMaybe
  return $ Just (getEntity th) == (getEntity <$> mbT) && Just True == (Device.switchedOn <$> d)

escapeTestMeWith :: [Text]
escapeTestMeWith = fromI7TestMe [wrappedText|x readout / turn on readout / x readout / lock inner airlock with security pass / x readout|]