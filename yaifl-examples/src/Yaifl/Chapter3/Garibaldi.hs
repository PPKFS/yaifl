module Yaifl.Chapter3.Garibaldi
  ( ex22
  ) where

import Yaifl.Prelude

import Yaifl (PlainWorldModel)

import Yaifl.Object.Kind
import Yaifl.Object.Create
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
import Yaifl.Device.Create
import Yaifl.Create.Rule
import Yaifl.Person.Query
import Yaifl.Room.Create
import Yaifl.Door.Create
import Yaifl.Device.Kind (Device(..), getDeviceMaybe)
import Yaifl.Thing.Kind (Thing)
import Yaifl.TH

ex22 :: (Text, [Text], Game PlainWorldModel ())
ex22 = ("Garibaldi", escapeTestMeWith, garibaldiWorld)

garibaldiWorld :: Game PlainWorldModel ()
garibaldiWorld = do
  setTitle "Garibaldi"
  sr <- addDevice "security readout"
    ! #initialAppearance "The one major source of entertainment is the holographic projector, a top of the line Misthon 9000, on which you view every beam you can get."
    ! #description "The screen is blank."
    ! done
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

  tdb <- addRoom "Docking Bay" ! #modify makeNameImproper ! done
  tz <- addRoom "Zocalo" ! #modify makeNameImproper ! done
  s <- addRoom' "Space" ! done
  ml <- addRoom "Medlab" ! done
  tia <- addDoor $ newDoor
    { name = "inner airlock"
    , front = (tdb, northOf)
    , back = (tz, southOf)
    , locked = Just Unlocked
    }
  pass

theObjectWhenSwitchedOn :: forall wm a. (WMWithProperty wm Device, ArgsMightHaveMainObject a (Thing wm)) => ThingEntity -> Precondition PlainWorldModel (Args wm a)
theObjectWhenSwitchedOn th = Precondition (pure "the object when switched on") $ \args -> do
  let (mbT :: Maybe (Thing wm)) = args ^? #variables % argsMainObjectMaybe
  let d = mbT >>= getDeviceMaybe
  return $ Just (getEntity th) == (getEntity <$> mbT) && Just True == (switchedOn <$> d)

escapeTestMeWith :: [Text]
escapeTestMeWith = fromI7TestMe [wrappedText|x readout / turn on readout / x readout / lock inner airlock with security pass / x readout|]