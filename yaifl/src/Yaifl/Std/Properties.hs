module Yaifl.Std.Properties
  ( HasStandardProperties
  ) where

import Yaifl.Prelude hiding ( Reader, runReader )

import Yaifl.Enclosing.Kind
import Yaifl.WorldModel
import Yaifl.Std.Actions.Looking.Visibility
import Yaifl.Container.Kind
import Yaifl.Device.Kind
import Yaifl.Direction.Kind
import Yaifl.Door.Kind
import Yaifl.Openable.Kind
import Yaifl.Person.Kind
import Yaifl.Text.Say
import Yaifl.Property.Has
import Yaifl.MultiLocated.Kind

type HasStandardProperties s = (
  WMWithProperty s Enclosing
  , WMWithProperty s Openability
  , WMWithProperty s Container
  , WMWithProperty s Enterable
  , WMWithProperty s Device
  , WMWithProperty s Person
  , WMWithProperty s MultiLocated
  , HasLookingProperties s
  , WMStdDirections s
  , WMWithProperty s Door
  , HasDirectionalTerms s
  , Pointed (WMObjSpecifics s)
  , SayableValue (WMText s) s
  )