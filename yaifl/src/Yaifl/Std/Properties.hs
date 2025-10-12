module Yaifl.Std.Properties
  ( HasStandardProperties
  ) where

import Yaifl.Prelude hiding ( Reader, runReader )

import Yaifl.Core.Kinds.Enclosing
import Yaifl.Core.WorldModel
import Yaifl.Std.Actions.Looking.Visibility
import Yaifl.Std.Kinds.Container
import Yaifl.Std.Kinds.Device
import Yaifl.Std.Kinds.Direction
import Yaifl.Std.Kinds.Door
import Yaifl.Std.Kinds.Openable
import Yaifl.Std.Kinds.Person
import Yaifl.Text.Say
import Yaifl.Core.HasProperty

type HasStandardProperties s = (
  WMWithProperty s Enclosing
  , WMWithProperty s Openability
  , WMWithProperty s Container
  , WMWithProperty s Enterable
  , WMWithProperty s Device
  , WMWithProperty s Person
  , HasLookingProperties s
  , WMStdDirections s
  , WMWithProperty s Door
  , HasDirectionalTerms s
  , Pointed (WMObjSpecifics s)
  , SayableValue (WMText s) s
  )