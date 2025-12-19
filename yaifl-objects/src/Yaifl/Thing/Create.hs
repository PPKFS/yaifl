module Yaifl.Thing.Create
  ( makeItScenery

  ) where

import Yaifl.Prelude

import GHC.Records
import Yaifl.Entity
import Yaifl.Object.Kind
import Yaifl.Room.Kind
import Yaifl.Tag
import Yaifl.WorldModel

makeItScenery :: Eff '[State (Thing wm)] ()
makeItScenery = #objectData % #isScenery .= True