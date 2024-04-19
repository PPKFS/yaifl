{-# LANGUAGE RecordWildCards #-}
module Building where

import Solitude
import Yaifl.Model.Entity
import Yaifl.Model.Kinds.Region
import Yaifl.Game.Create.Object
import Yaifl.Model.WorldModel
import Yaifl.Model.Effects
import Yaifl.Model.Kinds.Object
import Yaifl.Model.Query (isSubregionOf, areInRegion)
import Yaifl.Model.Kinds.Direction
import Named
import Yaifl.Model.HasProperty
import Yaifl.Model.Kinds.Enclosing
import Yaifl.Game.Create.RoomConnection
import Yaifl.Game.ObjectSpecifics (addDoor, WMHasObjSpecifics)
import Yaifl.Model.MultiLocated
import Yaifl.Gen.Plan
