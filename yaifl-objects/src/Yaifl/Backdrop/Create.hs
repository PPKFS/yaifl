
module Yaifl.Backdrop.Create
  ( addBackdrop
  , BackdropConfig(..)
  , newBackdrop
  , backdropInRooms
  ) where

import Yaifl.Prelude

import Yaifl.Entity
import Yaifl.Object.Kind
import Yaifl.Object.Create
import Yaifl.Thing.Kind
import Yaifl.MultiLocated.Kind
import qualified Data.Set as S
import Yaifl.Backdrop.Kind
import Yaifl.WorldModel
import Yaifl.ObjectSpecifics
import Yaifl.Thing.Create
import Yaifl.MultiLocated.Query
import Yaifl.Builder
import Yaifl.ObjectLike
import Yaifl.Room.Kind
import Yaifl.Enclosing.Kind
import qualified Data.List.NonEmpty as NE

data BackdropConfig wm p = BackdropConfig
  { description :: WMText wm
  , described :: ThingDescribed
  , initialAppearance :: WMText wm
  , thingModify :: Eff '[State (Thing wm)] ()
  , locations :: NonEmpty EnclosingEntity
  } deriving stock (Generic)

newBackdrop :: IsString (WMText wm) => NonEmpty EnclosingEntity -> BackdropConfig wm 'Defaults
newBackdrop locations = BackdropConfig
  { description = ""
  , initialAppearance = ""
  , thingModify = pass
  , described = Described
  , locations
  }

backdropInRooms :: NonEmpty (Room wm) -> NonEmpty EnclosingEntity
backdropInRooms = NE.map (coerceTag . tagRoomEntity)

addBackdrop ::
  forall wm es.
  AddObjects wm es
  => WMText wm
  -> BackdropConfig wm 'Complete
  -> Eff es ThingEntity
addBackdrop name BackdropConfig{initialAppearance, description, thingModify, described, locations=l:|ls} = do
  d <- addThing name newThing
        { initialAppearance
        , description
        , thingModify = do
            -- A backdrop is usually scenery.
            #objectData % #isScenery .= True
            thingModify
            #objectData % #portable .= FixedInPlace
            #objectData % #pushableBetweenRooms .= False
            #objectData % #described .= described
        , specifics = inj (Proxy @wm) $ BackdropSpecifics (Backdrop (MultiLocated (S.fromList $ l:ls)))
        , location = Just l
        , objType = ObjectKind "backdrop"
        }

  updateMultiLocatedObject d
  pure d