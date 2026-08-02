
module Yaifl.Backdrop.Create
  ( addBackdrop
  , BackdropConfig(..)
  , BackdropLocationsConfig(..)
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
import Yaifl.Region.Kind
import Yaifl.Region.Query
import Yaifl.Metadata (Metadata)

data BackdropConfig wm = BackdropConfig
  { description :: WMText wm
  , described :: ThingDescribed
  , initialAppearance :: WMText wm
  , thingModify :: Eff '[State (Thing wm)] ()
  , locations :: BackdropLocationsConfig
  } deriving stock (Generic)

data BackdropLocationsConfig = InRooms (NonEmpty RoomEntity) | InRegions (NonEmpty RegionEntity) | Everywhere
  deriving stock (Eq, Ord, Generic)

newBackdrop :: IsString (WMText wm) => BackdropLocationsConfig -> BackdropConfig wm
newBackdrop locations = BackdropConfig
  { description = ""
  , initialAppearance = ""
  , thingModify = pass
  , described = Described
  , locations
  }

backdropInRooms :: NonEmpty (Room wm) -> BackdropLocationsConfig
backdropInRooms = InRooms . NE.map tagRoomEntity

addBackdrop ::
  forall wm es.
  AddObjects wm es
  => WMText wm
  -> BackdropConfig wm
  -> Eff es ThingEntity
addBackdrop name BackdropConfig{initialAppearance, description, thingModify, described, locations} = do
  (mainLocation, backdrop) <- case locations of
    Everywhere -> do
      r <- coerceTag <$> use @(Metadata wm) #firstRoom
      return (r, Backdrop (MultiLocated S.empty) S.empty True)
    InRegions (r:|rs) -> do
      anyRoom <- fromMaybe voidID . listToMaybe . catMaybes <$>
        mapM (\region -> getRegion region >>= roomsInRegion >>= return . listToMaybe . S.toList) (r:rs)
      return (anyRoom, Backdrop (MultiLocated S.empty) (S.fromList (r:rs)) True)

    InRooms (l:|ls) -> return (l, (Backdrop (MultiLocated (S.fromList $ map coerceTag $ l:ls)) S.empty False))

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
        , specifics = inj (Proxy @wm) $ BackdropSpecifics backdrop
        , location = Just $ coerceTag mainLocation
        , objType = ObjectKind "backdrop"
        }

  when (has #_InRooms locations) $ updateMultiLocatedObject d
  pure d