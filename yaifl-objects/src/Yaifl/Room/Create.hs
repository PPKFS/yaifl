module Yaifl.Room.Create
  ( addRoom
  , addRoom'
  , addRoomInternal
  , RoomConfig(..)
  , newRoom
  , makeItDark
  , placeInRegion
  ) where

import Yaifl.Prelude

import Yaifl.Object.Kind
import Yaifl.Effects.ObjectQuery
import Yaifl.Entity
import Yaifl.Object.Query
import Yaifl.Room.Kind
import Yaifl.WorldModel

import Yaifl.Object.Create
import Yaifl.Metadata (Metadata(..))
import Yaifl.Region.Kind
import Yaifl.Region.Query

data RoomConfig wm = RoomConfig
  { description :: WMText wm
  , roomModify :: Eff '[State (Room wm)] ()
  , region :: Maybe RegionEntity
  } deriving stock (Generic)

newRoom :: IsString (WMText wm) => RoomConfig wm
newRoom = RoomConfig
  { description = ""
  , roomModify = pass
  , region = Nothing
  }

addRoomInternal ::
  forall wm es.
  AddObjects wm es
  => WMText wm -- ^ Name.
  -> WMText wm -- ^ Description.
  -> ObjectKind -- ^ Type.
  -> Maybe (WMObjSpecifics wm)
  -> Maybe (RoomData wm) -- ^
  -> Maybe (Eff '[State (Room wm)] ())
  -> Maybe RegionEntity
  -> Eff es RoomEntity
addRoomInternal name desc objtype specifics details stateUpdate reg = do
  e <- Room <$> addObject (setRoom . Room) name desc objtype False specifics (fromMaybe blankRoomData details) Nothing
  md <- get @(Metadata wm)
  when (isVoid $ md ^. #firstRoom) (updateFirstRoom e)
  whenJust stateUpdate $ \su -> failHorriblyIfMissing $ modifyRoom e (`runLocalState` su)
  whenJust reg $ \r -> failHorriblyIfMissing $ tagRoomEntity e `isInRegion` r
  return (tagRoomEntity e)

addRoomInternal1 ::
  AddObjects wm es
  => WMText wm -- ^ Name.
  -> WMText wm -- ^ Description.
  -> Maybe (Eff '[State (Room wm)] v)
  -> Maybe RegionEntity

  -> Eff es RoomEntity
addRoomInternal1 n d rd = addRoomInternal n d (ObjectKind "room")
  Nothing Nothing (void <$> rd)

addRoom ::
  AddObjects wm es
  => WMText wm -- ^ Name.
  -> RoomConfig wm
  -> Eff es RoomEntity
addRoom n config = addRoomInternal1 n (view #description config) (Just $ view #roomModify config) (view #region config)

addRoom' ::
  AddObjects wm es
  => WMText wm -- ^ Name.
  -> Eff es RoomEntity
addRoom' n = addRoom n newRoom

makeItDark ::
  RoomConfig wm
  -> RoomConfig wm
makeItDark = #roomModify %~ (>> #objectData % #darkness .= Dark)

placeInRegion ::
  RegionEntity
  -> RoomConfig wm
  -> RoomConfig wm
placeInRegion r = #region ?~ r