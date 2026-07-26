module Yaifl.Room.Create
  ( addRoom
  , addRoom'
  , addRoomInternal
  , RoomConfig(..)
  , newRoom

  ) where

import Yaifl.Prelude

import Yaifl.Object.Kind
import Yaifl.Effects.ObjectQuery
import Yaifl.Entity
import Yaifl.Object.Query
import Yaifl.Room.Kind ( RoomData, blankRoomData, Room (..), tagRoomEntity, isVoid, updateFirstRoom )
import Yaifl.WorldModel

import Yaifl.Object.Create
import Yaifl.Metadata (Metadata(..))
import Yaifl.Builder

data RoomConfig wm p = RoomConfig
  { description :: WMText wm
  , roomModify :: Eff '[State (Room wm)] ()
  } deriving stock (Generic)

newRoom :: IsString (WMText wm) => RoomConfig wm 'Complete
newRoom = RoomConfig
  { description = ""
  , roomModify = pass
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
  -> Eff es RoomEntity
addRoomInternal name desc objtype specifics details stateUpdate = do
  e <- Room <$> addObject (setRoom . Room) name desc objtype False specifics (fromMaybe blankRoomData details) Nothing
  md <- get @(Metadata wm)
  when (isVoid $ md ^. #firstRoom) (updateFirstRoom e)
  whenJust stateUpdate $ \su -> failHorriblyIfMissing $ modifyRoom e (`runLocalState` su)
  return (tagRoomEntity e)

addRoomInternal1 ::
  AddObjects wm es
  => WMText wm -- ^ Name.
  -> WMText wm -- ^ Description.
  -> Maybe (Eff '[State (Room wm)] v)
  -> Eff es RoomEntity
addRoomInternal1 n d rd = addRoomInternal n d (ObjectKind "room")
  Nothing Nothing (void <$> rd)

addRoom ::
  AddObjects wm es
  => WMText wm -- ^ Name.
  -> RoomConfig wm 'Complete
  -> Eff es RoomEntity
addRoom n config = addRoomInternal1 n (view #description config) (Just $ view #roomModify config)

addRoom' ::
  AddObjects wm es
  => WMText wm -- ^ Name.
  -> Eff es RoomEntity
addRoom' n = addRoom n newRoom
