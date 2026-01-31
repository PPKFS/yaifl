module Yaifl.Room.Create
  ( addRoom
  , addRoom'
  , addRoomInternal

  ) where

import Yaifl.Prelude

import Yaifl.Object.Kind
import Yaifl.Effects.ObjectQuery
import Yaifl.Entity
import Yaifl.Object.Query
import Yaifl.Room.Kind ( RoomData, blankRoomData, Room (..), tagRoomEntity, isVoid, updateFirstRoom )
import Yaifl.Enclosing.Kind ( Enclosing )
import Yaifl.WorldModel

import Yaifl.Property.Has
import Yaifl.Object.Create
import Yaifl.Metadata (Metadata(..))

addRoomInternal ::
  WMWithProperty wm Enclosing
  => AddObjects wm es
  => WMText wm -- ^ Name.
  -> WMText wm -- ^ Description.
  -> ObjectKind -- ^ Type.
  -> Maybe (WMObjSpecifics wm)
  -> Maybe (RoomData wm) -- ^
  -> Maybe (Eff '[State (Room wm)] ())
  -> Eff es RoomEntity
addRoomInternal name desc objtype specifics details stateUpdate = do
  e <- Room <$> addObject (setRoom . Room) name desc objtype False specifics (fromMaybe blankRoomData details) Nothing
  md <- get @Metadata
  when (isVoid $ md ^. #firstRoom) (updateFirstRoom e)
  whenJust stateUpdate $ \su -> failHorriblyIfMissing $ modifyRoom e (`runLocalState` su)
  return (tagRoomEntity e)

addRoomInternal1 ::
  WMWithProperty wm Enclosing
  => AddObjects wm es
  => WMText wm -- ^ Name.
  -> WMText wm -- ^ Description.
  -> Maybe (Eff '[State (Room wm)] v)
  -> Eff es RoomEntity
addRoomInternal1 n d rd = addRoomInternal n d (ObjectKind "room")
  Nothing Nothing (void <$> rd)

addRoom ::
  WMWithProperty wm Enclosing
  => AddObjects wm es
  => WMText wm -- ^ Name.
  -> "description" :? WMText wm -- ^ Description.
  -> "modify" :? Eff '[State (Room wm)] ()
  -> Eff es RoomEntity
addRoom n (argDef #description "" -> d) (argF #modify -> rd) = addRoomInternal1 n d rd

addRoom' ::
  WMWithProperty wm Enclosing
  => AddObjects wm es
  => WMText wm -- ^ Name.
  -> "description" :? WMText wm -- ^ Description.
  -> Eff es RoomEntity
addRoom' n (argDef #description "" -> d) = addRoomInternal1 n d Nothing
