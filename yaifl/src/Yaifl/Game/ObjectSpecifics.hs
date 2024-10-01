module Yaifl.Game.ObjectSpecifics
  ( -- * Specifics
  ObjectSpecifics(..)
  , WMHasObjSpecifics(..)
  , addDoor
  , addDevice
  , addPerson
  , addContainer
  , addSupporter
  ) where

import Yaifl.Prelude

import Yaifl.Model.Metadata ( noteError )
import Yaifl.Model.Kinds.Direction (WMStdDirections)
import Yaifl.Model.Entity
import Yaifl.Model.Kinds.Object
import Yaifl.Game.Create.Object
import Yaifl.Model.Effects
import Yaifl.Game.Move
import Yaifl.Model.ObjectLike
import Yaifl.Game.Create.RoomConnection
import Yaifl.Model.Kinds.Thing
import Yaifl.Model.Kinds.Container
import Yaifl.Model.Kinds.Door
import Yaifl.Model.Kinds.Enclosing ( Enclosing (..), blankEnclosing )
import Yaifl.Model.HasProperty ( MayHaveProperty(..), WMWithProperty )
import Yaifl.Model.MultiLocated
import Yaifl.Model.Kinds.Openable
import Yaifl.Model.Query
import Yaifl.Model.WorldModel ( WMObjSpecifics, WorldModel(..), WMText, WMDirection )
import qualified Data.Set as S
import Yaifl.Model.Tag
import Yaifl.Model.Kinds.Device
import Yaifl.Model.Kinds.Person
import Yaifl.Model.Rules (RuleEffects)
import Yaifl.Model.Kinds.Supporter

data ObjectSpecifics =
  NoSpecifics
  | EnclosingSpecifics Enclosing
  | ContainerSpecifics Container
  | OpenabilitySpecifics Openability
  | DoorSpecifics Door
  | DeviceSpecifics Device
  | PersonSpecifics Person
  | SupporterSpecifics Supporter
  deriving stock (Eq, Show, Read)

makePrisms ''ObjectSpecifics

instance Pointed ObjectSpecifics where
  identityElement = NoSpecifics

class WMHasObjSpecifics (wm :: WorldModel) where
  inj :: Proxy wm -> ObjectSpecifics -> WMObjSpecifics wm

instance WMHasObjSpecifics ('WorldModel ObjectSpecifics a b c ac r se) where
  inj _ = id

instance MayHaveProperty ObjectSpecifics Enclosing where
  propertyAT = _EnclosingSpecifics
    `thenATraverse` (_ContainerSpecifics % #enclosing)
    `thenATraverse` (_SupporterSpecifics % #enclosing)
    `thenATraverse` (_PersonSpecifics % #carrying)

instance MayHaveProperty ObjectSpecifics MultiLocated where
  propertyAT = _DoorSpecifics % #multiLocated --`thenATraverse` (_ContainerSpecifics % containerEnclosing)

instance MayHaveProperty ObjectSpecifics Container where
  propertyAT = castOptic _ContainerSpecifics

instance MayHaveProperty ObjectSpecifics Enterable where
  propertyAT = _ContainerSpecifics % #enterable

instance MayHaveProperty ObjectSpecifics Openability where
  propertyAT = _OpenabilitySpecifics
    `thenATraverse` (_ContainerSpecifics % #openable)
    `thenATraverse` (_DoorSpecifics % #opened)

instance MayHaveProperty ObjectSpecifics Door where
  propertyAT = castOptic _DoorSpecifics

instance MayHaveProperty ObjectSpecifics Device where
  propertyAT = castOptic _DeviceSpecifics

instance MayHaveProperty ObjectSpecifics Person where
  propertyAT = castOptic _PersonSpecifics

instance MayHaveProperty ObjectSpecifics Supporter where
  propertyAT = castOptic _SupporterSpecifics

localST ::
  State st :> es
  => (st -> st)
  -> Eff es a
  -> Eff es a
localST f l = do
  b <- get
  modify f
  r <- l
  put b
  pure r

--TODO: I am only going to enforce implications as a quickcheck or hedgehog
-- invariant, namely that the door type will have a smart ctr (addDoor) that makes sure
-- it isn't portable on creation, and then the check will be whenever we modify an object
-- make sure that it isn't breaking an invariant.
addDoor ::
  forall wm es.
  WMHasObjSpecifics wm
  => WMWithProperty wm Enclosing
  => WMStdDirections wm
  => WMWithProperty wm MultiLocated
  => RuleEffects wm es
  => AddObjects wm es
  => WMText wm -- ^ name
  -> "front" :! (RoomEntity, WMDirection wm)
  -> "back" :! (RoomEntity, WMDirection wm)
  -> "initialAppearance" :? WMText wm
  -> "description" :? WMText wm -- ^ Description.
  -> "described" :? ThingDescribed
  -> "modify" :? Eff '[State (Thing wm)] () -- ^ Build your own thing monad!
  -> "thingData" :? ThingData wm -- ^ Optional details; if 'Nothing' then the default is used.
  -> Eff es DoorEntity
addDoor n (arg #front -> f) (arg #back -> b) ia des (argDef #described Described -> desc) (argDef #modify pass -> upD) (argF #thingData -> mbD) = do
  let ds = blankDoor (fst f) (fst b)
  d <- addThing @wm n ia des
    ! #specifics (inj (Proxy @wm) $ DoorSpecifics ds)
    ! #modify (do
      upD
      #objectData % #portable .= FixedInPlace
      #objectData % #pushableBetweenRooms .= False
      #objectData % #described .= desc)
    ! #type (ObjectKind "door")
    ! paramF #thingData mbD
    ! #location (coerceTag $ fst f)
    ! done
      -- A door is always fixed in place.
      -- A door is never pushable between rooms.
  updateMultiLocatedObject d
  let tagged = tag @Door @DoorTag ds d
  addDoorToConnection tagged f b
  pure (tag ds d)

updateMultiLocatedObject ::
  WMWithProperty wm MultiLocated
  => WMWithProperty wm Enclosing
  => NoMissingObjects wm es
  => ThingLike wm tl
  => tl
  -> Eff es ()
updateMultiLocatedObject tl = do
  t <- getThing tl
  case getMultiLocatedMaybe t of
    Nothing -> noteError (const ()) "the object had no multilocated component"
    Just ml -> mapM_ (\x -> do
      obj <- getObject x
      let enc = getEnclosing x obj
      updateToContain obj enc t) (S.toList $ ml ^. #locations)

addDevice ::
  forall wm es.
  WMHasObjSpecifics wm
  => WMWithProperty wm Enclosing
  => AddObjects wm es
  => WMText wm -- ^ Name.
  -> "initialAppearance" :? WMText wm
  -> "description" :? WMText wm -- ^ Description.
  -> "device" :? Device
  -> Eff es ThingEntity
addDevice n ia d (argDef #device identityElement -> dev) = addThing @wm n ia d
  ! #specifics (inj (Proxy @wm) (DeviceSpecifics dev))
  ! #type (ObjectKind "device")
  ! done

addContainer ::
  forall wm es.
  WMHasObjSpecifics wm
  => WMWithProperty wm Enclosing
  => AddObjects wm es
  => WMText wm -- ^ Name.
  -> "initialAppearance" :? WMText wm
  -> "description" :? WMText wm
  -> "carryingCapacity" :? Int
  -> "opacity" :? Opacity
  -> "enterable" :? Enterable
  -> "openable" :? Openable
  -> "opened" :? Opened
  -> "location" :? EnclosingEntity
  -> "portable" :? ThingPortable
  -> Eff es ContainerEntity
addContainer n ia d
  (argF #carryingCapacity -> cc)
  (argF #opacity -> op)
  (argF #enterable -> e)
  (argF #openable -> o)
  (argF #opened -> od)
  (argF #location -> l)
  (argF #portable -> p) = do
    let cs = makeContainer cc op e o od
    c <- addThing @wm n ia d
        ! #specifics (inj (Proxy @wm) $ ContainerSpecifics cs)
        ! #type (ObjectKind "container")
        ! paramF #location l
        ! paramF #portable p
        ! done
    pure $ tag @Container @ContainerTag cs c

addSupporter ::
  forall wm es.
  WMHasObjSpecifics wm
  => WMWithProperty wm Enclosing
  => AddObjects wm es
  => WMText wm -- ^ Name.
  -> "initialAppearance" :? WMText wm
  -> "description" :? WMText wm
  -> "carryingCapacity" :? Int
  -> "location" :? EnclosingEntity
  -> "enterable" :? Enterable
  -> Eff es SupporterEntity
addSupporter n ia d
  (argF #carryingCapacity -> cc) (argF #location -> l) (argF #enterable -> e) = do
    let enc = (blankEnclosing { capacity = cc <|> Just 100 })
        sup = Supporter enc (fromMaybe NotEnterable e)
    c <- addThing @wm n ia d
        ! #specifics (inj (Proxy @wm) $ SupporterSpecifics sup)
        ! #type (ObjectKind "container")
        ! paramF #location l
        ! done
    pure $ tag @_ @SupporterTag sup c

addPerson ::
  forall wm es.
  WMHasObjSpecifics wm
  => WMWithProperty wm Enclosing
  => AddObjects wm es
  => WMText wm -- ^ Name.
  -> "gender" :! Gender
  -> "initialAppearance" :? WMText wm
  -> "description" :? WMText wm -- ^ Description.
  -> "carrying" :? Enclosing
  -> Eff es ThingEntity
addPerson n (Arg g) ia d (argF #carrying -> e) = addThing @wm n ia d
  ! #specifics (inj (Proxy @wm) (PersonSpecifics (Person g (fromMaybe defaultPersonEnclosing e))))
  ! #type (case g of
    Male -> ObjectKind "man"
    Female -> ObjectKind "woman"
    NonBinary -> ObjectKind "person"
    Other _ -> ObjectKind "person")
  ! done