module Yaifl.Std.ObjectSpecifics
  ( -- * Specifics
  ObjectSpecifics(..)
  , WMHasObjSpecifics(..)
  , addDoor
  , addDevice
  , addPerson
  , addContainer
  , addSupporter
  , addBackdrop
  , addBaseObjects
  ) where

import Yaifl.Prelude

import Yaifl.Core.Metadata ( noteError )
import Yaifl.Std.Kinds.Direction (WMStdDirections)
import Yaifl.Core.Entity
import Yaifl.Core.Kinds.Object
import Yaifl.Std.Create.Object
import Yaifl.Core.Effects
import Yaifl.Std.Move
import Yaifl.Core.ObjectLike
import Yaifl.Std.Create.RoomConnection
import Yaifl.Core.Kinds.Thing
import Yaifl.Std.Kinds.Container
import Yaifl.Std.Kinds.Door
import Yaifl.Core.Kinds.Enclosing ( Enclosing (..), blankEnclosing )
import Yaifl.Core.HasProperty ( MayHaveProperty(..) )
import Yaifl.Std.Kinds.MultiLocated
import Yaifl.Std.Kinds.Openable
import qualified Data.Set as S
import Yaifl.Core.Tag
import Yaifl.Std.Kinds.Device
import Yaifl.Std.Kinds.Person
import Yaifl.Std.Kinds.Supporter
import Yaifl.Std.Kinds.Backdrop
import Yaifl.Core.WorldModel
import Yaifl.Core.Query.Enclosing
import Yaifl.Core.Rules.RuleEffects

data ObjectSpecifics =
  NoSpecifics
  | EnclosingSpecifics Enclosing
  | ContainerSpecifics Container
  | OpenabilitySpecifics Openability
  | DoorSpecifics Door
  | DeviceSpecifics Device
  | PersonSpecifics Person
  | SupporterSpecifics Supporter
  | BackdropSpecifics Backdrop
  deriving stock (Eq, Show, Read, Generic)

makePrisms ''ObjectSpecifics

instance Pointed ObjectSpecifics where
  identityElement = NoSpecifics

class WMHasObjSpecifics (wm :: WorldModel) where
  inj :: Proxy wm -> ObjectSpecifics -> WMObjSpecifics wm

instance WMHasObjSpecifics ('WorldModel ObjectSpecifics a b c ac r se) where
  inj _ = id

instance MayHaveProperty ObjectSpecifics Enclosing where
  propertyAT = _EnclosingSpecifics
    `thenATraverse` (_PersonSpecifics % #carrying)
    `thenATraverse` (_ContainerSpecifics % #enclosing)
    `thenATraverse` (_SupporterSpecifics % #enclosing)


instance MayHaveProperty ObjectSpecifics MultiLocated where
  propertyAT = _DoorSpecifics % #multiLocated
    `thenATraverse` (_BackdropSpecifics % coerced @_ @MultiLocated)

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

instance MayHaveProperty ObjectSpecifics Backdrop where
  propertyAT = castOptic _BackdropSpecifics

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
  let tagged = tagEntity @Door @DoorTag ds d
  addDoorToConnection tagged f b
  pure (tagEntity ds d)

addBackdrop ::
  forall wm es.
  WMHasObjSpecifics wm
  => WMWithProperty wm Enclosing
  => WMWithProperty wm MultiLocated
  => RuleEffects wm es
  => AddObjects wm es
  => WMText wm -- ^ name
  -> "initialAppearance" :? WMText wm
  -> "description" :? WMText wm -- ^ Description.
  -> "described" :? ThingDescribed
  -> "modify" :? Eff '[State (Thing wm)] () -- ^ Build your own thing monad!
  -> "locations" :! (NonEmpty EnclosingEntity)
  -> Eff es ThingEntity
addBackdrop n ia des (argDef #described Described -> desc) (argDef #modify pass -> upD) (argF #locations -> Identity (l:|ls)) = do
  d <- addThing @wm n ia des
    ! #specifics (inj (Proxy @wm) $ BackdropSpecifics (Backdrop (MultiLocated (S.fromList $ l:ls))))
    ! #modify (do
      upD
      -- A backdrop is usually scenery.
      makeItScenery
      #objectData % #portable .= FixedInPlace
      #objectData % #pushableBetweenRooms .= False
      #objectData % #described .= desc)
    ! #type (ObjectKind "backdrop")
    ! #location l
    ! done
  updateMultiLocatedObject d
  pure d

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
      obj <- getEnclosingObject x
      let enc = getEnclosing obj
      updateToContain (getTaggedObject obj) enc t) (S.toList $ ml ^. #locations)

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
  -> "modify" :? Eff '[State (Thing wm)] ()
  -> Eff es ContainerEntity
addContainer n ia d
  (argF #carryingCapacity -> cc)
  (argF #opacity -> op)
  (argF #enterable -> e)
  (argF #openable -> o)
  (argF #opened -> od)
  (argF #location -> l)
  (argF #portable -> p)
  (argF #modify -> m) = do
    let cs = makeContainer cc op e o od
    c <- addThing @wm n ia d
        ! #specifics (inj (Proxy @wm) $ ContainerSpecifics cs)
        ! #type (ObjectKind "container")
        ! paramF #location l
        ! paramF #portable p
        ! paramF #modify m
        ! done
    pure $ tagEntity @Container @ContainerTag cs c

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
  -> "modify" :? Eff '[State (Thing wm)] ()
  -> Eff es SupporterEntity
addSupporter n ia d
  (argF #carryingCapacity -> cc) (argF #location -> l) (argF #enterable -> e) (argF #modify -> m) = do
    let enc = (blankEnclosing { capacity = cc <|> Just 100 })
        sup = Supporter enc (fromMaybe NotEnterable e)
    c <- addThing @wm n ia d
        ! #specifics (inj (Proxy @wm) $ SupporterSpecifics sup)
        ! #type (ObjectKind "supporter")
        ! paramF #location l
        ! paramF #modify m
        ! done
    pure $ tagEntity @_ @SupporterTag sup c

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
  -> "modify" :? Eff '[State (Thing wm)] ()
  -> Eff es ThingEntity
addPerson n (Arg g) ia d (argF #carrying -> e) (argF #modify -> m) = addThing @wm n ia d
  ! #specifics (inj (Proxy @wm) (PersonSpecifics (Person g (fromMaybe defaultPersonEnclosing e))))
  ! #type (case g of
    Male -> ObjectKind "man"
    Female -> ObjectKind "woman"
    NonBinary -> ObjectKind "person"
    Other _ -> ObjectKind "person")
  ! paramF #modify m
  ! done

addBaseObjects ::
  WMWithProperty wm Enclosing
  => WMHasObjSpecifics wm
  => AddObjects wm es
  => Eff es ()
addBaseObjects = do
  v <- addRoom "The Void" ! #description "If you're seeing this, you did something wrong." ! done
  addPerson "yourself" ! #description "It's you, looking handsome as always" !
    #gender NonBinary !
    #modify (do
      #objectData % #described .= Undescribed
      #nameProperness .= Proper) ! done
  #firstRoom .= v