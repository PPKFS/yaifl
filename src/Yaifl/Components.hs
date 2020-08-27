module Yaifl.Components
(
    HasComponent, addComponent,
    getComponent, getComponent',
    getDescription, getDescription',
    isComponent, isX,
    component, component',
    getPlayer', playerLocation',
    move,
    Object(..), objectComponent, --updateObjectPromise,
    getName,
    ThingLit(..),
    Physical, physicalComponent, _enclosedBy, mentioned, _mentioned, _markedForListing, markedForListing,
    isWearable, _lit, lit, wornBy, _wornBy, isWorn, isLit, described, Describable(..), _described, _handled,
    handled, _initialAppearance, initialAppearance, _scenery, scenery, _concealedBy, concealedBy,
    Enclosing, enclosingComponent, _encloses, 
    RoomData(..), roomComponent,
    Container(..), containerComponent, Opacity(..),
    Openable(..), openableComponent,
    Player, playerComponent,
    Supporter(..), supporterComponent,
    sceneryComponent,
    makeObject, makeRoom, makeRoom', makePlayer, makeThing,
    mapObjects, mapObjects2,
    HasStd, HasStd',
    defaultWorld
) where

import Relude
import Yaifl.Common
import Yaifl.Say
import Yaifl.Utils
import qualified Data.IntMap.Strict as IM
import qualified Data.Map as Map
import qualified Text.Show
import qualified Data.Set as DS
import qualified Language.Haskell.TH as TH
import Control.Lens

-- | A UNIVERSE u over a WORLD w has stores for a COMPONENT c
-- basically just a transitive typeclass constraint
type HasComponent u w c = (Has w Object, HasMessageBuffer u, HasWorld' w, HasWorld u w, Has w c)

globalComponent :: Int
globalComponent = 0

-- | update the corresponding store with a new component
addComponent :: HasComponent u w c => Int -> c -> System u ()
addComponent e c = world . store (Proxy :: Proxy a) . at e ?= c

-- | since an object component has reference to its entity, we can also update the promise if needed
-- again, this is more just for being smart with our partial function usage
--updateObjectPromise :: Atom.Symbol -> Int -> System u ()
--updateObjectPromise s e = zoom world (store (Proxy :: Proxy Object) . at e . _Just . objectID . entityType .= s)

setComponent :: HasComponent u w c => u -> Proxy c -> Entity -> c -> u
setComponent w p e v = w & world . store p . ix e .~ v

-- | TODO: see if I can make a version that throws an error
-- but for now, it's fine. just for consistency.
setComponent' :: HasComponent u w c => u -> Proxy c -> Entity -> c -> u
setComponent' = setComponent

getComponent :: (HasComponent u w c, HasID o) => u -> Proxy c -> o -> Maybe c
getComponent w t e = IM.lookup (objID e) $ w ^. world . store t

-- | if we're ~sure~ it exists but it's only user-invariant, we can use this to save time and maybes.
getComponent' :: (HasComponent u w c, HasID o) => u -> Proxy c -> o -> c
getComponent' w t e = fromMaybe (error "the component was missing") (getComponent w t e)

-- | a lens to /maybe/ a certain component from an entity
component :: HasComponent u w c => Proxy c -> Entity -> Lens' u (Maybe c)
component p e = lens (\w -> getComponent w p e) (\w b -> case b of
    Just mb -> setComponent w p e mb
    Nothing -> w)
-- | and an equivalent lens when we can promise that it exists
component' :: HasComponent u w c => Proxy c -> Entity -> Lens' u c
component' p e = lens (\w -> getComponent' w p e) (\w -> setComponent' w p e)

mapObjects :: HasComponent u w a => Proxy a -> (a -> System u a) -> System u ()
mapObjects c1 func = do
    g <- get
    let evs = IM.assocs (g ^. world . store c1)
    mapM_ (\(k, v) -> do
        res <- func v
        world . store c1 . at k ?= res) evs

mapObjects2 :: (HasComponent u w a, HasComponent u w b) => (Proxy a, Proxy b) -> (a -> b -> System u (a, b)) 
                    ->  System u ()
mapObjects2 (c1, c2) func = do
    g <- get
    let evs = IM.assocs (g ^. world . store c1)
    mapM_ (\(k, v) -> do
        g <- get
        let comp2 = getComponent g c2 k
        whenJust comp2 (\co2 -> do
                        (r1, r2) <- func v co2
                        world . store c1 . at k ?= r1
                        world . store c2 . at k ?= r2)) evs

isComponent :: HasComponent u w c => u -> Proxy c -> Entity -> Bool
isComponent u p e = IM.member e (u ^. world . store p)

isX :: (Eq b, HasComponent u w c) => b -> (c -> b) -> Proxy c -> u -> Entity -> Bool
isX p recordField comp w e = fmap recordField (getComponent w comp e) == Just p

-- | everything /should/ have an object component.
data Object = Object
    {
        _name :: Name,
        _description :: Description,
        _objectID :: Entity
    } deriving Show
makeLenses ''Object

instance HasID Object where
    objID = objID . _objectID

objectComponent :: Proxy Object
objectComponent = Proxy

getName :: HasComponent u w Object => u -> Entity -> Name
getName w e = _name $ getComponent' w objectComponent e 
-- | the 'correct' show

getDescription :: w -> Entity -> Description -> Text
getDescription w o d =  case d of
    PlainDescription t -> t
    DynamicDescription p -> p w o

getDescription' :: w -> Object -> Text
getDescription' w o = getDescription w (_objectID o) (_description o)

makeObject :: HasComponent u w Object => Text -> Description -> System u Entity
makeObject n d = do
    e <- newEntity
    addComponent e (Object n d e)
    sayDbgLn $ "made an object called " <> n <> " with id " <> show e
    return e

data ThingLit = Lit | Unlit deriving (Eq, Show)
data Edibility = Edible | Inedible deriving (Eq, Show)
data Portability = FixedInPlace | Portable deriving (Eq, Show)
data Wearability = Wearable | Unwearable deriving (Eq, Show)
data Pushability = PushableBetweenRooms | NotPushableBetweenRooms deriving (Eq, Show)
data Describable = Described | NotDescribed deriving (Eq, Show)
data Physical = Physical
    {
        _location :: Entity,
        _lit :: ThingLit,
        _edible :: Edibility,
        _portable :: Portability,
        _wearable :: Wearability,
        _pushable :: Pushability,
        _enclosedBy :: Entity,
        _mentioned :: Bool,
        _markedForListing :: Bool,
        _wornBy :: Maybe Entity,
        _concealedBy :: Maybe Entity,
        _described :: Describable,
        _handled :: Bool,
        _initialAppearance :: Maybe Description,
        _scenery :: Bool
    } deriving Show
makeLenses ''Physical

physicalComponent :: Proxy Physical
physicalComponent = Proxy

blankPhysical :: Entity -> Physical
blankPhysical e = Physical e Lit Edible Portable Wearable PushableBetweenRooms e False False Nothing Nothing Described False Nothing False

isWearable :: HasComponent u w Physical => u -> Entity -> Bool
isWearable = isX Wearable _wearable physicalComponent

isWorn :: HasComponent u w Physical => u -> Entity -> Bool
isWorn u e = not $ isX Nothing _wornBy physicalComponent u e

isLit :: HasComponent u w Physical => u -> Entity -> Bool
isLit = isX Lit _lit physicalComponent

isEdible :: HasComponent u w Physical => u -> Entity -> Bool
isEdible = isX Wearable _wearable physicalComponent

newtype Enclosing = Enclosing
    {
        _encloses :: Set Entity
    } deriving Show
makeLenses ''Enclosing

enclosingComponent :: Proxy Enclosing
enclosingComponent = Proxy

makeThing :: (HasComponent u w Enclosing, HasComponent u w Physical) => Text -> Description -> Entity -> System u Entity
makeThing n d loc = do
    e <- makeObject n d
    addComponent e (blankPhysical loc)
    component' enclosingComponent loc . encloses %= DS.insert e
    return e

data Darkness = Lighted | Dark deriving (Eq, Show)
data IsVisited = Visited | Unvisited deriving (Eq, Show)
type MapConnections = Store Entity
type ContainingRegion = Maybe Entity

data RoomData = RoomData
    {
        _isVisited :: IsVisited,
        _mapConnections :: MapConnections,
        _containingRegion :: ContainingRegion
    } deriving Show

roomComponent :: Proxy RoomData
roomComponent = Proxy :: (Proxy RoomData)

makeRoom :: (HasComponent u w Enclosing, HasComponent u w RoomData) => Text -> Description -> System u Entity
makeRoom n d = do
    e <- makeObject n d
    addComponent e (RoomData Unvisited emptyStore Nothing)
    addComponent e (Enclosing DS.empty)
    return e

makeRoom' :: (HasComponent u w Enclosing, HasComponent u w RoomData) => Text -> System u Entity
makeRoom' n = makeRoom n $ "It's " <> PlainDescription n <> "."

data Opacity = Opaque | Transparent deriving (Eq, Show)

data Container = Container
    {
        _enterable :: Bool,
        _opacity :: Opacity,
        _carryingCapacity :: Int
    } deriving Show
containerComponent :: Proxy Container
containerComponent = Proxy :: (Proxy Container)

data Supporter = Supporter
    {
        _supporterEnterable :: Bool,
        _supporterCarryingCapacity :: Int
    } deriving Show
supporterComponent :: Proxy Supporter
supporterComponent  = Proxy :: (Proxy Supporter)

data Openable = Open | Closed deriving (Eq, Show)
openableComponent :: Proxy Openable
openableComponent = Proxy :: (Proxy Openable)

newtype Player = Player Int deriving Show
playerComponent :: Proxy Player
playerComponent = Proxy :: Proxy Player

getPlayer' :: (HasComponent u w Player) => u -> Entity
getPlayer' w = maybe (error "missing player") coerce (w ^. world . store (Proxy :: Proxy Player) . at globalComponent)

playerLocation' :: (HasComponent u w Player, HasComponent u w Physical) => u -> Entity
playerLocation' u = _location $ getComponent' u physicalComponent $ getPlayer' u

makePlayer :: (HasComponent u w Enclosing, HasComponent u w Player, HasComponent u w Physical) => Entity -> System u Entity
makePlayer e' = do
    e <- makeThing "yourself" "it's you." e'
    component' physicalComponent e . described .= NotDescribed
    addComponent globalComponent (Player e)
    return e
-- TODO
sceneryComponent :: Proxy Openable
sceneryComponent = Proxy
move :: (HasComponent u w Physical, HasComponent u w Enclosing) => Entity -> Entity -> System u Bool
move obj le = do
    w <- get
    --mp is the thing we're moving
    let mp = getComponent w physicalComponent obj
    --mloc is the new place to put it
        mloc = getComponent w enclosingComponent le
    --mcurrloc is the existing location
        mcurrLoc = _location <$> mp
    doIfExists3 mp mloc mcurrLoc (show obj <> " no physical thing to move") "no future loc" "no current loc" 
        (\_ _ c -> do
            component' physicalComponent obj . location .= le
            component' physicalComponent obj . enclosedBy .= le
            component' enclosingComponent c . encloses %= DS.delete obj
            component' enclosingComponent le . encloses %= DS.insert obj
            return True
        )

defaultWorld :: [TH.Name]
defaultWorld = [''Object, ''RoomData, ''Physical, ''Enclosing, ''Player, ''Openable, ''Container, ''Supporter]

type HasStd u w = (HasComponent u w Object, HasComponent u w RoomData, HasComponent u w Physical, 
                    HasComponent u w Enclosing, HasComponent u w Player, HasComponent u w Openable, 
                    HasComponent u w Container, HasComponent u w Supporter)
type HasStd' w = HasStd w w