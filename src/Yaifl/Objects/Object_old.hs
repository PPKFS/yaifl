module Yaifl.Components.Object (
    Name,
    Description (..),
    Object (..),
    ThingLit (..),
    Edibility (..),
    Portability (..),
    Wearability (..),
    Pushability (..),
    Describable (..),
    Physical (..),
    Handled (..),
    Thing (..),

    name,
    description,
    objID,
    objType,
    HasObject,
    object,
    ObjType,
    thereIs,
    evalDescription,
    HasDescription,
    showObjDebug,
    showMaybeObjDebug,
    isType,
    getDescription,
    evalDescription',
    HasObjectStore,
    getName,
    blankThingBase,
    ThereIs,
    defaultObject,
    thereIs',

    thing,
    things,
    described,
    HasThing,
    move,

    getThing,
    getPhysical,
    getPhysical',
    getObject,
    getObject',

    getLocation,
    HasPhysical,
    HasPhysicalStore,
    HasThingStore,
    thingPhysical,
    isEnclosedBy,
    isConcealed,
    lit,
    markedForListing,
    isWorn,
    isLit,
    ThereIsThingConstraints,
    thingObject,
    isDescribed,
) where

import Colog hiding (Lens')
import qualified Data.Set as DS
import qualified Text.Show
import Yaifl.Common
import Yaifl.Components.Enclosing
import Yaifl.Prelude
import Yaifl.Utils

-- | The printed name of something.
type Name = Text

{- | Object types follow a hierarchy which only holds superficial information
 i.e. there's no real subtyping, more just a DAG of strings.
-}
type ObjType = Text

-- | The description of something.
data Description w
    = -- | A static description.
      PlainDescription Text
    | -- | A description that is a function over the world.
      DynamicDescription (Entity -> GameData w -> Text)

data Object w = Object
    { _name :: Name
    , _description :: Description w
    , _objID :: Entity
    , _objType :: ObjType
    }
    deriving (Show)

data ThingLit = Lit | Unlit deriving (Eq, Show)
data Edibility = Edible | Inedible deriving (Eq, Show)
data Portability = FixedInPlace | Portable deriving (Eq, Show)
data Wearability = Wearable | Unwearable deriving (Eq, Show)
data Pushability = PushableBetweenRooms | NotPushableBetweenRooms deriving (Eq, Show)
data Describable = Described | NotDescribed deriving (Eq, Show)
data Handled = Handled | NotHandled deriving (Eq, Show)
data Physical w = Physical
    { _lit :: ThingLit
    , _edible :: Edibility
    , _portable :: Portability
    , _wearable :: Wearability
    , _pushable :: Pushability
    , _enclosedBy :: Entity
    , _markedForListing :: Bool
    , _wornBy :: Maybe Entity
    , _concealedBy :: Maybe Entity
    , _described :: Describable
    , _handled :: Handled
    , _initialAppearance :: Maybe (Description w)
    , _isScenery :: Bool
    }
    deriving (Show)

data Thing w = Thing
    { _thingObject :: Object w
    , _thingPhysical :: Physical w
    }
    deriving (Show)

-- some neater HasStore types

type HasObjectStore w = HasStore w (Object w)
type HasPhysicalStore w = (HasStore w (Object w), HasStore w (Physical w))
type HasThingStore w = HasStore w (Thing w)
type HasThing w = (HasObjectStore w, HasPhysicalStore w)

class HasDescription a m where
    evalDescription :: a -> m Text

class Monad m => ThereIs w t m where
    defaultObject :: Name -> Description w -> Entity -> m t

instance (MonadState (GameData w) m, HasStore w (Object w)) => HasDescription Entity m where
    evalDescription e = do
        v <- getComponent @(Object w) e
        maybe (return "Uh oh.") evalDescription v

instance MonadState (GameData w) m => HasDescription (Object w) m where
    evalDescription e = evalDescription' (_objID e) (_description e)

evalDescription' :: MonadState (GameData w) m => Entity -> Description w -> m Text
evalDescription' _ (PlainDescription t) = return t
evalDescription' e (DynamicDescription f) = f e <$> get


dynamicDescription ::Monad m0 => (t00 -> m0 ()) -> Description w
dynamicDescription = error "not implemented"

-- | For OverloadedStrings
instance IsString (Description w) where
    fromString = PlainDescription . fromString

instance Semigroup (Description w) where
    (<>) (PlainDescription e) (PlainDescription e2) =
        PlainDescription (e <> e2)
    (<>) (PlainDescription e) (DynamicDescription e2) =
        DynamicDescription
            ( \e1 -> do
                e' <- e2 e1
                return $ e <> e'
            )
    (<>) (DynamicDescription e2) (PlainDescription e) =
        DynamicDescription
            ( \e1 -> do
                e' <- e2 e1
                return $ e' <> e
            )
    (<>) (DynamicDescription e) (DynamicDescription e2) =
        DynamicDescription
            ( \e1 -> do
                e' <- e2 e1
                e'' <- e e1
                return $ e'' <> e'
            )

-- | we define a show instance just for debug purposes.
instance Show (Description w) where
    show (PlainDescription t) = show t
    show (DynamicDescription _) = "dynamic description"

makeLenses ''Thing
makeClassy ''Physical
makeClassy ''Object

instance HasObject (Thing w) w where
    object = thingObject

instance MonadState (GameData w) m => HasID w (Thing w) m where
    getID e = return $ e ^. thingObject . objID

instance HasPhysical (Thing w) w where
    physical = thingPhysical

type ThereIsThingConstraints w m = (HasThing w, HasStore w Enclosing, Monad m, MonadReader (Env (World w)) m, MonadState (GameData w) m, HasLog (Env (World w)) Message m, MonadWorld w m)

instance ThereIsThingConstraints w m => ThereIs w (Thing w) m where
    defaultObject n d e = return $ blankThingBase n d e "thing"

instance MonadState (GameData w) m => HasDescription (Thing w) m where
    evalDescription (Thing o _) = evalDescription o

instance HasThing w => HasStore w (Thing w) where
    store = things

blankPhysical :: Entity -> Physical w
blankPhysical e =
    Physical
        Unlit
        Inedible
        Portable
        Unwearable
        PushableBetweenRooms
        e
        False
        Nothing
        Nothing
        Described
        NotHandled
        Nothing
        False

blankThingBase :: Name -> Description w -> Entity -> ObjType -> Thing w
blankThingBase n d e t = Thing (Object n d e t) (blankPhysical defaultVoidRoom)

thereIs' :: forall s w m a. (ThereIs w s m, HasPhysicalStore w, HasStore w Enclosing, HasStore w s, WithGameData w m, HasObject s w) => Name -> Description w -> m s
thereIs' = flip flip pass . thereIs
thereIs :: forall s w m a. (ThereIs w s m, HasPhysicalStore w, HasStore w Enclosing, HasStore w s, WithGameData w m, HasObject s w) => Name -> Description w -> State s a -> m s
thereIs n d s = do
    e <- newEntity
    defaultObj <- defaultObject n d e
    gameWorld . store . at e ?= defaultObj
    phys <- getPhysical e -- @(Physical w) e
    whenJust
        phys
        (const $ whenJustM (use mostRecentRoom) (\x -> do move e x; pass))
    let v = execState s defaultObj
    gameWorld . store . at e ?= v
    logDebug $ "Made a new object at ID " <> show e <> " named " <> _name (v ^. object)
    whenM (e `isType` "room") (mostRecentRoom ?= e)
    return v

showObjDebug :: HasObject s w => s -> Text
showObjDebug s = "(" <> s ^. object . name <> ", ID: " <> show (s ^. object . objID) <> ", type: " <> s ^. object . objType <> ")"

showMaybeObjDebug :: HasObject s w => Maybe s -> Text
showMaybeObjDebug = maybe "(No object)" showObjDebug

-- helper functions for looking stuff up --


getPhysical :: forall w m. (WithGameData w m, HasStore w (Physical w)) => Entity -> m (Maybe (Physical w))
getPhysical = getComponent @(Physical w)

getPhysical' :: forall w m. (WithGameData w m, HasStore w (Physical w)) => Entity -> m (Physical w)
getPhysical' = getComponent' @(Physical w)

getObject :: forall w m. (WithGameData w m, HasStore w (Object w)) => Entity -> m (Maybe (Object w))
getObject = getComponent @(Object w)

getObject' :: forall w m. (WithGameData w m, HasStore w (Object w)) => Entity -> m (Object w)
getObject' = getComponent' @(Object w)

getThing :: forall w m. (WithGameData w m, HasStore w (Thing w)) => Entity -> m (Maybe (Thing w))
getThing = getComponent @(Thing w)

getDescription :: forall w m. (HasObjectStore w, WithGameData w m) => Entity -> m (Maybe (Description w))
getDescription e = _description <<$>> getObject e

getDescription' :: forall w m. (HasObjectStore w, WithGameData w m) => Entity -> m (Description w)
getDescription' e = _description <$> getObject' e

getName :: forall w m. (HasStore w (Object w), WithGameData w m) => Entity -> m (Maybe Name)
getName e = _name <<$>> getObject e

getName' :: forall w m. (HasStore w (Object w), WithGameData w m) => Entity -> m Name
getName' e = _name <$> getObject' e

getLocation :: forall w m. (HasPhysicalStore w, WithGameData w m) => Entity -> m (Maybe Entity)
getLocation e = _enclosedBy <<$>> getPhysical e

getLocation' :: forall w m. (HasPhysicalStore w, WithGameData w m) => Entity -> m Entity
getLocation' e = _enclosedBy <$> getPhysical' e

isConcealed :: forall w m. (WithGameData w m, HasStore w (Physical w)) => Entity -> m Bool
isConcealed = isX @(Physical w) Nothing _concealedBy

isDescribed :: forall w m. (WithGameData w m, HasStore w (Physical w)) => Entity -> m Bool
isDescribed = isX @(Physical w) Described _described

isWorn :: forall w m. (HasThing w, WithGameData w m) => Entity -> m Bool
isWorn e = maybe False (isJust . _wornBy) <$> getComponent @(Physical w) e

isLit :: forall w m. (HasThing w, WithGameData w m) => Entity -> m Bool
isLit = isX @(Physical w) Lit _lit

-- | either it's directly enclosed by the thing, or an upper level is
isEnclosedBy :: (HasStore w (Object w), HasStore w (Physical w), WithGameData w m) => Entity -> Entity -> m Bool
isEnclosedBy obj encloser = do
    v <- getLocation obj
    maybe (return False) (\x -> if x == encloser then return True else x `isEnclosedBy` encloser) v 

-- TODO: redo this
isType :: forall w m. (MonadState (GameData w) m, HasObjectStore w) => Entity -> Text -> m Bool
isType e t = do
    o <- getComponent @(Object w) e
    return $ fmap _objType o == Just t

things :: HasThing w => Lens' w (Store (Thing w))
things = storeLens2 Thing _thingObject _thingPhysical

thing :: HasThing w => Entity -> Lens' w (Maybe (Thing w))
thing k = things . at k

move :: forall w m. (HasThing w, HasStore w Enclosing, WithGameData w m) => Entity -> Entity -> m Bool
move obj le = do
    objToMove <- getThing obj
    mloc <- getComponent @Enclosing le -- use $ gameWorld . (store @w @Enclosing) . at le
    locName <- getComponent @(Object w) le
    doIfExists2
        objToMove
        mloc
        (showMaybeObjDebug objToMove <> " has no physical component, so cannot be moved.")
        (showMaybeObjDebug locName <> " has no enclosing component, so cannot move objects into it.")
        ( \o _ -> do
            --todo: recalc the location?
            -- todo: doesn't this mean the location is actually
            -- a derived property?
            let vl = o ^. thingPhysical . enclosedBy
            vlo <- getComponent @(Object w) vl
            logDebug $ "Moving " <> showObjDebug o <> " from " <> showMaybeObjDebug vlo <> " to " <> showMaybeObjDebug locName
            adjustComponent @(Physical w) obj (enclosedBy .~ le)
            adjustComponent @Enclosing vl (encloses %~ DS.delete obj)
            adjustComponent @Enclosing le (encloses %~ DS.insert obj)
            return True
        )