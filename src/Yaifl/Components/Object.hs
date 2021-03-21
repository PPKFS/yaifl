module Yaifl.Components.Object
    ( Name
    , Description(..)
    , Object(..)
    , name
    , description
    , objID
    , objType
    , HasObject
    , object
    , ObjType
    , blankObject
    , thereIs
    , evalDescription
    , HasDescription
    , showObjDebug
    , showMaybeObjDebug
    , isType
    , getDescription
    , evalDescription'
    , HasObjectStore
    , getName
    , ThingLit(..)
    , Edibility(..)
    , Portability(..)
    , Wearability(..)
    , Pushability(..)
    , Describable(..)
    , Physical(..)
    , Thing
    , thing
    , things
    , described
    , HasThing
    , move
    , getThing
    , getLocation
    , HasPhysical
    , HasPhysicalStore
    , HasThingStore
    , thingPhysical
    , isEnclosedBy
    , isConcealed
    , lit
    , markedForListing
    , isWorn
    , isLit
    , ThereIsThingConstraints
    )
where

import qualified Text.Show
import Yaifl.Prelude
import Yaifl.Common
import Yaifl.Components.Enclosing
import qualified Data.Set as DS
import Yaifl.Utils
import Colog hiding (Lens')

-- the printed name of something
type Name = Text
type ObjType = Text

data Description w = PlainDescription Text | DynamicDescription (Entity -> GameData w -> Text)

instance IsString (Description w) where
    fromString = PlainDescription . fromString

-- | also for overloadedstrings
instance Semigroup (Description w) where
    (<>) (PlainDescription e) (PlainDescription e2) =
        PlainDescription (e <> e2)
    (<>) (PlainDescription e) (DynamicDescription e2) =
        DynamicDescription (\e1 -> do
            e' <- e2 e1
            return $ e <> e')
    (<>) (DynamicDescription e2) (PlainDescription e) =
        DynamicDescription (\e1 -> do
            e' <- e2 e1
            return $ e' <> e)
    (<>) (DynamicDescription e) (DynamicDescription e2) =
        DynamicDescription (\e1 -> do
            e' <- e2 e1
            e'' <- e e1
            return $ e'' <> e')
-- | we define a show instance just for debug purposes.
instance Show (Description w) where
    show (PlainDescription   t) = show t
    show (DynamicDescription _) = "dynamic description"

data Object w = Object
    { _name        :: Name
    , _description :: Description w
    , _objID    :: Entity
    , _objType  :: Text
    }
    deriving Show

data ThingLit = Lit | Unlit deriving (Eq, Show)
data Edibility = Edible | Inedible deriving (Eq, Show)
data Portability = FixedInPlace | Portable deriving (Eq, Show)
data Wearability = Wearable | Unwearable deriving (Eq, Show)
data Pushability = PushableBetweenRooms | NotPushableBetweenRooms deriving (Eq, Show)
data Describable = Described | NotDescribed deriving (Eq, Show)
data Physical w = Physical
    { _lit               :: ThingLit
    , _edible            :: Edibility
    , _portable          :: Portability
    , _wearable          :: Wearability
    , _pushable          :: Pushability
    , _enclosedBy        :: Entity
    , _markedForListing  :: Bool
    , _wornBy            :: Maybe Entity
    , _concealedBy       :: Maybe Entity
    , _described         :: Describable
    , _handled           :: Bool
    , _initialAppearance :: Maybe (Description w)
    , _scenery           :: Bool
    } deriving Show

data Thing w = Thing
    {
        _thingObject :: Object w
      , _thingPhysical :: Physical w
    } deriving Show

makeLenses ''Thing
makeClassy ''Physical

blankObject :: Entity -> ObjType -> Object w
blankObject = Object "" ""
makeClassy ''Object

type HasObjectStore w = HasStore w (Object w)
class HasDescription a m where
    evalDescription :: a -> m Text

evalDescription' :: MonadState (GameData w) m => Entity -> Description w -> m Text
evalDescription' _ (PlainDescription t) = return t
evalDescription' e (DynamicDescription f) = f e <$> get

instance (MonadState (GameData w) m, HasStore w (Object w)) => HasDescription Entity m where
    evalDescription e = do
        v <- getComponent @(Object w) e
        maybe (return "Uh oh.") evalDescription v

instance MonadState (GameData w) m => HasDescription (Object w) m where
    evalDescription e = evalDescription' (_objID e) (_description e)

getPhysical :: forall w m. (WithGameData w m, HasStore w (Physical w)) => Entity -> m (Maybe (Physical w))
getPhysical = getComponent @(Physical w)
thereIs :: (ThereIs s m, HasStore w (Physical w), HasStore w Enclosing, HasStore w (Object w), HasStore w s, WithGameData w m, HasObject s w) => State s a -> m s
thereIs s = do
    e <- newEntity
    defaultObj <- defaultObject e
    gameWorld . store . at e ?= defaultObj
    phys <- getPhysical e -- @(Physical w) e
    whenJust phys (\_ -> do
        rm <- use mostRecentRoom
        whenJust rm (\x -> do { move e x; pass })
        pass)
    let v = execState s defaultObj
    gameWorld . store . at e ?= v
    logDebug $ "Made a new object at ID " <> show e <> " named " <> _name (v ^. object)
    whenM (e `isType` "room") (mostRecentRoom ?= e)
    return v

showObjDebug :: HasObject s w => s -> Text
showObjDebug s = "(" <> s ^. object . name <> ", ID: " <> show (s ^. object . objID) <> ", type: " <> s ^. object . objType <> ")"

showMaybeObjDebug :: HasObject s w => Maybe s -> Text
showMaybeObjDebug = maybe "(No object)" showObjDebug

isType :: forall w m. (MonadState (GameData w) m, HasObjectStore w) => Entity -> Text -> m Bool
isType e t = do
    o <- getComponent @(Object w) e
    return $ fmap _objType o == Just t

getDescription :: forall w m. (HasObjectStore w, WithGameData w m) => Entity -> m (Maybe (Description w))
getDescription e = do
    o <- getComponent @(Object w) e
    return (_description <$> o)

class HasName w m a where
    getName :: (HasStore w (Object w)) => a -> m Name

instance MonadState (GameData w) m => HasName w m Entity where
    getName e = do
        o <- getComponent @(Object w) e
        return $ maybe "(no object)" _name o

isConcealed :: forall w m. (WithGameData w m, HasStore w (Physical w)) => Entity -> m Bool
isConcealed = isX @(Physical w) Nothing _concealedBy

type HasPhysicalStore w = (HasStore w (Object w), HasStore w (Physical w))
type HasThingStore w = HasStore w (Thing w)
blankPhysical :: Entity -> Physical w
blankPhysical e = Physical
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
                           False
                           Nothing
                           False



instance HasObject (Thing w) w where
    object = thingObject

instance HasPhysical (Thing w) w where
    physical = thingPhysical

type ThereIsThingConstraints w m = (HasThing w, HasStore w Enclosing, Monad m, MonadReader (Env (World w)) m, MonadState (GameData w) m, HasLog (Env (World w)) Message m, MonadWorld w m)

instance ThereIsThingConstraints w m => ThereIs (Thing w) m where
    defaultObject e = return $ Thing (blankObject e "thing") (blankPhysical defaultVoidRoom)

instance MonadState (GameData w) m => HasDescription (Thing w) m where
    evalDescription (Thing o _) = evalDescription o

instance HasThing w => HasStore w (Thing w) where
    store = things
type HasThing w = (HasObjectStore w , HasPhysicalStore w)

things :: HasThing w => Lens' w (Store (Thing w))
things = storeLens2 Thing _thingObject _thingPhysical

thing :: HasThing w => Entity -> Lens' w (Maybe (Thing w))
thing k = things . at k

getThing :: (WithGameData w m, HasThing w) => Entity -> m (Maybe (Thing w))
getThing o = use $ gameWorld . thing o

getLocation :: forall w m. (HasPhysicalStore w, WithGameData w m) => Entity -> m (Maybe Entity)
getLocation e = do
    o <- getComponent @(Physical w) e
    return (_enclosedBy <$> o)

move :: forall w m . (HasThing w, HasStore w Enclosing, WithGameData w m) => Entity -> Entity -> m Bool
move obj le = do
    objToMove <- getThing obj
    mloc <- getComponent @Enclosing le -- use $ gameWorld . (store @w @Enclosing) . at le
    locName <- getComponent @(Object w) le
    doIfExists2 objToMove mloc (showMaybeObjDebug objToMove <> " has no physical component, so cannot be moved.")
        (showMaybeObjDebug locName <> " has no enclosing component, so cannot move objects into it.")
        (\o _ -> do
            --todo: recalc the location?
            -- todo: doesn't this mean the location is actually
            -- a derived property?
            --o . location .= le
            let vl = o ^. thingPhysical . enclosedBy
            vlo <- getComponent @(Object w) vl
            logDebug $ "Moving " <> showObjDebug o <> " from " <> showMaybeObjDebug vlo <> " to " <> showMaybeObjDebug locName
            adjustComponent @(Physical w) obj (enclosedBy .~ le)
            adjustComponent @Enclosing vl (encloses %~ DS.delete obj)
            adjustComponent @Enclosing le (encloses %~ DS.insert obj)
            return True
        )

-- | either it's directly enclosed by the thing, or an upper level is
isEnclosedBy :: (HasStore w (Object w), HasStore w (Physical w), WithGameData w m) => Entity -> Entity -> m Bool
isEnclosedBy obj encloser = do
    directly <- getLocation obj
    maybe (return False) (\x -> if x == encloser then return True else x `isEnclosedBy` encloser) directly

isWorn :: forall w m. (HasThing w, WithGameData w m) => Entity -> m Bool
isWorn e = do
    phys <- getComponent @(Physical w) e
    maybeM False (return . isJust . _wornBy) phys

isLit :: forall w m. (HasThing w, WithGameData w m) => Entity -> m Bool
isLit e = getComponent @(Physical w) e >>= (\x -> return $ fmap _lit x == Just Lit)


{-
mapObjects :: HasWorld w '[c] r => Proxy c -> (c -> Sem r c) -> Sem r ()
mapObjects c1 func = do
    g <- getStore c1
    mapM_ (\(k, v) -> do
        res <- func v
        setComponent c1 k res) $ IM.assocs g

mapObjects2 :: HasWorld w '[c1, c2] r => Proxy c1 -> Proxy c2 -> (c1 -> c2 -> Sem r (c1, c2)) -> Sem r ()
mapObjects2 c1 c2 func = do
    g <- getStore c1
    mapM_ (\(k, v) -> do
        comp2 <- getComponent c2 k
        whenJust comp2 (\co2 -> do
                        (r1, r2) <- func v co2
                        setComponent c1 k r1
                        setComponent c2 k r2)) $ IM.assocs g

mapObjects3 :: HasWorld w '[c1, c2, c3] r => Proxy c1 -> Proxy c2 -> Proxy c3 -> (c1 -> c2 -> c3 -> Sem r (c1, c2, c3)) -> Sem r ()
mapObjects3 c1 c2 c3 func = do
    g <- getStore c1
    mapM_ (\(k, v) -> do
        comp2 <- getComponent c2 k
        comp3 <- getComponent c3 k
        whenJust comp2 (\co2 -> whenJust comp3 
                    (\co3 -> do
                        (r1, r2, r3) <- func v co2 co3
                        setComponent c1 k r1
                        setComponent c2 k r2
                        setComponent c3 k r3))) $ IM.assocs g                        
                        -}