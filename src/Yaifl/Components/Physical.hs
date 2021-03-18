module Yaifl.Components.Physical
    ( ThingLit(..)
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
    )
where

import Yaifl.Prelude
import Yaifl.Common
import Yaifl.Components.Object
import Yaifl.Components.Enclosing
import qualified Data.Set as DS
import Yaifl.Utils
import Colog hiding (Lens')

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
makeClassy ''Physical

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

data Thing w = Thing
    {
        _thingObject :: Object w
      , _thingPhysical :: Physical w
    } deriving Show

makeLenses ''Thing

instance HasObject (Thing w) w where
    object = thingObject

instance HasPhysical (Thing w) w where
    physical = thingPhysical
instance ThereIs (Thing w) where
    defaultObject e = Thing (blankObject e "thing") (blankPhysical defaultVoidRoom)

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
    indirectly <- maybeM False (obj `isEnclosedBy`) directly
    return $ directly == Just encloser || indirectly 
{-
makeThing' :: HasWorld w '[Physical] r => Name -> Description -> Entity -> Sem r Entity
makeThing' n d l = do
    addContext "ThingConstruction" Nothing
    e <- makeThing n d (defaultPhysical l) "thing"
    removeContext
    return e

makeThing :: HasWorld w '[Physical] r => Name -> Description -> Physical -> Text -> Sem r Entity
makeThing n d p t = do
    e <- makeObject n d t
    addComponent e p
    n2 <- nameOf (_enclosedBy p)
    logMsg Info ("Placed " <> n <> " in the " <> n2)
    return e



physicalLens :: HasComponent u w Physical => Entity -> Lens' u (Maybe Physical)
physicalLens = component physicalComponent

physicalLens' :: HasComponent u w Physical => Entity -> Lens' u Physical
physicalLens' = component' physicalComponent

--mentionedLens :: HasComponent u w Physical => Entity -> Lens' u (Maybe Bool)
--mentionedLens e = physicalLens e . _Just . mentioned

mentionedLens' :: HasComponent u w Physical => Entity -> Lens' u Bool
mentionedLens' e = physicalLens' e . mentioned
physicalComponent :: Proxy Physical
physicalComponent = Proxy



isWearable :: HasComponent u w Physical => u -> Entity -> Bool
isWearable = isX Wearable _wearable physicalComponent

isWorn :: HasComponent u w Physical => u -> Entity -> Bool
isWorn u e = not $ isX Nothing _wornBy physicalComponent u e

isLit :: HasComponent u w Physical => u -> Entity -> Bool
isLit = isX Lit _lit physicalComponent

isEdible :: HasComponent u w Physical => u -> Entity -> Bool
isEdible = isX Wearable _wearable physicalComponent
-}