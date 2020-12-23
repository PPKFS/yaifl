module Yaifl.Components.Physical
    ( ThingLit(..)
    , Edibility(..)
    , Portability(..)
    , Wearability(..)
    , Pushability(..)
    , Describable(..)
    , Physical(..)
    , defaultPhysical
    , physicalComponent
    , makeThing
    , makeThing'
    , described
    )
where

import Yaifl.Prelude
import Yaifl.Common
import Yaifl.Say
import Yaifl.Components.Object

data ThingLit = Lit | Unlit deriving (Eq, Show)
data Edibility = Edible | Inedible deriving (Eq, Show)
data Portability = FixedInPlace | Portable deriving (Eq, Show)
data Wearability = Wearable | Unwearable deriving (Eq, Show)
data Pushability = PushableBetweenRooms | NotPushableBetweenRooms deriving (Eq, Show)
data Describable = Described | NotDescribed deriving (Eq, Show)
data Physical = Physical
    { _location          :: Entity
    , _lit               :: ThingLit
    , _edible            :: Edibility
    , _portable          :: Portability
    , _wearable          :: Wearability
    , _pushable          :: Pushability
    , _enclosedBy        :: Entity
    , _mentioned         :: Bool
    , _markedForListing  :: Bool
    , _wornBy            :: Maybe Entity
    , _concealedBy       :: Maybe Entity
    , _described         :: Describable
    , _handled           :: Bool
    , _initialAppearance :: Maybe Description
    , _scenery           :: Bool
    }
    deriving Show
makeLenses ''Physical

physicalComponent :: Proxy Physical
physicalComponent = Proxy

isConcealed :: HasWorld w '[Physical] r => Entity -> Sem r Bool
isConcealed e = not <$> isX Nothing _concealedBy physicalComponent e

defaultPhysical :: Entity -> Physical
defaultPhysical e = Physical e
                           Lit
                           Inedible
                           Portable
                           Wearable
                           PushableBetweenRooms
                           e
                           False
                           False
                           Nothing
                           Nothing
                           Described
                           False
                           Nothing
                           False

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

{-
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