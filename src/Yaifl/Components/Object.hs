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
    , HasObjectStore
        {-
    , objectComponent
    , makeObject
    , HasWorld
    , HasComponents
    , nameOf
    , HasName
    , mapObjects
    , mapObjects2
    , mapObjects3-}
    )
where

import           Yaifl.Common
import           Yaifl.Prelude
import qualified Text.Show
import Colog

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

blankObject :: Entity -> ObjType -> Object w
blankObject = Object "" ""
makeClassy ''Object

type HasObjectStore w = HasStore w (Object w)
class HasDescription a m where
    evalDescription :: a -> m Text

instance (MonadState (GameData w) m, HasStore w (Object w)) => HasDescription Entity m where
    evalDescription e = do
        v <- getComponent @(Object w) e
        maybe (return "Uh oh.") evalDescription v

instance MonadState (GameData w) m => HasDescription (Object w) m where
    evalDescription e = ed e <$> get where
            ed Object{_description=PlainDescription t} = return t
            ed Object{_description=DynamicDescription f, _objID=i} = f i

thereIs :: (ThereIs s, HasStore w s, WithGameData w m, HasObject s w) => State s a -> m s
thereIs s = do
    e <- newEntity
    let v = execState s $ defaultObject e
    gameWorld . store . at e ?= v
    logDebug $ "Made a new object at ID " <> show e <> " named " <> _name (v ^. object)
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
{-
class HasName a where
    nameOf :: (HasWorld w '[Object] r) => a -> Sem r Name

instance HasName Entity where
    nameOf e = do
        _name <$> getComponent' objectComponent e
    


type HasWorld w c r
    = (Members (SemWorldList w) r, HasComponents w c)

type family HasComponents w (constraints :: [Type]) :: Constraint where
  HasComponents w '[] = (HasStore w Object)
  HasComponents w (x ': xs) = (HasStore w x, HasComponents w xs)

objectComponent :: Proxy Object
objectComponent = Proxy

makeObject :: (WithLogging r, HasWorld w '[Object] r) => Text -> Description -> Text -> Sem r Entity
makeObject n d t = do
    e <- newEntity
    addComponent e (Object n d e t)
    logMsg Info $ "Made a new " <> t <> " with ID " <> show e <> " called " <> n
    return e

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