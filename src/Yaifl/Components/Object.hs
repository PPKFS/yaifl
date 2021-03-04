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

data Description = PlainDescription Text | DynamicDescription (forall w. w -> Entity -> Text)

instance IsString Description where
    fromString = PlainDescription . fromString

-- | also for overloadedstrings
instance Semigroup Description where
    (<>) (PlainDescription e) (PlainDescription e2) =
        PlainDescription (e <> e2)
    (<>) (PlainDescription e) (DynamicDescription e2) =
        DynamicDescription (\w e1 -> e <> e2 w e1)
    (<>) (DynamicDescription e2) (PlainDescription e) =
        DynamicDescription (\w e1 -> e2 w e1 <> e)
    (<>) (DynamicDescription e) (DynamicDescription e2) =
        DynamicDescription (\w e1 -> e w e1 <> e2 w e1)
-- | we define a show instance just for debug purposes.
instance Show Description where
    show (PlainDescription   t) = show t
    show (DynamicDescription _) = "dynamic description"

data Object = Object
    { _name        :: Name
    , _description :: Description
    , _objID    :: Entity
    , _objType  :: Text
    }
    deriving Show

blankObject :: Entity -> ObjType -> Object
blankObject = Object "" ""
makeClassy ''Object

class HasDescription w m a where
    evalDescription :: a -> GameData w m -> Text

instance (HasStore w Object) => HasDescription w m Entity where
    evalDescription e gd = maybe "Uh oh." (`evalDescription` gd) v where
        v :: Maybe Object
        v = gd ^. gameWorld . store . at e

instance HasObject i => HasDescription w m i where
    evalDescription o = ed (o ^. object) where
        ed Object{_description=PlainDescription t} _ = t
        ed Object{_description=DynamicDescription f, _objID=i} g = f g i

thereIs :: (ThereIs s, HasStore w s, WithGameLog w m, HasObject s) => State s a -> World w m s
thereIs s = do
    e <- newEntity
    let v = execState s $ defaultObject e
    gameWorld . store . at e ?= v
    logDebug $ "Made a new object at ID " <> show e <> " named " <> _name (v ^. object)
    return v
    
showObjDebug :: HasObject s => s -> Text
showObjDebug s = "(" <> s ^. object . name <> ", ID: " <> show (s ^. object . objID) <> ", type: " <> s ^. object . objType <> ")"

showMaybeObjDebug :: HasObject s => Maybe s -> Text
showMaybeObjDebug = maybe "(No object)" showObjDebug

isType :: (HasStore w Object, Monad m) => Entity -> Text -> World w m Bool 
isType e t = do
    o <- getComponent @Object e
    return $ fmap _objType o == Just t
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