module Yaifl.Components.Object
    ( Name
    , Description(..)
    , Object(..)
    , descriptionOf
    , HasDescription
    , objectComponent
    , makeObject
    , HasWorld
    , HasComponents
    , nameOf
    , HasName
    , mapObjects
    , mapObjects2
    , mapObjects3
    )
where

import           Yaifl.Common
import           Yaifl.Say
import           Yaifl.Prelude
import qualified Text.Show
import Yaifl.PolysemyOptics
import Polysemy.State
import qualified Data.IntMap.Strict as IM

-- the printed name of something
type Name = Text

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
    , _objectID    :: Entity
    , _objectType  :: Text
    }
    deriving Show
makeLenses ''Object

class HasName a where
    nameOf :: (HasWorld w '[Object] r) => a -> Sem r Name

instance HasName Entity where
    nameOf e = do
        _name <$> getComponent' objectComponent e
    
class HasDescription a where
    descriptionOf :: (HasWorld w '[Object] r) => a -> Sem r Description

instance HasDescription Entity where
    descriptionOf e = do
        _description <$> getComponent' objectComponent e

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
                        
