module Yaifl.Components.Container (
    Opacity (..),
    ContainerData (..),
    ContainerObject (..),
    isOpaqueClosedContainer,
    HasContainer,
    containerEnclosing,
    containerEnterable,
    containerOpenable,
    deleteContainer,
) where

import qualified Data.Set as DS
import Yaifl.Common
import Yaifl.Components.Enclosing
import Yaifl.Components.Enterable
import Yaifl.Components.Object
import Yaifl.Components.Openable
import Yaifl.Prelude

data Opacity = Opaque | Transparent deriving (Eq, Show)

newtype ContainerData = ContainerData
    { _opacity :: Opacity
    }
    deriving (Eq, Show)

data ContainerObject w = ContainerObject
    { _containerThing :: Thing w
    , _containerObjData :: ContainerData
    , _containerEnclosing :: Enclosing
    , _containerOpenable :: Openable
    , _containerEnterable :: Enterable
    }
    deriving Show

makeClassy ''ContainerData
makeLenses ''ContainerObject

instance HasObject (ContainerObject w) w where
    object = containerThing . object

instance HasContainer w => HasStore w (ContainerObject w) where
    store = containers

instance Monad m => ThereIs (ContainerObject w) m where
    defaultObject e = return $ ContainerObject (blankThingBase e "container") (ContainerData Opaque) (Enclosing DS.empty Nothing) Closed NotEnterable

instance HasContainer w => Deletable w (ContainerObject w) where
    deleteObject e = do
        deleteComponent @(Thing w) e
        deleteComponent @ContainerData e
        deleteComponent @Enclosing e
        deleteComponent @Openable e
        deleteComponent @Enterable e
        pass

type HasContainer w = (HasThingStore w, HasStore w ContainerData, HasStore w Enclosing, HasStore w Openable, HasStore w Enterable)
deleteContainer :: forall w m. (WithGameData w m, HasContainer w) => Entity -> m ()
deleteContainer = deleteObject @w @(ContainerObject w)

containers :: HasContainer w => Lens' w (Store (ContainerObject w))
containers = storeLens5 ContainerObject _containerThing _containerObjData _containerEnclosing _containerOpenable _containerEnterable

container :: HasContainer w => Entity -> Lens' w (Maybe (ContainerObject w))
container k = containers . at k

isOpaqueClosedContainer :: (WithGameData w m, HasContainer w) => Entity -> m Bool
isOpaqueClosedContainer e = do
    c <- use $ gameWorld . container e
    return $ c ^? _Just . containerObjData . opacity == Just Opaque && fmap _containerOpenable c == Just Closed
