module Yaifl.Components.Container
    ( Opacity(..)
    , ContainerData(..)
    , ContainerObject(..)
    , isOpaqueClosedContainer
    , HasContainer
    , containerEnclosing
    , containerEnterable
    , containerOpenable
    , deleteContainer
    )
where

import Yaifl.Prelude
import Yaifl.Common
import Yaifl.Components.Enclosing
import Yaifl.Components.Object
import Yaifl.Components.Openable
import Yaifl.Components.Enterable
import qualified Data.Set as DS

data Opacity = Opaque | Transparent deriving (Eq, Show)

data ContainerData = ContainerData
    {
        _opacity :: Opacity
    } deriving (Eq, Show)

data ContainerObject w = ContainerObject
    {
        _containerObject :: Object w,
        _containerObjData :: ContainerData,
        _containerEnclosing :: Enclosing,
        _containerOpenable :: Openable,
        _containerEnterable :: Enterable
    } deriving Show

makeClassy ''ContainerData
makeLenses ''ContainerObject

instance HasObject (ContainerObject w) w where
    object = containerObject

instance HasContainer w => HasStore w (ContainerObject w) where
    store = containers

instance Monad m => ThereIs (ContainerObject w) m where
    defaultObject e = return $ ContainerObject (blankObject e "container") (ContainerData Opaque ) (Enclosing DS.empty Nothing) Closed NotEnterable

instance HasContainer w => Deletable w (ContainerObject w) where
    deleteObject e = do
        deleteComponent @(Object w) e
        deleteComponent @ContainerData e
        deleteComponent @Enclosing e
        deleteComponent @Openable e
        deleteComponent @Enterable e
        pass

type HasContainer w = (HasObjectStore w, HasStore w ContainerData, HasStore w Enclosing, HasStore w Openable, HasStore w Enterable)
deleteContainer :: forall w m. (WithGameData w m, HasContainer w) => Entity -> m ()
deleteContainer = deleteObject @w @(ContainerObject w)

containers :: HasContainer w => Lens' w (Store (ContainerObject w))
containers = storeLens5 ContainerObject _containerObject _containerObjData _containerEnclosing _containerOpenable _containerEnterable

container :: HasContainer w => Entity -> Lens' w (Maybe (ContainerObject w) )
container k = containers . at k

isOpaqueClosedContainer :: (WithGameData w m, HasContainer w) => Entity -> m Bool 
isOpaqueClosedContainer e = do
    c <- use $ gameWorld . container e
    return $ c ^? _Just . containerObjData . opacity == Just Opaque && fmap _containerOpenable c == Just Closed
