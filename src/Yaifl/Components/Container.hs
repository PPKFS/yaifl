module Yaifl.Components.Container
    ( Opacity(..)
    , ContainerData(..)
    , ContainerObject(..)
    , isOpaqueClosedContainer
    , HasContainer
    )
where

import Yaifl.Prelude
import Yaifl.Common
import Yaifl.Components.Enclosing
import Yaifl.Components.Object
import Yaifl.Components.Openable
import qualified Data.Set as DS

data Opacity = Opaque | Transparent deriving (Eq, Show)

data ContainerData = ContainerData
    {
        _enterable :: Bool,
        _opacity :: Opacity,
        _carryingCapacity :: Int
    } deriving Show

data ContainerObject w = ContainerObject
    {
        _containerObject :: Object w,
        _containerObjData :: ContainerData,
        _containerEnclosing :: Enclosing,
        _containerOpenable :: Openable
    } deriving Show

makeClassy ''ContainerData
makeLenses ''ContainerObject

instance HasObject (ContainerObject w) w where
    object = containerObject

instance HasContainer w => HasStore w (ContainerObject w) where
    store = containers

instance ThereIs (ContainerObject w) where
    defaultObject e = ContainerObject (blankObject e "container") (ContainerData False Opaque (-1)) (Enclosing DS.empty) Closed

instance HasContainer w => Deletable w (ContainerObject w) where
    deleteObject e = do
        deleteComponent @(Object w) e
        deleteComponent @ContainerData e
        deleteComponent @Enclosing e
        deleteComponent @Openable e
        pass

type HasContainer w = (HasObjectStore w, HasStore w ContainerData, HasStore w Enclosing, HasStore w Openable)
deleteContainer :: forall w m. (WithGameData w m, HasContainer w) => Entity -> m ()
deleteContainer = deleteObject @w @(ContainerObject w)

containers :: HasContainer w => Lens' w (Store (ContainerObject w))
containers = storeLens4 ContainerObject _containerObject _containerObjData _containerEnclosing _containerOpenable

container :: HasContainer w => Entity -> Lens' w (Maybe (ContainerObject w) )
container k = containers . at k

isOpaqueClosedContainer :: (WithGameData w m, HasContainer w) => Entity -> m Bool 
isOpaqueClosedContainer e = do
    c <- use $ gameWorld . container e
    return $ c ^? _Just . containerObjData . opacity == Just Opaque && fmap _containerOpenable c == Just Closed
