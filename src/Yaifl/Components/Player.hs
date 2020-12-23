module Yaifl.Components.Player
    ( Player(..)
    , playerComponent
    )
where

import Yaifl.Prelude
import Yaifl.Common
import Yaifl.Say
import Yaifl.Components.Object
import Yaifl.Components.Physical
import Yaifl.Components.Enclosing
import Polysemy.Error

newtype Player = Player Entity deriving Show

playerComponent :: Proxy Player
playerComponent = Proxy :: Proxy Player

getPlayer' :: HasWorld w '[Player] r => Sem r Entity
getPlayer' = do
    p <- getComponent playerComponent globalComponent
    let res = maybeToRight "Somehow couldn't find the player. I think you deleted it." (coerce p)
    when (isLeft res) (logMsg Error "Attempted to find the player and failed.")
    fromEither res

playerLocation' :: HasWorld w '[Player, Physical] r => Sem r Entity
playerLocation' = do
    p <- getPlayer'
    _location <$> getComponent' physicalComponent p

makePlayer :: HasWorld w '[Player, Physical, Enclosing] r => Entity -> Sem r Entity
makePlayer e' = do
    e <- makeThing' "yourself" "it's you." e'
    adjustComponent physicalComponent e (set described NotDescribed)
    addComponent globalComponent (Player e)
    return e