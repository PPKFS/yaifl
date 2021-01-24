module Yaifl.Components.Player
    ( Player(..)
    , playerComponent
    )
where

import Yaifl.Prelude
import Yaifl.Common
import Yaifl.Components.Object
import Yaifl.Components.Physical
import Yaifl.Components.Enclosing

newtype Player = Player Entity deriving Show

player :: forall w m. (HasStore w Player, WithGameLog w m)  => World w m Entity
player = do
    p <- use $ gameWorld . store @w @Player . at 
    let res = maybeToRight "Somehow couldn't find the player. I think you deleted it." (coerce p)
    when (isLeft res) (logMsg Error "Attempted to find the player and failed.")
    fromEither res
{-
playerLocation' :: HasWorld w '[Player, Physical] r => Sem r Entity
playerLocation' = do
    p <- getPlayer'
    _location <$> getComponent' physicalComponent p

makePlayer :: HasWorld w '[Player, Physical, Enclosing] r => Entity -> Sem r Entity
makePlayer e' = do
    e <- makeThing' "yourself" "it's you." e'
    adjustComponent physicalComponent e (set described NotDescribed)
    addComponent globalComponent (Player e)
    return e-}