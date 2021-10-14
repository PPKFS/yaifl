module Yaifl.Components.Player (
    Player (..),
    movePlayer',
    getPlayer',
    makePlayer,
) where

import Yaifl.Common
import Yaifl.Components.Enclosing
import Yaifl.Components.Object
import Yaifl.Prelude

newtype Player = Player Entity deriving (Show)

getPlayer' :: forall w m. (HasStore w Player, WithGameData w m) => m Entity
getPlayer' = coerce <$> getComponent' @Player uniqueComponent

movePlayer' :: (HasStore w Player, HasThing w, HasStore w Enclosing, WithGameData w m) => Entity -> m Bool
movePlayer' r = do
    p <- getPlayer'
    move p r

makePlayer :: forall w m. (HasStore w Player, ThereIsThingConstraints w m) => Entity -> m Entity
makePlayer e = do
    withEntityIDBlock e $
        thereIs @(Thing w) "yourself" "it's you." (described .= NotDescribed)
    setComponent uniqueComponent (Player e)
    return e