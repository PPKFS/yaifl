module Yaifl.Components.Player
    ( Player(..)
    , movePlayer
    , getPlayer
    , makePlayer
    )
where

import Yaifl.Prelude
import Yaifl.Common
import Yaifl.Components.Object
import Yaifl.Components.Physical
import Colog
import Yaifl.Components.Enclosing

newtype Player = Player Entity deriving Show

-- UNSAFE. TODO: ERROR HANDLE.
getPlayer :: forall w m. (HasStore w Player, WithGameData w m) => m Entity
getPlayer = do
    p <- getComponent @Player uniqueComponent
    let res = maybeToRight "Somehow couldn't find the player. I think you deleted it." p
    either (\v -> do 
        logError v
        return 0) (return . coerce) res

movePlayer :: (HasStore w Player, HasThing w, HasStore w Enclosing, WithGameData w m) => Entity -> m Bool 
movePlayer r = do
    p <- getPlayer
    move p r

makePlayer :: forall w m. (HasStore w Player, HasThing w, WithGameData w m) => Entity -> m Entity
makePlayer e = do
    withEntityIDBlock e $ thereIs @(Thing w) $ do
        name .= "yourself" 
        description .= "it's you."
        described .= NotDescribed 
    setComponent uniqueComponent (Player e)
    return e