{- |
Copyright: (c) 2020 Avery
SPDX-License-Identifier: MIT
Maintainer: Avery <thecommunistduck@hotmail.co.uk>

Yet another interactive fiction library.
-}

module Yaifl.Std 
(
    Player, Physical, RoomData,    
    makePlayer, makeThing, makeThingNoDesc, makeRoom,
    defaultWorld
) where

import Relude
import Yaifl.Common
import Yaifl.Say
import qualified Data.Text.Prettyprint.Doc.Render.Terminal as PPTTY
import qualified Language.Haskell.TH as TH
import qualified Data.Text as Text
import Lens.Micro.Platform

data ThingLit = Lit | Unlit deriving Show
data Edibility = Edible | Inedible deriving Show
data Portability = FixedInPlace | Portable deriving Show
data Wearability = Wearable | Unwearable deriving Show
data Pushability = PushableBetweenRooms | NotPushableBetweenRooms deriving Show

data Physical = Physical
    {
        _enclosedBy :: Maybe Entity,
        _location :: Entity,
        _lit :: ThingLit,
        _edible :: Edibility,
        _portable :: Portability,
        _wearable :: Wearability,
        _pushable :: Pushability
    } deriving Show

newtype Player = Player Int deriving Show
missingPlayer :: Entity
missingPlayer = -2

emptyPhysical :: Physical
emptyPhysical = Physical Nothing (-1) Lit Edible Portable Wearable PushableBetweenRooms

makeThing :: (HasGameInfo w, Has w Object, Has w Physical) => Text -> Text -> System w Entity
makeThing n d = do
    e <- makeObject n d
    addComponent e emptyPhysical
    return e

makeThingNoDesc :: (HasGameInfo w, Has w Object, Has w Physical) => Text -> System w Entity
makeThingNoDesc n = makeThing n ""

makePlayer :: (HasGameInfo w, Has w Object, Has w Player) => System w Entity
makePlayer = do
    e <- makeObject "yourself" "it's you."
    addComponent globalComponent (Player e)
    return e

getPlayer :: Has w Player => w -> Entity
getPlayer w = case w ^. store (Proxy :: Proxy Player) . at globalComponent of
    Just (Player p) -> p
    Nothing -> missingPlayer

data Darkness = Lighted | Dark deriving Show
data IsVisited = Visited | Unvisited deriving Show
type MapConnections = Store Entity
type ContainingRegion = Maybe Entity

data RoomData = RoomData
    {
        _isVisited :: IsVisited,
        _mapConnections :: MapConnections,
        _containingRegion :: ContainingRegion
    } deriving Show

makeRoom :: (HasGameInfo w, Has w Object, Has w RoomData) => Text -> System w Entity
makeRoom n = do
    e <- makeObject n ("It's the " <> n <> ".")
    addComponent e (RoomData Unvisited emptyStore Nothing)
    return e

defaultWorld :: [TH.Name]
defaultWorld = [''Object, ''RoomData, ''Physical, ''Player]

introText :: (HasMessageBuffer w, HasGameInfo w) => System w ()
introText = do
    w <- get
    let shortBorder = "------" 
        totalLength = 2 * Text.length shortBorder + Text.length (w ^. gameInfo . title) + 2
        longBorder = foldr (<>) "" $ replicate totalLength ("-" :: Text)
    setStyle (Just (PPTTY.color PPTTY.Green <> PPTTY.bold))
    sayLn longBorder
    w2 <- get
    sayLn (shortBorder <> " " <> (w2 ^. title) <> " " <> shortBorder)
    sayLn longBorder
    sayLn "\n"
    setStyle Nothing
    pass

move :: State Physical
whenPlayBeginsRulesImpl :: (HasMessageBuffer w, HasGameInfo w, Has w Player) => UncompiledRulebook w ()
whenPlayBeginsRulesImpl = (blankRulebook "when play begins rulebook") {
    _rules = 
        [
            makeRule "display banner rule" (do
                introText
                return Nothing),
                
            makeRule "position player in model world rule" (do
                    w <- getWorld
                    --move player firstRoom
                    return Nothing)
                {-
            -- | do looking.
            makeRule "initial room description rule" (\r -> do
                _ <- zoom _1 $ tryAction (r ^. actions . lookingAction) r
                return Nothing)-}
        ]
}

