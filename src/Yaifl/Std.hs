{- |
Copyright: (c) 2020 Avery
SPDX-License-Identifier: MIT
Maintainer: Avery <thecommunistduck@hotmail.co.uk>

Yet another interactive fiction library.
-}

module Yaifl.Std 
(
    Player, Physical, RoomData, Enclosing,
    makePlayer, makeThing, makeThingWithoutDescription, makeRoom,
    whenPlayBeginsRulesImpl, printingNameImpl, HasStd, HasStd',
    printName,
    introText,
    whenPlayBeginsName, printNameName, printDarkRoomNameName,
    defaultWorld
) where

import Relude
import Yaifl.Common
import Yaifl.Say
import Yaifl.Utils
import qualified Data.Text.Prettyprint.Doc.Render.Terminal as PPTTY
import qualified Language.Haskell.TH as TH
import qualified Data.Text as Text
import qualified Data.Set as DS
import Control.Lens

{- TYPES -}

type HasStd a w = (HasWorld a w, HasWorld w w, HasGameInfo a w, HasGameInfo' w, Has w Object, Has w Physical, 
                    Has w Player, Has w Enclosing, Has w RoomData, HasMessageBuffer w, HasMessageBuffer a)
type HasStd' w = HasStd w w

data ThingLit = Lit | Unlit deriving Show
data Edibility = Edible | Inedible deriving Show
data Portability = FixedInPlace | Portable deriving Show
data Wearability = Wearable | Unwearable deriving Show
data Pushability = PushableBetweenRooms | NotPushableBetweenRooms deriving Show

data Physical = Physical
    {
        _location :: Entity,
        _lit :: ThingLit,
        _edible :: Edibility,
        _portable :: Portability,
        _wearable :: Wearability,
        _pushable :: Pushability
    } deriving Show

physicalComponent :: Proxy Physical
physicalComponent = Proxy
emptyPhysical :: Physical
emptyPhysical = Physical missingRoom Lit Edible Portable Wearable PushableBetweenRooms

makeThing :: HasStd a w => Text -> Text -> System a Entity
makeThing n d = do
    e <- makeObject n d
    addComponent e emptyPhysical
    return e

makeThingWithoutDescription :: HasStd a w => Text -> System a Entity
makeThingWithoutDescription n = makeThing n ""

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

makeRoom :: HasStd a w => Text -> System a Entity
makeRoom n = do
    e <- makeObject n ("It's the " <> n <> ".")
    addComponent e (RoomData Unvisited emptyStore Nothing)
    addComponent e (Enclosing DS.empty)
    return e

getFirstRoom :: HasGameInfo a w => a -> Entity
getFirstRoom w = w ^. gameInfo . firstRoom

newtype Enclosing = Enclosing
    {
        _encloses :: Set Entity
    } deriving Show
enclosingComponent :: Proxy Enclosing
enclosingComponent = Proxy

newtype Player = Player Int deriving Show

makePlayer :: HasStd a w => System a Entity
makePlayer = do
    e <- makeThing "yourself" "it's you."
    addComponent globalComponent (Player e)
    return e

getPlayer :: HasComponent a w Player => a -> Entity
getPlayer w = maybe missingPlayer (\(Player p) -> p) $ 
                w ^. world . store (Proxy :: Proxy Player) . at globalComponent

makeLenses ''Physical
makeLenses ''Enclosing
defaultWorld :: [TH.Name]
defaultWorld = [''Object, ''RoomData, ''Physical, ''Enclosing, ''Player]

move :: HasStd a w => Entity -> Entity -> System a Bool
move t le = do
    w <- get
    let mp = getComponent w physicalComponent t
        mloc = getComponent w enclosingComponent le
        mcurrLoc = mp ^? _Just . location
    sayDbgLn $ show le
    doIfExists3 mp mloc mcurrLoc (show t <> " no physical thing to move") "no future loc" "no current loc" 
        (\_ _ c -> do
            world . component physicalComponent t . _Just . location .= le
            world . component enclosingComponent c . _Just . encloses %= DS.delete t
            world . component enclosingComponent le . _Just . encloses %= DS.insert t
            return True
        )

whenPlayBeginsRulesImpl :: (HasStd' w) => UncompiledRulebook w ()
whenPlayBeginsRulesImpl = (blankRulebook "when play begins rulebook") {
    _rules = 
        [
            makeRule "display banner rule" (do
                sayIntroText
                return Nothing),
                
            makeRule "position player in model world rule" (do
                w <- get
                _ <- move (getPlayer w) (getFirstRoom w)
                return Nothing),
            makeRule "initial room description rule" (do
                tryAction "looking" []
                return Nothing)
        ]
}

introText :: Text -> [Text]
introText w = [longBorder<>"\n", shortBorder <> " " <> w <> " " <> shortBorder<>"\n", 
                longBorder<>"\n\n"]
            where shortBorder = "------" 
                  totalLength = 2 * Text.length shortBorder + Text.length w + 2
                  longBorder = foldr (<>) "" $ replicate totalLength ("-" :: Text)

sayIntroText :: HasStd a w => System a ()
sayIntroText = do
    w <- get
    setStyle (Just (PPTTY.color PPTTY.Green <> PPTTY.bold))
    mapM_ say (introText $ w ^. gameInfo . title)
    setStyle Nothing
    pass

printingNameOfADarkRoomImpl :: (HasMessageBuffer w) => UncompiledRulebook w ()
printingNameOfADarkRoomImpl = (blankRulebook printDarkRoomNameName) {
    _rules = [
        makeRule "" (do
            say "Darkness"
            return Nothing)]
}

printingNameImpl :: HasStd' w => UncompiledActivity w ()
printingNameImpl = makeActivity printNameName 1 (do
    (w, (e, ())) <- get
    whenJust (getComponent w objectComponent =<< viaNonEmpty head e) (say . _name)
    return $ Just True)

makeActivity :: Text -> Int -> RuleEvaluation (w, ([Entity], ())) -> UncompiledActivity w ()
makeActivity n numObj r = Activity n numObj (const ()) (blankRulebookVars ("before " <> n)) 
                    (\r1 -> (blankRulebookVars ("for " <> n) r1) 
                        {
                            _rules = [("", r)]
                        })
                            (blankRulebookVars ("after " <> n))

printNameName :: Text
printNameName = "printing the name"
printDarkRoomNameName :: Text
printDarkRoomNameName = "printing the name of a dark room"
whenPlayBeginsName :: Text
whenPlayBeginsName = "when play begins rules"

printName :: (HasStd u w, HasID o) => o -> System u ()
printName o = zoom world $ doActivity printNameName [objID o]

doActivity :: HasStd' w => Text -> [Entity] -> System w ()
doActivity n params = do
    sayDbgLn $ "running activity " <> n
    w <- get
    let v = w ^. world . gameInfo . activities . at n
    maybe (sayDbgLn $ "couldn't find activity " <> n) (\(CompiledActivity x) -> do
        _ <- x params
        pass) v
