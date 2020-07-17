{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import Yaifl
import Relude
import Lens.Micro.Platform
import qualified Data.Text as Text
import Data.Text.Prettyprint.Doc.Render.Terminal

makeWorld "World" defaultWorld
makeClassy ''World

instance HasWorld w => HasWorld (w, a) where
    world = _1 . world

blankBuildInfo :: WorldBuildInfo w
blankBuildInfo = WBI 0 (RulebookCache whenPlayBeginsRulesImpl 0)
data RulebookCache w = RulebookCache
    {
        _whenPlayBeginsRules :: UncompiledRulebook w (),
        _dummy2 :: Int
    } deriving Show

makeLenses ''WorldBuildInfo
makeLenses ''RulebookCache

instance HasGameInfo World where
    gameInfo = worldGameInfo

instance Has World Object where
    store _ = objectStore

instance Has World RoomData where
    store _ = roomDataStore

instance Has World Physical where
    store _ = physicalStore

instance Has World (Rulebook World) where
    store _ = rulebookStore

addRule :: Lens' (RulebookCache w) (UncompiledRulebook w rv) -> Text 
            -> System (w, rv) (Maybe Bool) -> System (w, WorldBuildInfo w) ()
addRule rb n r = zoom (_2 . rulebookCache . rb . rules) (modify (++ [(n, r)]) )

addWhenPlayBeginsRule :: Text -> System (w, ()) (Maybe Bool) -> System (w, WorldBuildInfo w) ()
addWhenPlayBeginsRule = addRule whenPlayBeginsRules

type WorldBuilder w = State (w, WorldBuildInfo w) ()
type WorldBuilderStep w = State (w, WorldBuildInfo w) Entity

addRoom :: (HasGameInfo w, Has w Object, Has w RoomData) => Text -> WorldBuilderStep w
addRoom n = do
    e <- zoom _1 $ makeRoom n
    _2 . currentRoom .= e
    return e

example1World :: WorldBuilder World
example1World = do
    setTitle "Bic"
    addRoom "The Staff Break Room"
    makeThingNoDesc "Bic pen"
    makeThing "orange" 
        "It's a small hard pinch-skinned thing from the lunch room, probably with lots of pips and no juice."
    makeThing "napkin" "Slightly crumpled."
    addWhenPlayBeginsRule "n" (return Nothing)
    addWhenPlayBeginsRule "t" (return $ Just True)
    addWhenPlayBeginsRule "f" (return $ Just False)

buildWorld :: (HasGameInfo w, Has w (Rulebook w)) => WorldBuilder w -> w -> w
buildWorld wb blw = compileRulebooks (_rulebookCache (snd execWB)) (fst execWB)
    where execWB = execState wb (blw, blankBuildInfo)

doUntilJustM :: (Foldable t, Monad m) => (a1 -> m (Maybe a2)) -> t a1 -> m (Maybe a2)
doUntilJustM f = runMaybeT . asumMap (MaybeT . f)

-- | opposite of zoom, from a state over b1, run a state with b2 bolted on
zoomOut :: Monad m => StateT (b1, b2) m a -> b2 -> StateT b1 m a
zoomOut stabc b = StateT $ \a -> do 
    (c, (a', _)) <- runStateT stabc (a, b)
    pure (c, a')

compileRulebook :: UncompiledRulebook w r -> Rulebook w
compileRulebook (Rulebook _ initVars rs) = CompiledRulebook (do
    w <- get
    doUntilJustM (\r1 -> zoomOut (snd r1) (initVars w)) rs)

compileRulebooks :: (HasGameInfo w, Has w (Rulebook w)) => RulebookCache w -> w -> w
compileRulebooks rbs = execState (
        addCompiledRulebook $ compileRulebook (_whenPlayBeginsRules rbs)
    )

addCompiledRulebook :: (HasGameInfo w, Has w (Rulebook w)) => Rulebook w -> System w Entity
addCompiledRulebook rb =  do
    e <- newEntity
    addComponent e rb

main :: IO ()
main = do
    print $ execState example1World (blankWorld, blankBuildInfo)
    let w = buildWorld example1World blankWorld
    print $ w ^. gameInfo . entityCounter
    let (Just (CompiledRulebook w2)) = w ^? rulebookStore . at 4 . _Just 
    let w3 = runState w2 w
    print $ fst w3
    pass
