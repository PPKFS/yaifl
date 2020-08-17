{- |
Copyright: (c) 2020 Avery
SPDX-License-Identifier: MIT
Maintainer: Avery <thecommunistduck@hotmail.co.uk>

Yet another interactive fiction library.
-}

module Yaifl.WorldBuilder
(
    WorldBuildInfo(..), WorldBuilder, blankBuildInfo,
    addRoom, addRule, addWhenPlayBeginsRule,
    buildWorld
) where

import Relude
import Yaifl.Common
import Control.Lens
import Yaifl.Std
import Yaifl.Say

data WorldBuildInfo w = WBI
    {
        _currentRoom :: Entity,
        _rulebookCache :: RulebookCache w
    } deriving Show

data RulebookCache w = RulebookCache
    {
        _whenPlayBeginsRules :: UncompiledRulebook w (),
        _printingNameActivity :: UncompiledActivity w ()
    } deriving Show

blankBuildInfo :: (HasStd' w) => WorldBuildInfo w
blankBuildInfo = WBI 0 (RulebookCache whenPlayBeginsRulesImpl printingNameImpl)

makeLenses ''WorldBuildInfo
makeLenses ''RulebookCache

addRuleWithArgs :: Lens' (RulebookCache w) (UncompiledRulebook w rv) -> Text 
            -> System (w, rv) (Maybe Bool) -> System (w, WorldBuildInfo w) ()
addRuleWithArgs rb n r = zoom (_2 . rulebookCache . rb . rules) (modify (++ [(n, r)]) )

addRule :: Lens' (RulebookCache w) (UncompiledRulebook w ()) -> Text 
            -> System w (Maybe Bool) -> System (w, WorldBuildInfo w) ()
addRule rb n r = addRuleWithArgs rb n (zoom _1 r)

addWhenPlayBeginsRule :: Text -> System w (Maybe Bool) -> System (w, WorldBuildInfo w) ()
addWhenPlayBeginsRule = addRule whenPlayBeginsRules

type WorldBuilder w = State (w, WorldBuildInfo w) ()
type WorldBuilderStep w = State (w, WorldBuildInfo w) Entity

addRoom :: HasStd' w => Text -> WorldBuilderStep w
addRoom n = do
    e <- zoom _1 (do
        e <- makeRoom n 
        gameInfo . firstRoom %= (\x -> if x == missingRoom then e else x)
        return e)
    _2 . currentRoom .= e
    return e

buildWorld :: HasStd' w => WorldBuilder w -> w -> w
buildWorld wb blw = compileRulebooks (_rulebookCache (snd execWB)) (fst execWB)
    where execWB = execState (do
            initWorld
            wb) (blw, blankBuildInfo)

initWorld :: HasStd' w => System (w, WorldBuildInfo w) ()
initWorld = do
    makeVoid
    makePlayer
    pass

makeVoid :: HasStd' w => System (w, WorldBuildInfo w) ()
makeVoid = do
    e <- use $ _1 . world . gameInfo . entityCounter
    _1 . world . gameInfo . entityCounter .= missingRoom
    _ <- addRoom "The void"
    _1 . world . gameInfo . entityCounter .= e
    

doUntilJustM :: (Foldable t, Monad m) => (a1 -> m (Maybe a2)) -> t a1 -> m (Maybe a2)
doUntilJustM f = runMaybeT . asumMap (MaybeT . f)

-- | opposite of zoom, from a state over b1, run a state with b2 bolted on
zoomOut :: Monad m => StateT (b1, b2) m a -> b2 -> StateT b1 m a
zoomOut stabc b = StateT $ \a -> do 
    (c, (a', _)) <- runStateT stabc (a, b)
    pure (c, a')

compileRulebook :: HasMessageBuffer w => UncompiledRulebook w r -> Rulebook w
compileRulebook r = CompiledRulebook (do
    let (CompiledRulebook cr) = compileRulebookWithResult r
    w <- get
    let iv = _rulebookInit r w
    zoomOut cr iv)

compileRulebookWithResult :: HasMessageBuffer w => UncompiledRulebook w r -> Rulebook (w, r)
compileRulebookWithResult (Rulebook n initVars rs) = CompiledRulebook (
    if null rs then return Nothing else (do
    (w, _) <- get
    unless (n == "") $ sayDbgLn $ "Following the " <> n
    indentDbg True
    let iv = initVars w
    _2 .= iv
    result <- doUntilJustM (\r1 -> do
        unless (fst r1 == "") $ sayDbgLn $ "Following the " <> fst r1
        snd r1
        ) rs
    indentDbg False
    sayDbgLn $ "Finished following the " <> n <> " with result " <> maybe "nothing" show result
    return result))

-- | what is this atrocious mess
compileActivity :: HasMessageBuffer w => UncompiledActivity w r -> Activity w
compileActivity a = CompiledActivity (\e -> do
    w <- get
    let initRules = _initActivity a w
    let (CompiledRulebook cb) = compileRulebookWithResult (_beforeRules a (e, initRules))
    let (CompiledRulebook cf) = compileRulebookWithResult (_forRules a (e, initRules))
    let (CompiledRulebook ca) = compileRulebookWithResult (_afterRules a (e, initRules))
    w' <- get
    let Identity (_, (w1, (e1, r1))) = runStateT cb (w', (e, initRules))
    let Identity (_, (w2, (e2, r2))) = runStateT cf (w1, (e1, r1))
    let Identity (_, (w3, (_, _))) = runStateT ca (w2, (e2, r2))
    put w3
    return Nothing
    )

compileRulebooks :: HasStd u w => RulebookCache w -> u -> u
compileRulebooks rbs = execState (do
    addCompiledRulebook whenPlayBeginsName $ compileRulebook (_whenPlayBeginsRules rbs)
    addCompiledActivity printNameName $ compileActivity (_printingNameActivity rbs)
    pass)

addCompiledActivity :: HasStd u w => Text -> Activity w -> System u ()
addCompiledActivity n a = world . gameInfo . activities . at n ?= a

addCompiledRulebook :: HasStd u w => Text -> Rulebook w -> System u ()
addCompiledRulebook n rb = world . gameInfo . rulebooks . at n ?= rb