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
import Yaifl.Utils
import Yaifl.Say
import Data.Map as DM
import qualified Text.Show

data WorldBuildInfo w = WBI
    {
        _currentRoom :: Entity,
        _rulebookCache :: RulebookCache w
    } deriving Show

data RulebookCache w = RulebookCache
    {
        _whenPlayBeginsRules :: UncompiledRulebook w (),
        _actionProcessingRules :: forall r p. (RulebookArgs r, ActionArgs p) => UncompiledAction w r p -> [Entity] ->  UncompiledRulebook w (p, r),
        _printingNameActivity :: UncompiledActivity w () Entity,
        _printingDarkRoomNameActivity :: UncompiledActivity w () (),
        _lookingAction :: UncompiledAction w LookingActionVariables ()
    }

instance Show (RulebookCache w) where
    show _ = "rulebooks"

blankBuildInfo :: (HasStd' w) => WorldBuildInfo w
blankBuildInfo = WBI 0 (RulebookCache whenPlayBeginsRulesImpl actionProcessingRulebookImpl printingNameImpl printingNameOfADarkRoomImpl lookingActionImpl)

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
        gameInfo . firstRoom %= (\x -> if Relude.null x then Just e else x)
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
    
-- | what is this atrocious mess
compileActivity :: (HasMessageBuffer w, ActionArgs p) => UncompiledActivity w r p -> Activity w
compileActivity a = CompiledActivity (\e -> do
    w <- get
    let initRules = _initActivity a w
    let args = unboxArguments e
    whenJust args (\defArgs ->
        zoomOut (do
            getRule $ compileRulebookWithResult (_beforeRules a (defArgs, initRules))
            (_, (_, r)) <- get
            getRule $ compileRulebookWithResult (_forRules a (defArgs, r)) 
            (_, (_, r)) <- get
            getRule $ compileRulebookWithResult (_afterRules a (defArgs,  r))
            pass) (defArgs, initRules))
    return Nothing
    )

compileAction ::HasStd' w => UncompiledAction w r p
                                 -> (UncompiledAction w r p -> [Entity] ->  UncompiledRulebook w (p, r)) -> Action w
compileAction action aprules = CompiledAction (\e -> do
    sayDbgLn $ "doing action " <> _actionName action
    let (CompiledRulebook r) = compileRulebook (aprules action e) 
    r)

compileRulebooks :: (HasStd u w) => RulebookCache w -> u -> u
compileRulebooks rbs = execState (do
    let addAction = addCompiledAction (_actionProcessingRules rbs)
    addCompiledRulebook (_whenPlayBeginsRules rbs)
    addCompiledActivity (_printingNameActivity rbs)
    addCompiledActivity (_printingDarkRoomNameActivity rbs)
    addAction (_lookingAction rbs)
    pass)

addCompiledInfo :: HasStd u w => Lens' (GameInfo w) (DM.Map Text x) -> Text -> x -> System u ()
addCompiledInfo l n a = world . gameInfo . l . at n ?= a

addCompiledActivity :: (ActionArgs p, HasStd u w) => UncompiledActivity w r p -> System u ()
addCompiledActivity r = addCompiledInfo activities  (_activityName r) $ compileActivity r

addCompiledRulebook :: HasStd u w => UncompiledRulebook w r -> System u ()
addCompiledRulebook r = addCompiledInfo rulebooks (_rulebookName r) $ compileRulebook r

addCompiledAction :: HasStd u w => (UncompiledAction w r p -> [Entity] -> UncompiledRulebook w (p, r)) -> UncompiledAction w r p -> System u ()
addCompiledAction ap r = addCompiledInfo actions (_actionName r) $ compileAction r ap