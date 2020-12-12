{- |
Copyright: (c) 2020 Avery
SPDX-License-Identifier: MIT
Maintainer: Avery <thecommunistduck@hotmail.co.uk>

Yet another interactive fiction library.
-}

module Yaifl.WorldBuilder
(
    WorldBuildInfo(..), WorldBuilder, blankBuildInfo,
    addRoom, addRoom', addRule, addWhenPlayBeginsRule, addThing, addThing',
    isLocated, isWestOf,
    buildWorld
) where

import Relude
import Yaifl.Common
import Yaifl.Actions
import Yaifl.Components
import Yaifl.Activities
import Yaifl.Rulebooks
import Control.Lens
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
        _lookingAction :: UncompiledAction w LookingActionVariables (),
        _printingDescriptionOfADarkRoom :: UncompiledActivity w () (),
        _describingLocaleActivity :: UncompiledActivity w LocaleDescription Entity,
        _choosingNotableLocaleObjectsActivity :: UncompiledActivity w () Entity,
        _printingLocaleParagraphAboutActivity :: UncompiledActivity w Bool Entity
    }

instance Show (RulebookCache w) where
    show _ = "rulebooks"

blankBuildInfo :: (HasStd' w) => WorldBuildInfo w
blankBuildInfo = WBI 0 (RulebookCache whenPlayBeginsRulesImpl actionProcessingRulebookImpl printingNameOfSomethingImpl printingNameOfADarkRoomImpl lookingActionImpl printingDescriptionOfADarkRoomImpl describingLocaleActivityImpl choosingNotableLocaleObjectsActivityImpl printingLocaleParagraphAboutActivityImpl)

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

addRoom' n = do
    e <- zoom _1 (do
        e <- makeRoom' n
        gameInfo . firstRoom %= (\x -> if Relude.null x then Just e else x)
        return e)
    _2 . currentRoom .= e
    return e

addRoom :: HasStd' w => Text -> Description -> WorldBuilderStep w
addRoom n d = do
    e <- zoom _1 (do
        e <- makeRoom n d
        gameInfo . firstRoom %= (\x -> if Relude.null x then Just e else x)
        return e)
    _2 . currentRoom .= e
    return e

addThing' :: HasStd' w => Text -> WorldBuilderStep w
addThing' n = addThing n ""

addThing :: HasStd' w => Text -> Description -> WorldBuilderStep w
addThing n d = do
    (_, wbi) <- get
    zoom _1 $ makeThing n d (_currentRoom wbi)

buildWorld :: HasStd' w => WorldBuilder w -> w -> w
buildWorld wb blw = compileRulebooks (_rulebookCache (snd execWB)) (fst execWB)
    where execWB = execState (do
            initWorld
            wb) (blw, blankBuildInfo)

initWorld :: HasStd' w => System (w, WorldBuildInfo w) ()
initWorld = do
    e <- makeVoid
    makeDirections
    makePlayer e
    pass

missingRoom :: Int
missingRoom = -3

isWestOf :: HasStd' w => Entity -> WorldBuilder w
isWestOf e = do
   r <- use $ _2 . currentRoom
   isLocated e west r

--TODO: one way connections
isLocated :: HasStd' w => Entity -> Entity -> Entity -> WorldBuilder w
isLocated e1 dir e2 = do
    component' roomComponent e1 . mapConnections . at dir ?= e2
    (Direction opp) <- use $ component' directionComponent dir
    component' roomComponent e1 . mapConnections . ix dir .= e2

makeVoid :: HasStd' w => System (w, WorldBuildInfo w) Entity
makeVoid = do
    e <- use $ _1 . world . gameInfo . entityCounter
    _1 . world . gameInfo . entityCounter .= missingRoom
    _ <- addRoom "The void" "you really shouldn't be here."
    _1 . world . gameInfo . firstRoom .= Nothing
    _1 . world . gameInfo . entityCounter .= e
    return e
    
compileRulebooks :: (HasStd u w) => RulebookCache w -> u -> u
compileRulebooks rbs = execState (do
    let addAction = addCompiledAction (_actionProcessingRules rbs)
    addCompiledRulebook (_whenPlayBeginsRules rbs)
    addCompiledActivity (_printingNameActivity rbs)
    addCompiledActivity (_printingDarkRoomNameActivity rbs)
    addAction (_lookingAction rbs)
    addCompiledActivity (_printingDescriptionOfADarkRoom rbs)
    addCompiledActivity (_describingLocaleActivity rbs)
    addCompiledActivity (_choosingNotableLocaleObjectsActivity rbs)
    addCompiledActivity (_printingLocaleParagraphAboutActivity rbs)
    pass)

addCompiledInfo :: HasStd u w => Lens' (GameInfo w) (DM.Map Text x) -> Text -> x -> System u ()
addCompiledInfo l n a = world . gameInfo . l . at n ?= a

addCompiledActivity :: (ActionArgs p, HasStd u w) => UncompiledActivity w r p -> System u ()
addCompiledActivity r = addCompiledInfo activities  (_activityName r) $ compileActivity r

addCompiledRulebook :: HasStd u w => UncompiledRulebook w r -> System u ()
addCompiledRulebook r = addCompiledInfo rulebooks (_rulebookName r) $ compileRulebook' r Full

addCompiledAction :: HasStd u w => (UncompiledAction w r p -> [Entity] -> UncompiledRulebook w (p, r)) -> UncompiledAction w r p -> System u ()
addCompiledAction ap r = addCompiledInfo actions (_actionName r) $ compileAction r ap