{- |
Copyright: (c) 2020 Avery
SPDX-License-Identifier: MIT
Maintainer: Avery <thecommunistduck@hotmail.co.uk>

Yet another interactive fiction library.
-}
module Yaifl
(
    Entity, Store, Object, System, GameInfo, MessageBuffer,
    blankGameInfo, emptyStore,
    gameInfo, title, setTitle,
    Has, HasGameInfo, HasMessageBuffer, store, messageBuffer, blankMessageBuffer, msgBuffer,
    printMessageBuffer,
    say, sayLn, sayDbg, sayDbgLn,
    printName,
    Player, Physical, RoomData, Enclosing,
    newEntity, addComponent,
    makePlayer, makeThing, makeThingWithoutDescription, makeRoom,
    makeWorld,   
    UncompiledRulebook(..), Rulebook(..),
    blankRulebook, rules, 
    rulebooks,
    defaultWorld, entityCounter,
    objectComponent, world,
    description,
    HasStd', HasStd, HasWorld, world,
    mapObjects, mapObjects2,
    WorldBuildInfo(..), WorldBuilder, blankBuildInfo,
    addRoom, addRule, addWhenPlayBeginsRule,
    whenPlayBeginsName, printNameName, printDarkRoomNameName,
    buildWorld,
    introText
) where

import Relude
import Yaifl.Common
import Yaifl.Std
import Yaifl.Say
import Yaifl.TH
import Yaifl.WorldBuilder

data NameProperness = ImproperNamed | ProperNamed deriving Show
data NamePlurality = SingularNamed | PluralNamed deriving Show

data NamingOptions = NamingOptions
    {
        _nameProperness :: NameProperness,
        _namePlurality :: NamePlurality,
        _indefiniteArticle :: Text
    }

--rulebooks :: Has w (Rulebook w) => Lens' w (Store (Rulebook w))
--rulebooks = store (Proxy :: Proxy (Rulebook w))

