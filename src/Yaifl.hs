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
    printName, getDescription,
    Player, Physical, RoomData, Enclosing, Container, Openable, Supporter,
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
    introText,
    stdBuffer
) where

import Yaifl.Common
import Yaifl.Say
import Yaifl.TH
import Yaifl.WorldBuilder

--rulebooks :: Has w (Rulebook w) => Lens' w (Store (Rulebook w))
--rulebooks = store (Proxy :: Proxy (Rulebook w))

