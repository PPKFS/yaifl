{- |
Copyright: (c) 2020 Avery
SPDX-License-Identifier: MIT
Maintainer: Avery <thecommunistduck@hotmail.co.uk>

Yet another interactive fiction library.
-}
module Yaifl
(
    Entity, Store, Object, System, GameInfo,
    blankGameInfo, emptyStore,
    gameInfo, title, setTitle,
    Has, HasGameInfo,

    Player, Physical, RoomData,    
    newEntity, addComponent,
    makePlayer, makeThing, makeThingNoDesc, makeRoom,
    makeWorld,   
    UncompiledRulebook(..), Rulebook(..),
    blankRulebook, rules, 
    defaultWorld
) where

import Relude
import Yaifl.Common
import Yaifl.Std
import Yaifl.TH

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

