{- |
Copyright: (c) 2020 Avery
SPDX-License-Identifier: MIT
Maintainer: Avery <thecommunistduck@hotmail.co.uk>

Yet another interactive fiction library.
-}

module Yaifl.Common 
(
    Entity, Store, Object, System, GameInfo,
    blankGameInfo, emptyStore, globalComponent,
    gameInfo, title, setTitle, store,
    Has, HasGameInfo,
 
    newEntity, addComponent, makeObject, makeRule,
    UncompiledRulebook(..), Rulebook(..),
    blankRulebook, rules, 
    getWorld
) where

import Relude
import qualified Data.IntMap.Strict as IM
import qualified Text.Show
import Lens.Micro.Platform


type Entity = Int

type Store a = IntMap a

emptyStore :: Store a
emptyStore = IM.empty

type Name = Text
type Description = Text

data Object = Object
    {
        _name :: Name,
        _description :: Description
    } deriving Show


type RuleOutcome = Maybe Bool
type System w a = State w a

data UncompiledRulebook w r = Rulebook
    {
        _rulebookName :: Text,
        _rulebookInit :: w -> r,
        _rules :: [(Text, System (w, r) RuleOutcome)]
    }

instance Show (UncompiledRulebook w r) where
    show (Rulebook n _ r) = toString n <> concatMap (toString . fst) r

blankRulebook :: Text -> UncompiledRulebook w ()
blankRulebook n = Rulebook n (const ()) []

newtype Rulebook w = CompiledRulebook (System w (Maybe Bool))

data GameInfo = GameInfo 
    {
        _title :: Text,
        _entityCounter :: Int
    } deriving Show

makeClassy ''GameInfo
instance HasGameInfo w => HasGameInfo (w, a) where
    gameInfo = _1 . gameInfo

class Has w a where
    store :: Proxy a -> Lens' w (Store a)

instance Has w a => Has (w, b) a where
    store p = _1 . store p

blankGameInfo :: GameInfo
blankGameInfo = GameInfo "" 0

setTitle :: HasGameInfo w => Text -> System w ()
setTitle t = gameInfo . title .= t

newEntity :: HasGameInfo w => System w Entity
newEntity = gameInfo . entityCounter <%= (+1)

globalComponent :: Entity
globalComponent = 0
addComponent :: Has w a => Entity -> a -> System w ()
addComponent e c = store (Proxy :: Proxy a) . at e ?= c

makeObject :: HasGameInfo w => Has w Object => Text -> Text -> System w Entity
makeObject n d = do
    e <- newEntity
    addComponent e (Object n d) 
    return e

makeRule :: Text -> System (w, r) (Maybe Bool) -> (Text, System (w, r) (Maybe Bool))
makeRule = (,)

getWorld :: System (w, r) w
getWorld = fst <$> get

makeLenses ''UncompiledRulebook