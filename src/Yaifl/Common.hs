{- |
Copyright: (c) 2020 Avery
SPDX-License-Identifier: MIT
Maintainer: Avery <thecommunistduck@hotmail.co.uk>

Yet another interactive fiction library.
-}

module Yaifl.Common 
(
    Entity, Store, Object, System, GameInfo, Description(..),
    Activity(..), UncompiledActivity(..),
    Action(..), UncompiledAction(..), RoomDescriptions(..),
    blankGameInfo, emptyStore, globalComponent,
    gameInfo, title, setTitle, store, msgBuffer, world,
    activities,
    HasID, objID,
    Has, HasMessageBuffer,
    HasWorld, HasWorld', HasGameInfo, HasGameInfo', HasComponent,
    missingRoom, missingPlayer, firstRoom, actions,
    getDescription, _firstRoom,
    
    addComponent, component, setComponent, getComponent,
    newEntity, makeObject, makeRule,
    rulebooks,
    UncompiledRulebook(..), Rulebook(..),
    RuleOutcome, RuleEvaluation,
    blankRulebook, blankRulebookVars, rules, tryAction, description, roomDescriptions, _roomDescriptions,
    objectComponent, entityCounter,
    mapObjects, mapObjects2,
    _darknessWitnessed,
    getWorld,
    badArguments, isBad,
    ActionArgs, unboxArguments, RulebookArgs, defaultRulebookArguments,
    makeActivity, makeBlankRule, defaultArguments, makeRuleWithArgs,
    NumberOfArguments(..),
    _name, _objectID, _description,
    isComponent,

    compileRulebook, compileRulebookWithResult
) where

import Relude
import Yaifl.Say
import Yaifl.Utils
import qualified Data.IntMap.Strict as IM
import qualified Data.Map as Map
import qualified Text.Show
import Control.Lens

{- TYPES -}
-- | an entity is just an ID that is loosely associated with components.
type Entity = Int

-- | Store a is a container of components of type a, indexed by entity ID
type Store a = IntMap a

-- | A store with nothing in it
emptyStore :: Store a
emptyStore = IM.empty

type Name = Text
data Description = PlainDescription Text | DynamicDescription (forall w. w -> Entity -> Text)

instance IsString Description where
    fromString = PlainDescription . fromString

instance Semigroup Description where
    (<>) (PlainDescription e) (PlainDescription e2) = PlainDescription (e <> e2)
    (<>) (PlainDescription e) (DynamicDescription e2) = DynamicDescription (\w e1 -> e <> e2 w e1)
    (<>) (DynamicDescription e2) (PlainDescription e)  = DynamicDescription (\w e1 -> e2 w e1 <> e)
    (<>) (DynamicDescription e) (DynamicDescription e2) = DynamicDescription (\w e1 -> e w e1 <> e2 w e1)

instance Show Description where
    show (PlainDescription t) = show t
    show (DynamicDescription _) = "dynamicdescription" 

-- | possibly the most basic component, a name.
data Object = Object
    {
        _name :: Name,
        _description :: Description,
        _objectID :: Entity
    } deriving Show

objectComponent :: Proxy Object
objectComponent = Proxy

type RuleOutcome = Maybe Bool
type System w a = State w a
type RuleEvaluation w = System w RuleOutcome

-- | a rulebook over a world w with rulebook variable r.
data UncompiledRulebook w r = Rulebook
    {
        _rulebookName :: Text,
        _rulebookInit :: w -> r,
        _rules :: [(Text, RuleEvaluation (w, r))]
    }

newtype Rulebook w = CompiledRulebook { getRule :: RuleEvaluation w }

data GameInfo w = GameInfo 
    {
        _title :: Text,
        _firstRoom :: Maybe Entity,
        _msgBuffer :: MessageBuffer,
        _entityCounter :: Entity,
        _activities :: Map.Map Text (Activity w),
        _rulebooks :: Map.Map Text (Rulebook w),
        _actions :: Map.Map Text (Action w),
        _roomDescriptions :: RoomDescriptions,
        _darknessWitnessed :: Bool
    } deriving Show

data UncompiledActivity w r p = Activity
    {
        _activityName :: Text,
        _initActivity :: w -> r,
        _beforeRules :: (p, r) -> UncompiledRulebook w (p, r),
        _forRules :: (p, r) -> UncompiledRulebook w (p, r),
        _afterRules :: (p, r) -> UncompiledRulebook w (p, r)
    }

newtype Activity w = CompiledActivity ([Entity] -> RuleEvaluation w)

data NumberOfArguments = Exactly Int | NoArgs | AnyNumber | AtLeast Int

data RoomDescriptions = AbbreviatedRoomDescriptions | SometimesAbbreviatedRoomDescriptions
                            | NeverAbbreviatedRoomDescriptions deriving (Eq, Show)
-- | p is the parameters, r is any rulebook specific arguments
data UncompiledAction w r p = Action
    {
        _actionName :: Text,
        _understandAs :: [Text],
        _setActionVariables :: p -> UncompiledRulebook w (p, r),
        _beforeActionRules :: (p, r) -> UncompiledRulebook w (p, r),
        _checkActionRules :: (p, r) -> UncompiledRulebook w (p, r),
        _carryOutActionRules :: (p, r) -> UncompiledRulebook w (p, r),
        _reportActionRules :: (p, r) -> UncompiledRulebook w (p, r)
    }

newtype Action w = CompiledAction ([Entity] -> RuleEvaluation w)

{- TYPECLASSES -}

instance Show (UncompiledRulebook w r) where
    show (Rulebook n _ r) = toString n <> concatMap (toString . fst) r

instance Show (UncompiledAction w r p) where
    show = show . _actionName
instance Show (Rulebook w) where
    show _ = "compiled rulebook"

instance Show (Activity w) where
    show _ = "compiled activity"

instance Show (Action w) where
    show _ = "compiled action"

instance Show (UncompiledActivity w r p) where
    show = show . _activityName

class HasGameInfo u w => HasWorld u w | u -> w where
    world :: Lens' u w

instance HasWorld w w => HasWorld (w, b) w where
    world = _1 . world

type HasWorld' w = HasWorld w w

-- | basically just manually writing a lens typeclass for...reasons I cannot remember
class HasGameInfo u w | u -> w where
    gameInfo :: Lens' u (GameInfo w)
    title :: Lens' u Text
    title = gameInfo . go where go f g@GameInfo{..} = (\x' -> g {_title = x'}) <$> f _title
    firstRoom :: Lens' u (Maybe Entity)
    firstRoom = gameInfo . go where go f g@GameInfo{..} = (\x' -> g {_firstRoom = x'}) <$> f _firstRoom
    msgBuffer :: Lens' u MessageBuffer
    msgBuffer = gameInfo . go where go f g@GameInfo{..} = (\x' -> g {_msgBuffer = x'}) <$> f _msgBuffer
    entityCounter :: Lens' u Entity
    entityCounter = gameInfo . go where go f g@GameInfo{..} = (\x' -> g {_entityCounter = x'}) <$> f _entityCounter
    rulebooks :: Lens' u (Map.Map Text (Rulebook w))
    rulebooks = gameInfo . go where go f g@GameInfo{..} = (\x' -> g {_rulebooks = x'}) <$> f _rulebooks
    activities :: Lens' u (Map.Map Text (Activity w))
    activities = gameInfo . go where go f g@GameInfo{..} = (\x' -> g {_activities = x'}) <$> f _activities
    actions :: Lens' u (Map.Map Text (Action w))
    actions = gameInfo . go where go f g@GameInfo{..} = (\x' -> g {_actions = x'}) <$> f _actions
    roomDescriptions :: Lens' u RoomDescriptions
    roomDescriptions = gameInfo . go where go f g@GameInfo{..} = (\x' -> g {_roomDescriptions = x'}) <$> f _roomDescriptions

type HasGameInfo' w = HasGameInfo w w

instance HasGameInfo w w => HasGameInfo (w, a) w where
    gameInfo = _1 . gameInfo

instance HasGameInfo (GameInfo w) w where
    gameInfo = id

class Has w c where
    store :: Proxy c -> Lens' w (Store c)

class HasID o where
    objID :: o -> Entity

instance HasID Entity where
    objID = id

instance HasID Object where
    objID = _objectID
-- A UNIVERSE u (some type containing a WORLD w) has stores for a COMPONENT c
type HasComponent u w c = (HasWorld' w, HasWorld u w, Has w c)

class Eq a => ActionArgs a where
    unboxArguments :: [Entity] -> Maybe a
    defaultArguments :: a
    isBad :: a -> Bool
    isBad = (== defaultArguments)

class RulebookArgs a where
    defaultRulebookArguments :: a

instance ActionArgs Entity where
    unboxArguments [a] = Just a
    unboxArguments _ = Nothing
    defaultArguments = badArguments

instance ActionArgs () where
    
    unboxArguments [] = Just ()
    unboxArguments _ = Nothing
    defaultArguments = ()
    isBad _ = False

getDescription :: w -> Object -> Text
getDescription w o = case _description o of
    PlainDescription t -> t
    DynamicDescription p -> p w (_objectID o)

blankRulebook :: Text -> UncompiledRulebook w ()
blankRulebook n = Rulebook n (const ()) []

blankRulebookVars :: Text -> r -> UncompiledRulebook w r
blankRulebookVars n r = Rulebook n (const r) []

blankGameInfo :: GameInfo w
blankGameInfo = GameInfo "" Nothing blankMessageBuffer 1 Map.empty Map.empty Map.empty NeverAbbreviatedRoomDescriptions False

globalComponent :: Entity
globalComponent = 0

badArguments :: Entity
badArguments = -100

missingThing :: Entity
missingThing = -1

missingPlayer :: Entity
missingPlayer = -2

missingRoom :: Entity
missingRoom = -3

setTitle :: HasGameInfo a w  => Text -> System a ()
setTitle t = gameInfo . title .= t

newEntity :: HasGameInfo u w => System u Entity
newEntity = gameInfo . entityCounter <<%= (+1)

addComponent :: HasComponent u w c => Entity -> c -> System u ()
addComponent e c = zoom world (store (Proxy :: Proxy a) . at e ?= c)

component :: HasComponent u w c => Proxy c -> Entity -> Lens' u (Maybe c)
component p e = lens (\w -> getComponent w p e) (\w b -> case b of
    Just mb -> setComponent w p e mb
    Nothing -> w)

getComponent :: HasComponent u w c => u -> Proxy c -> Entity -> Maybe c
getComponent w t e = IM.lookup e $ w ^. world . store t

getComponentST :: HasComponent u w c => Proxy c -> Entity -> System u (Maybe c)
getComponentST t e = do
    g <- get
    let c1 = getComponent g t e
    return c1

--setComponent :: Has w a => w -> Proxy a -> Entity -> a -> w
setComponent :: HasComponent u w c => u -> Proxy c -> Entity -> c -> u
setComponent w p e v = w & world . store p . at e ?~ v

makeObject :: (HasMessageBuffer w, HasComponent u w Object) => Text -> Description -> System u Entity
makeObject n d = zoom world (do
    e <- newEntity
    addComponent e (Object n d e)
    sayDbgLn $ "made an object called " <> n <> " with id " <> show e
    return e)

makeRule :: Text -> RuleEvaluation w -> (Text, RuleEvaluation (w, ()))
makeRule n f = makeRuleWithArgs n (zoom _1 f)

makeRuleWithArgs :: Text -> RuleEvaluation (w, r) -> (Text, RuleEvaluation (w, r))
makeRuleWithArgs = (,)

getWorld :: System (w, r) w
getWorld = fst <$> get

tryAction :: (HasWorld' w, HasMessageBuffer w) => Text -> [Entity] -> State w (Maybe Bool)
tryAction action args = do
    w <- get
    let ac = Map.lookup action $ w ^. world . gameInfo . actions
    when (isNothing ac) (sayDbgLn $ "couldn't find the action " <> action)
    maybe (return $ Just False) (\(CompiledAction e) -> e args) ac 

mapObjects :: HasComponent u w a => Proxy a -> (a -> System u a) -> System u ()
mapObjects c1 func = do
    g <- get
    let evs = IM.assocs (g ^. world . store c1)
    mapM_ (\(k, v) -> do
        res <- func v
        world . store c1 . at k ?= res) evs

mapObjects2 :: (HasComponent u w a, HasComponent u w b) => (Proxy a, Proxy b) -> (a -> b -> System u (a, b)) 
                    ->  System u ()
mapObjects2 (c1, c2) func = do
    g <- get
    let evs = IM.assocs (g ^. world . store c1)
    mapM_ (\(k, v) -> do
        comp2 <- getComponentST c2 k
        whenJust comp2 (\co2 -> do
                        (r1, r2) <- func v co2
                        world . store c1 . at k ?= r1
                        world . store c2 . at k ?= r2)) evs

makeActivity :: Text -> RuleEvaluation (w, (p, ())) -> UncompiledActivity w () p
makeActivity n r = Activity n (const ()) (blankRulebookVars ("before " <> n)) 
                    (\r1 -> (blankRulebookVars ("for " <> n) r1) 
                        {
                            _rules = [("", r)]
                        })
                            (blankRulebookVars ("after " <> n))

makeBlankRule :: Text -> (Text, RuleEvaluation (w, r))
makeBlankRule n = makeRuleWithArgs n (return Nothing)

--makeAction n r = Action n [n] (const blankRulebook)
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

isComponent :: HasComponent u w c => u -> Proxy c -> Entity -> Bool
isComponent u p e = isJust $ getComponent u p e

makeLenses ''UncompiledRulebook
makeLenses ''Object