-- |
-- Copyright: (c) 2020 Avery
-- SPDX-License-Identifier: MIT
-- Maintainer: Avery <thecommunistduck@hotmail.co.uk>
--
-- Yet another interactive fiction library.
module Yaifl.Common
  ( Entity,
    Store,
    emptyStore,
    RuleOutcome,
    RuleEvaluation,
    World (..),
    Env(..),
    HasStore,
    store,
    firstRoom,
    ThereIs,
    defaultObject,
    Deletable,
    deleteObject,
    Rulebook(..)
    , Rule(..)
    , PlainRule
    , PlainRulebook
    , GameData (..),
    RuleVarsT,
    unwrapRuleVars,
    title,
    intersectStore,
    intersectStore2,
    intersectStore3,
    setStore,
    setStore2,
    setStore3,
    storeLens,
    storeLens2,
    storeLens3,
    isX,
    say,
    sayLn,
    sayIf,
    entityCounter,
    gameWorld,
    rulebookStore,
    WithGameLog,
    rulebookName,
    messageBuffer,
    buffer,
    setStyle,
    getComponent,
    uniqueComponent,
    withEntityIDBlock,
    defaultPlayerID,
    newEntity,
    component,
    adjustComponent,
    setComponent,
    deleteComponent,
    defaultVoidRoom,
    tryAction,
    Action(..),
    ActionArgs(..),
    {-
    , World(..)
    , Action(..)
    , worldToMapStore
    , newEntity
    , HasStore
    , store
    , EntityProducer
    , entityCounter
    , getEntityCounter
    , setEntityCounter
    , withEntityIDBlock
    , getStore
    , getComponent
    , getComponent'
    , setComponent
    , addComponent
    , adjustComponent
    , isX

    , HasGameSettings'

    , globalComponent
    , firstRoom
    , SemWorld
    , SemWorldList
    -}
    blankGameData)
where

import Colog (LogAction, HasLog (..), LogAction)
import Colog.Message
import Colog.Monad
import qualified Data.IntMap as IM
import qualified Data.Map.Strict as Map
import Yaifl.Prelude
import Data.Functor.Apply (Apply(..))
import Control.Monad.State.Strict
import qualified Data.Text.Prettyprint.Doc     as PP
import qualified Data.Text.Prettyprint.Doc.Render.Terminal as PPTTY

{- TYPES -}

-- | An Entity is just an ID that is loosely associated with components.
type Entity = Int

uniqueComponent :: Entity
uniqueComponent = 0

defaultPlayerID :: Entity
defaultPlayerID = -10

defaultdirectionBlockIDs :: Entity
defaultdirectionBlockIDs = -100

defaultVoidRoom :: Entity
defaultVoidRoom = -20

-- | Store a is a container of components of type a, indexed by entity ID.
-- in an ideal world (TODO? it'd have multiple different types of stores)
type Store a = IM.IntMap a
type RulebookStore w m = Map.Map Text (RuleEvaluation w m)
type ActionStore w m = Map.Map Text (ActionEvaluation w m)
-- | A store with nothing in it
emptyStore :: Store a
emptyStore = IM.empty

-- | rules either give no outcome, true, or false.
type RuleOutcome = Bool

type RuleEvaluation w m = World w m (Maybe RuleOutcome)
newtype ActionEvaluation w m = CompiledAction ([Entity] -> World w m RuleOutcome)

class HasStore w c where
    store :: Lens' w (Store c)
    
newtype Env m = Env { _envLogAction :: LogAction m Message } 

data GameData w m = GameData
  { _gameWorld :: w,
    _title :: Text,
    _firstRoom :: Maybe Entity,
    _entityCounter :: Entity,
    _messageBuffer :: MessageBuffer,
    _actionProcessingRulebook :: forall v. Action w v m -> [Entity] -> Rulebook w v m RuleOutcome,
    _rulebookStore :: RulebookStore w m,
    _actionStore :: ActionStore w m
  }

type StyledDoc = PP.Doc PPTTY.AnsiStyle
data MessageBuffer = MessageBuffer
    {
        _buffer :: [StyledDoc],
        _msgStyle :: Maybe PPTTY.AnsiStyle
    } deriving Show

newtype World w m a = World { unwrapWorld :: ReaderT (Env (World w m)) (StateT (GameData w m) m) a} 
    deriving newtype (Functor, Applicative, Monad,
        MonadState (GameData w m),
        MonadReader (Env (World w m)),
        MonadIO) 

instance HasLog (Env m) Message m where
    getLogAction :: Env m -> LogAction m Message
    getLogAction = _envLogAction
    {-# INLINE getLogAction #-}

    setLogAction :: LogAction m Message -> Env m -> Env m
    setLogAction newLogAction env = env { _envLogAction = newLogAction }
    {-# INLINE setLogAction #-}

type WithGameLog w m = (WithLog (Env (World w m)) Message (World w m), Monad m)


instance MonadTrans (World w) where
    lift = World . lift . lift
{-
instance MonadWorld m => MonadWorld (LoggerT msg m) where
    showWorld = lift showWorld

instance MonadWorld m => MonadWorld (ReaderT r m) where
    showWorld = lift showWorld
-}
instance MonadTrans (RuleVarsT v) where
    lift = RuleVarsT . lift

instance MonadReader r m => MonadReader r (RuleVarsT v m) where
    ask = lift ask 
    local l m = RuleVarsT $ mapStateT (local l) (unwrapRuleVars m)

blankGameData :: w -> (w -> w) -> (forall v. Action w v m -> [Entity] -> Rulebook w v m RuleOutcome) -> GameData w m
blankGameData w rbs a = GameData (rbs w) "untitled" Nothing 0 (MessageBuffer [] Nothing) a Map.empty Map.empty

--this is some stackoverflow black fing magic
--but idk if it's actually any easier to follow than the intersection one.
fanoutTraversal2 :: Traversal' s a -> Traversal' s b -> Traversal' s (a, b)
fanoutTraversal2 t1 t2 fab s =
  maybe (pure s) (fmap update . fab) mv
  where
    mv = liftA2 (,) (s ^? t1) (s ^? t2)
    update (c, d) = s & t1 .~ c & t2 .~ d

fanoutTraversal3 :: Traversal' s a -> Traversal' s b -> Traversal' s c -> Traversal' s ((a, b), c)
fanoutTraversal3 t1 t2 = fanoutTraversal2 (fanoutTraversal2 t1 t2)

intersectStore :: HasStore w a => (a -> b) -> w -> Store b
intersectStore f w = f <$> w ^. store

intersectStore2 :: (HasStore w a1, HasStore w a2) => (a1 -> a2 -> b) -> w -> Store b
intersectStore2 f w = intersectStore f w <.> w ^. store

intersectStore3 :: (HasStore w a1, HasStore w a2, HasStore w a) => (a1 -> a2 -> a -> b) -> w -> Store b
intersectStore3 f w = intersectStore2 f w <.> w ^. store

setStore :: forall c a w. HasStore w c => (a -> c) -> w -> Store a -> w
setStore f w m = w & store %~ IM.union (f <$> m)

setStore2 :: (HasStore w c1, HasStore w c2) => (a -> c1) -> (a -> c2) -> w -> Store a -> w
setStore2 f f2 w m = setStore f2 (setStore f w m) m

setStore3 :: (HasStore w c, HasStore w c1, HasStore w c2) => (a -> c1) -> (a -> c2) -> (a -> c) -> w -> Store a -> w
setStore3 f f2 f3 w m = setStore f3 (setStore2 f f2 w m) m

storeLens :: HasStore w a => (a -> b) -> (b -> a) -> Lens' w (Store b)
storeLens f a = lens (intersectStore f) (setStore a)

storeLens2 :: (HasStore w a, HasStore w c) => (a -> c -> b) -> (b -> a) -> (b -> c) -> Lens' w (Store b)
storeLens2 f a a2 = lens (intersectStore2 f) (setStore2 a a2)

storeLens3 :: (HasStore w a, HasStore w c, HasStore w d) => (a -> c -> d -> b) -> (b -> a) -> (b -> c) -> (b -> d) -> Lens' w (Store b)
storeLens3 f a a2 a3 = lens (intersectStore3 f) (setStore3 a a2 a3)

class ThereIs t where
    defaultObject :: Entity -> t

class Deletable w t where
    deleteObject :: (HasStore w t, Monad m) => Entity -> World w m ()

newtype RuleVarsT v m a = RuleVarsT { unwrapRuleVars :: StateT v m a } deriving (Functor, Applicative, Monad)

data Rule w v m a where
    Rule :: WithLog env Message (World w m) => Text -> World w m (Maybe a) -> Rule w () m a
    RuleWithVariables :: WithLog env Message (World w m) => Text -> RuleVarsT v (World w m) (Maybe a) -> Rule w v m a

-- | a rulebook runs in a monadic context m with rulebook variables v and returns a value r, which is normally a success/fail
data Rulebook w v m a where
    Rulebook :: Text -> Maybe a -> [Rule w () m a] -> Rulebook w () m a
    RulebookWithVariables :: Text -> Maybe a -> World w m (Maybe v) -> [Rule w v m a] -> Rulebook w v m a

rulebookName :: Rulebook w v m a -> Text
rulebookName (Rulebook t _ _) = t
rulebookName (RulebookWithVariables t _ _ _) = t
type PlainRule w m = Rule w () m RuleOutcome
type PlainRulebook w m = Rulebook w () m RuleOutcome

data Action w v m = Action
    { _actionName          :: Text
    , _understandAs        :: [Text]
    , _setActionVariables  :: v -> Rulebook w v m v
    , _beforeActionRules   :: Rulebook w v m RuleOutcome
    , _checkActionRules    :: Rulebook w v m RuleOutcome
    , _carryOutActionRules :: Rulebook w v m RuleOutcome
    , _reportActionRules   :: Rulebook w v m RuleOutcome
    }

class Eq a => ActionArgs a where
    unboxArguments :: [Entity] -> Maybe a

-- | just default arguments
class RulebookArgs a where
    defaultArguments :: a

instance ActionArgs Entity where
    unboxArguments [a] = Just a
    unboxArguments _   = Nothing

instance ActionArgs () where
    unboxArguments [] = Just ()
    unboxArguments _  = Nothing

makeLenses ''MessageBuffer
makeLenses ''GameData

getComponent :: forall c w m. (Monad m, HasStore w c) => Entity -> World w m (Maybe c)
getComponent e = use $ gameWorld . store . at e

setComponent :: forall c w m. (Monad m, HasStore w c) => Entity -> c -> World w m ()
setComponent e v = gameWorld . store . at e ?= v

adjustComponent :: forall c w m. (Monad m, HasStore w c) => Entity -> (c -> c) -> World w m ()
adjustComponent e f = do
    c <- getComponent e
    whenJust c (setComponent e . f)

deleteComponent :: forall c m w. (Monad m, HasStore w c) => Entity -> World w m ()
deleteComponent e = do
    gameWorld . store @w @c . at e .= Nothing
    pass

component :: forall c w. HasStore w c => Entity -> Lens' w (Maybe c)
component e = lens (\w -> w ^. store . at e ) sc where
    sc :: (w -> Maybe c -> w)
    sc w v = set (store @w @c . at e) v w

isX :: (Eq d, Monad m, HasStore w c) => d -> (c -> d) -> Entity -> World w m Bool
isX p recordField e = fmap (\t -> Just p == (recordField <$> t)) (getComponent e)

sayInternal :: WithGameLog w m => StyledDoc -> World w m ()
sayInternal a = do
    w <- use $ messageBuffer . msgStyle
    messageBuffer . buffer %= (:) (maybe id PP.annotate w a)

say :: WithGameLog w m => Text -> World w m ()
say a = sayInternal (PP.pretty a)

sayLn ::  WithGameLog w m => Text -> World w m ()
sayLn a = say (a <> "\n")

sayIf :: WithGameLog w m => Bool -> Text -> World w m ()
sayIf iff a = when iff (say a)

setStyle :: WithGameLog w m => Maybe PPTTY.AnsiStyle -> World w m ()
setStyle sty = messageBuffer . msgStyle .= sty

withEntityIDBlock :: Monad m => Entity -> World w m a -> World w m a
withEntityIDBlock idblock x = do
    ec <- setEntityCounter idblock
    v  <- x
    _  <- setEntityCounter ec
    return v

setEntityCounter :: Monad m => Entity -> World w m Entity
setEntityCounter e = do
    ec <- use entityCounter
    entityCounter .= e
    return ec

newEntity :: (Monad m) => World w m Entity
newEntity = do entityCounter <<%= (+ 1)



tryAction :: Monad m => Text -> [Entity] -> World w m Bool
tryAction action args = do
    ac <- use $ actionStore . at action
    --v <- use actionProcessingRulebook
    maybe (do
        logError $ "Couldn't find the action called " <> action
        return False) (\(CompiledAction a) -> a args) ac

{-

data Activity w v = Activity
    { _activityName :: Text
    , _initActivity :: [Entity] -> SemWorld w (Maybe v)
    , _beforeRules  :: Rulebook w v (v, RuleOutcome)
    , _forRules     :: Rulebook w v (v, RuleOutcome)
    , _afterRules   :: Rulebook w v (v, RuleOutcome)
    }

makeLenses ''Rulebook

-}
