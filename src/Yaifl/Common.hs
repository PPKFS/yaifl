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
    showWorld,
    MonadWorld,
    Env(..),
    HasStore,
    store,
    getStore,
    {-
    , Rulebook(..)
    , Rule(..)
    , PlainRule
    , PlainRulebook
    -}
    GameData (..),
    title,
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
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as Map
import Yaifl.Prelude
import qualified Data.Text.IO as TIO

{- TYPES -}

-- | An Entity is just an ID that is loosely associated with components.
type Entity = Int

-- | Store a is a container of components of type a, indexed by entity ID.
-- in an ideal world (TODO? it'd have multiple different types of stores)
type Store a = IM.IntMap a

class HasStore w c where
    store :: Lens' w (Store c)
    getStore :: w -> Store c
newtype Env m = Env { _envLogAction :: LogAction m Message } 

instance HasLog (Env m) Message m where
    getLogAction :: Env m -> LogAction m Message
    getLogAction = _envLogAction
    {-# INLINE getLogAction #-}

    setLogAction :: LogAction m Message -> Env m -> Env m
    setLogAction newLogAction env = env { _envLogAction = newLogAction }
    {-# INLINE setLogAction #-}

newtype World w m a = World { unwrapWorld :: ReaderT (Env (World w m)) (StateT (GameData w (World w m)) m) a} 
    deriving newtype (Functor, Applicative, Monad,
        MonadState (GameData w (World w m)),
        MonadReader (Env (World w m)),
        MonadIO) 

class Monad m => MonadWorld m where
    showWorld :: m Text

instance MonadTrans (World w) where
    lift = World . lift . lift

instance (Monad m, Show w) => MonadWorld (World w m) where
    showWorld = show <$> get

instance MonadWorld m => MonadWorld (LoggerT msg m) where
    showWorld = lift showWorld

instance MonadWorld m => MonadWorld (ReaderT r m) where
    showWorld = lift showWorld

data GameData w m = GameData
  { _world :: w,
    _title :: Text,
    _firstRoom :: Maybe Entity,
    _entityCounter :: Entity
  } deriving Show

blankGameData :: w -> GameData w m
blankGameData w = GameData w "Untitled" Nothing 0

type RulebookStore m = Map.Map Text (RuleEvaluation m)

-- | A store with nothing in it
emptyStore :: Store a
emptyStore = IM.empty

-- | rules either give no outcome, true, or false.
type RuleOutcome = Bool

type RuleEvaluation m = m (Maybe RuleOutcome)

fanoutTraversal2 :: Traversal' s a -> Traversal' s b -> Traversal' s (a, b)
fanoutTraversal2 t1 t2 fab s =
  maybe (pure s) (fmap update . fab) mv
  where
    mv = liftA2 (,) (s ^? t1) (s ^? t2)
    update (c, d) = s & t1 .~ c & t2 .~ d

fanoutTraversal3 :: Traversal' s a -> Traversal' s b -> Traversal' s c -> Traversal' s ((a, b), c)
fanoutTraversal3 t1 t2 = fanoutTraversal2 (fanoutTraversal2 t1 t2)

makeLenses ''GameData

{-
newtype RuleVarsT v m a = RuleVarsT { unwrapRuleVars :: StateT v m a } deriving (Functor, Applicative, Monad)

data Rule v m a where
    Rule :: Text -> ReaderT (RulebookStore m) m (Maybe a) -> Rule () m a
    RuleWithVariables :: Text -> ReaderT (RulebookStore (RuleVarsT v m)) m (Maybe a) -> Rule v m a

-- | a rulebook runs in a monadic context m with rulebook variables v and returns a value r, which is normally a success/fail
data Rulebook v m a where
    Rulebook :: Text -> Maybe a -> [Rule () m a] -> Rulebook () m a
    RulebookWithVariables :: Text -> Maybe a -> ReaderT (RulebookStore m) m (Maybe v) -> [Rule v m a] -> Rulebook v m a

type PlainRule m = Rule () m RuleOutcome
type PlainRulebook m = Rulebook () m RuleOutcome
-}

{-
rules :: Lens' (Rulebook w v r) [Rule w v r]
rules = lensVL $ \f s -> case s of
  Rulebook n d r -> fmap (Rulebook n d) (f r)
  RulebookWithVariables n d i r -> fmap (RulebookWithVariables n d i) (f r)
-}
{-
data World w m k where
    GetStore :: HasStore w c => Proxy c -> World w m (Store c)
    SetComponent :: HasStore w c => Proxy c -> Entity -> c -> World w m ()
    GetEntityCounter :: World w m Entity
    SetEntityCounter :: Entity -> World w m Entity

makeSem ''World

worldToMapStore :: EntityProducer w => Sem (World w ': r) a -> Sem (State w ': r) a
worldToMapStore = reinterpret $ \case
    GetStore p -> use $ store p
    SetComponent p e v -> store p % at' e ?= v
    GetEntityCounter   -> use entityCounter
    SetEntityCounter e -> do
        ec <- use entityCounter
        entityCounter .= e
        return ec

getComponent :: (HasStore w c, Members (SemWorldList w) r) => Proxy c -> Entity -> Sem r (Maybe c)
getComponent p e = do
    s <- getStore p
    return $ IM.lookup e s

getComponent' :: (HasStore w c, Members (SemWorldList w) r) => Proxy c -> Entity -> Sem r c
getComponent' p e = do
    s <- getStore p
    let res = maybeToRight "Somehow couldn't find the component" (IM.lookup e s)
    when (isLeft res) (
        logMsg Error $ "Attempted to find a component of " <>  show e <> " but couldn't find it")
    fromEither res

adjustComponent :: (HasStore w c, Members (SemWorldList w) r) => Proxy c -> Entity -> (c -> c) -> Sem r ()
adjustComponent p e f = do
    c <- getComponent p e
    whenJust c (setComponent p e . f)

type SemWorldList w = '[World w, Log, State LoggingContext, Say, State (GameSettings w), Error Text]

type SemWorld w r = Sem (SemWorldList w) r

data Activity w v = Activity
    { _activityName :: Text
    , _initActivity :: [Entity] -> SemWorld w (Maybe v)
    , _beforeRules  :: Rulebook w v (v, RuleOutcome)
    , _forRules     :: Rulebook w v (v, RuleOutcome)
    , _afterRules   :: Rulebook w v (v, RuleOutcome)
    }

data Action w v = Action
    { _actionName          :: Text
    , _understandAs        :: [Text]
    , _setActionVariables  :: [Entity] -> Rulebook w v (Maybe v)
    , _beforeActionRules   :: Rulebook w v (v, RuleOutcome)
    , _checkActionRules    :: Rulebook w v (v, RuleOutcome)
    , _carryOutActionRules :: Rulebook w v (v, RuleOutcome)
    , _reportActionRules   :: Rulebook w v RuleOutcome
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

withEntityIDBlock :: Member (World w) r => Entity -> Sem r a -> Sem r a
withEntityIDBlock idblock x = do
    ec <- setEntityCounter idblock
    v  <- x
    _  <- setEntityCounter ec
    return v

newEntity :: Member (World w) r => Sem r Entity
newEntity = do
    ec <- getEntityCounter
    _  <- setEntityCounter (ec + 1)
    return ec

-- TODO: check it doesn't have the component to begin with
addComponent :: (Member (World w) r, HasStore w c) => Entity -> c -> Sem r ()
addComponent = setComponent (Proxy :: Proxy c)

isX :: (Eq b, Members (SemWorldList w) r, HasStore w c) => b -> (c -> b) -> Proxy c -> Entity -> Sem r Bool
isX p recordField comp e =
    (Just p ==) . fmap recordField <$> getComponent comp e

makeLenses ''Rulebook

-}
