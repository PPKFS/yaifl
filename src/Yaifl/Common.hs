{- |
 Copyright: (c) 2020 Avery
 SPDX-License-Identifier: MIT
 Maintainer: Avery <thecommunistduck@hotmail.co.uk>

 Yet another interactive fiction library.
-}
module Yaifl.Common (
    Entity,
    Store,
    emptyStore,
    RuleOutcome,
    RuleEvaluation,
    World (..),
    Env (..),
    RoomDescriptions (..),
    roomDescriptions,
    HasStore,
    store,
    firstRoom,
    Deletable,
    deleteObject,
    Rulebook (..),
    Rule (..),
    PlainRule,
    PlainRulebook,
    GameData (..),
    RuleVarsT (..),
    HasID,
    getID,
    title,
    darknessWitnessed,
    intersectStore,
    intersectStore2,
    intersectStore3,
    setStore,
    setStore2,
    setStore3,
    storeLens,
    storeLens2,
    storeLens3,
    storeLens4,
    storeLens5,
    isX,
    say,
    sayLn,
    sayIf,
    entityCounter,
    gameWorld,
    rulebookStore,
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
    mentionedThings,
    localePriorities,
    MonadWorld,
    liftWorld,
    defaultVoidRoom,
    Action (..),
    ActionEvaluation (..),
    actionStore,
    BoxedRulebook (..),
    BoxedAction (..),
    actionProcessing,
    getActor,
    currentActionVars,
    getRulebookVariables,
    modifyRulebookVariables,
    activityStore,
    BoxedActivity (..),
    WithGameData,
    Activity (..),
    localeData,
    setLocalePriority,
    clearLocale,
    mostRecentRoom,
    foreachObject,
    getForeachObject,
    defaultdirectionBlockIDs,
    modifyObject,
    getComponent',
    blankGameData,
) where

import Colog (HasLog (..), LogAction)
import Colog.Message
import Colog.Monad
import Control.Monad.State.Strict (mapStateT)
import Data.Functor.Apply (Apply (..))
import qualified Data.IntMap as IM
import qualified Data.IntMap.Strict as DIM
import qualified Data.Map.Strict as Map
import qualified Data.Set as DS
import qualified Data.Text.Prettyprint.Doc as PP
import qualified Data.Text.Prettyprint.Doc.Render.Terminal as PPTTY
import Yaifl.Prelude

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

{- | Store a is a container of components of type a, indexed by entity ID.
 in an ideal world (TODO? it'd have multiple different types of stores)
-}
type Store a = IM.IntMap a

type RulebookStore w = Map.Map Text (BoxedRulebook w RuleOutcome)

-- | A store with nothing in it
emptyStore :: Store a
emptyStore = IM.empty

-- | rules either give no outcome, true, or false.
type RuleOutcome = Bool

newtype ActionEvaluation w = CompiledAction ([Entity] -> World w RuleOutcome)

class HasStore w c where
    store :: Lens' w (Store c)

newtype Env m = Env {_envLogAction :: LogAction m Message}

data GameData w = GameData
    { _gameWorld :: w
    , _title :: Text
    , _firstRoom :: Maybe Entity
    , _entityCounter :: Entity
    , _messageBuffer :: MessageBuffer
    , _rulebookStore :: RulebookStore w
    , _actionStore :: Map.Map Text (BoxedAction w)
    , _activityStore :: Map.Map Text (BoxedActivity w)
    , _actionProcessing :: BoxedAction w -> [Entity] -> World w RuleOutcome
    , _roomDescriptions :: RoomDescriptions
    , _darknessWitnessed :: Bool
    , _currentActionVars :: (Entity, [Entity])
    , _localeData :: LocaleData
    , _mostRecentRoom :: Maybe Entity
    }

data LocaleData = LocaleData
    { _localePriorities :: DIM.IntMap Int
    , _mentionedThings :: Set Entity
    }

type StyledDoc = PP.Doc PPTTY.AnsiStyle
data MessageBuffer = MessageBuffer
    { _buffer :: [StyledDoc]
    , _msgStyle :: Maybe PPTTY.AnsiStyle
    }
    deriving (Show)

newtype World w a = World {unwrapWorld :: ReaderT (Env (World w)) (StateT (GameData w) IO) a}
    deriving newtype
        ( Functor
        , Applicative
        , Monad
        , MonadState (GameData w)
        , MonadReader (Env (World w))
        , MonadIO
        )

class MonadState (GameData w) m => HasID w e m where
    getID :: e -> m Entity

instance MonadState (GameData w) m => HasID w Entity m where
    getID = return
instance HasLog (Env m) Message m where
    getLogAction :: Env m -> LogAction m Message
    getLogAction = _envLogAction
    {-# INLINE getLogAction #-}

    setLogAction :: LogAction m Message -> Env m -> Env m
    setLogAction newLogAction env = env{_envLogAction = newLogAction}
    {-# INLINE setLogAction #-}

class Monad m => MonadWorld w m where
    liftWorld :: World w a -> m a
    liftLogWorld :: LogAction (World w) Message -> LogAction m Message

instance MonadWorld w (World w) where
    liftWorld = id
    liftLogWorld = id

type WithGameData w m = (WithLog (Env (World w)) Message m, MonadState (GameData w) m, MonadWorld w m)

instance MonadTrans (RuleVarsT v) where
    lift = RuleVarsT . lift

instance MonadReader r m => MonadReader r (RuleVarsT v m) where
    ask = lift ask
    local l m = RuleVarsT $ mapStateT (local l) (unwrapRuleVars m)

type RuleEvaluation w v = RuleVarsT v (World w) (Maybe RuleOutcome)

data RoomDescriptions = SometimesAbbreviatedRoomDescriptions | AbbreviatedRoomDescriptions | NoAbbreviatedRoomDescriptions deriving (Eq, Show)

--setLogAction newLogAction env = env { _envLogAction = runLoggerT . newLogAction }
blankGameData :: w -> (w -> w) -> GameData w
blankGameData w rbs = GameData (rbs w) "untitled" Nothing 0 (MessageBuffer [] Nothing) Map.empty Map.empty Map.empty blankActionProcessor NoAbbreviatedRoomDescriptions False (-1, []) (LocaleData DIM.empty DS.empty) Nothing

blankActionProcessor :: BoxedAction w -> [Entity] -> World w RuleOutcome
blankActionProcessor _ _ = do
    logError "No action processing rulebook given"
    return False

{-
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
-}
intersectStore :: HasStore w a => (a -> b) -> w -> Store b
intersectStore f w = f <$> w ^. store

intersectStore2 :: (HasStore w a1, HasStore w a2) => (a1 -> a2 -> b) -> w -> Store b
intersectStore2 f w = intersectStore f w <.> w ^. store

intersectStore3 :: (HasStore w a1, HasStore w a2, HasStore w a) => (a1 -> a2 -> a -> b) -> w -> Store b
intersectStore3 f w = intersectStore2 f w <.> w ^. store

intersectStore4 :: (HasStore w a1, HasStore w a2, HasStore w a3, HasStore w a) => (a1 -> a2 -> a3 -> a -> b) -> w -> Store b
intersectStore4 f w = intersectStore3 f w <.> w ^. store

intersectStore5 :: (HasStore w a1, HasStore w a2, HasStore w a3, HasStore w a4, HasStore w a) => (a1 -> a2 -> a3 -> a4 -> a -> b) -> w -> Store b
intersectStore5 f w = intersectStore4 f w <.> w ^. store

setStore :: forall c a w. HasStore w c => (a -> c) -> w -> Store a -> w
setStore f w m = w & store %~ IM.union (f <$> m)

setStore2 :: (HasStore w c1, HasStore w c2) => (a -> c1) -> (a -> c2) -> w -> Store a -> w
setStore2 f f2 w m = setStore f2 (setStore f w m) m

setStore3 :: (HasStore w c, HasStore w c1, HasStore w c2) => (a -> c1) -> (a -> c2) -> (a -> c) -> w -> Store a -> w
setStore3 f f2 f3 w m = setStore f3 (setStore2 f f2 w m) m

setStore4 :: (HasStore w c, HasStore w c1, HasStore w c2, HasStore w c3) => (a -> c1) -> (a -> c2) -> (a -> c3) -> (a -> c) -> w -> Store a -> w
setStore4 f f2 f3 f4 w m = setStore f4 (setStore3 f f2 f3 w m) m

setStore5 :: (HasStore w c, HasStore w c1, HasStore w c2, HasStore w c3, HasStore w c4) => (a -> c1) -> (a -> c2) -> (a -> c3) -> (a -> c) -> (a -> c4) -> w -> Store a -> w
setStore5 f f2 f3 f4 f5 w m = setStore f5 (setStore4 f f2 f3 f4 w m) m

storeLens :: HasStore w a => (a -> b) -> (b -> a) -> Lens' w (Store b)
storeLens f a = lens (intersectStore f) (setStore a)

storeLens2 :: (HasStore w a, HasStore w c) => (a -> c -> b) -> (b -> a) -> (b -> c) -> Lens' w (Store b)
storeLens2 f a a2 = lens (intersectStore2 f) (setStore2 a a2)

storeLens3 :: (HasStore w a, HasStore w c, HasStore w d) => (a -> c -> d -> b) -> (b -> a) -> (b -> c) -> (b -> d) -> Lens' w (Store b)
storeLens3 f a a2 a3 = lens (intersectStore3 f) (setStore3 a a2 a3)

storeLens4 :: (HasStore w a, HasStore w c, HasStore w d, HasStore w e) => (a -> c -> d -> e -> b) -> (b -> a) -> (b -> c) -> (b -> d) -> (b -> e) -> Lens' w (Store b)
storeLens4 f a a2 a3 a4 = lens (intersectStore4 f) (setStore4 a a2 a3 a4)

storeLens5 :: (HasStore w a, HasStore w c, HasStore w d, HasStore w e, HasStore w f) => (a -> c -> d -> e -> f -> b) -> (b -> a) -> (b -> c) -> (b -> d) -> (b -> e) -> (b -> f) -> Lens' w (Store b)
storeLens5 f a a2 a3 a4 a5 = lens (intersectStore5 f) (setStore5 a a2 a3 a4 a5)


class Deletable w t where
    deleteObject :: (WithGameData w m, HasStore w t) => Entity -> m ()

newtype RuleVarsT v m a = RuleVarsT {unwrapRuleVars :: StateT v m a} deriving (Functor, Applicative, Monad)

getRulebookVariables :: Monad m => RuleVarsT v m v
getRulebookVariables = RuleVarsT get

modifyRulebookVariables :: Monad m => (s -> s) -> RuleVarsT s m ()
modifyRulebookVariables e = RuleVarsT (modify e)

instance MonadState (GameData w) m => MonadState (GameData w) (RuleVarsT v m) where
    state = lift . state

instance (Monad m, MonadWorld w m) => MonadWorld w (RuleVarsT v m) where
    liftWorld = lift . liftWorld
    liftLogWorld = error "log error"

instance (MonadWorld w m, HasLog (Env (World w)) Message m) => HasLog (Env (World w)) Message (RuleVarsT v m) where
    getLogAction e = liftLogAction (liftLogWorld $ _envLogAction e)
    overLogAction = error "log error"

data Rule w v a where
    Rule :: Text -> World w (Maybe a) -> Rule w () a
    RuleWithVariables :: Text -> RuleVarsT v (World w) (Maybe a) -> Rule w v a

-- | a rulebook runs in a monadic context m with rulebook variables v and returns a value r, which is normally a success/fail
data Rulebook w v a where
    Rulebook :: Text -> Maybe a -> [Rule w () a] -> Rulebook w () a
    RulebookWithVariables :: Text -> Maybe a -> World w (Maybe v) -> [Rule w v a] -> Rulebook w v a

data BoxedRulebook w a where
    BoxedRulebook :: Rulebook w v a -> BoxedRulebook w a

data BoxedAction w where
    BoxedAction :: Show v => Action w v -> BoxedAction w

data BoxedActivity w where
    BoxedActivity :: Show v => Activity w v -> BoxedActivity w
rulebookName :: Rulebook w v a -> Text
rulebookName (Rulebook t _ _) = t
rulebookName (RulebookWithVariables t _ _ _) = t

type PlainRule w m = Rule w () RuleOutcome
type PlainRulebook w m = Rulebook w () RuleOutcome

data Action w v where
    Action ::
        { _actionName :: Text
        , _understandAs :: [Text]
        , _appliesTo :: Int -> Bool
        , _setActionVariables :: [Entity] -> Rulebook w () v
        , _beforeActionRules :: v -> Rulebook w v RuleOutcome
        , _checkActionRules :: v -> Rulebook w v RuleOutcome
        , _carryOutActionRules :: v -> Rulebook w v RuleOutcome
        , _reportActionRules :: v -> Rulebook w v RuleOutcome
        } ->
        Action w v

data Activity w v = Activity
    { _activityName :: Text
    , _activityappliesTo :: Int -> Bool
    , _setActivityVariables :: [Entity] -> Rulebook w () v
    , _beforeRules :: v -> Rulebook w v RuleOutcome
    , _forRules :: v -> Rulebook w v RuleOutcome
    , _afterRules :: v -> Rulebook w v RuleOutcome
    }

makeLenses ''MessageBuffer
makeLenses ''GameData
makeLenses ''LocaleData

getActor :: WithGameData w m => m Entity
getActor = use $ currentActionVars . _1
getComponent :: forall c w m. (MonadState (GameData w) m, HasStore w c) => Entity -> m (Maybe c)
getComponent e = use $ gameWorld . store . at e

getComponent' :: forall c w m. (MonadState (GameData w) m, HasStore w c) => Entity -> m c
getComponent' e =
    use (gameWorld . store . at e) >>= \case
        Nothing -> error $ "Failed component lookup for ID " <> show e
        Just x -> return x

setComponent :: forall c w m. (WithGameData w m, HasStore w c) => Entity -> c -> m ()
setComponent e v = gameWorld . store . at e ?= v

adjustComponent :: forall c w m. (WithGameData w m, HasStore w c) => Entity -> (c -> c) -> m ()
adjustComponent e f = do
    c <- getComponent e
    whenJust c (setComponent e . f)

deleteComponent :: forall c m w. (WithGameData w m, HasStore w c) => Entity -> m ()
deleteComponent e = do
    gameWorld . store @w @c . at e .= Nothing
    pass

component :: forall c w. HasStore w c => Entity -> Lens' w (Maybe c)
component e = lens (\w -> w ^. store . at e) sc
  where
    sc :: (w -> Maybe c -> w)
    sc w v = set (store @w @c . at e) v w

isX :: forall c w d m. (Eq d, MonadState (GameData w) m, HasStore w c) => d -> (c -> d) -> Entity -> m Bool
isX p recordField e = fmap (\t -> Just p == (recordField <$> t)) (getComponent @c @w e)

sayInternal :: WithGameData w m => StyledDoc -> m ()
sayInternal a = do
    w <- use $ messageBuffer . msgStyle
    -- logDebug (T.dropWhileEnd (== '\n') $ show a)
    messageBuffer . buffer %= (:) (maybe id PP.annotate w a)

say :: WithGameData w m => Text -> m ()
say a = sayInternal (PP.pretty a)

sayLn :: WithGameData w m => Text -> m ()
sayLn a = say (a <> "\n")

sayIf :: WithGameData w m => Bool -> Text -> m ()
sayIf iff a = when iff (say a)

setStyle :: (MonadState (GameData w) m) => Maybe PPTTY.AnsiStyle -> m ()
setStyle sty = messageBuffer . msgStyle .= sty

withEntityIDBlock :: (MonadState (GameData w) m) => Entity -> m a -> m a
withEntityIDBlock idblock x = do
    ec <- setEntityCounter idblock
    v <- x
    _ <- setEntityCounter ec
    return v

setEntityCounter :: (MonadState (GameData w) m) => Entity -> m Entity
setEntityCounter e = do
    ec <- use entityCounter
    entityCounter .= e
    return ec

newEntity :: WithGameData w m => m Entity
newEntity = entityCounter <<%= (+ 1)

setLocalePriority :: MonadState (GameData w) m => Int -> Int -> m ()
setLocalePriority e p = localeData . localePriorities . at e ?= p

-- TODO: clear mentioned flags
clearLocale :: MonadState (GameData w) m => m ()
clearLocale = localeData . localePriorities .= DIM.empty

getForeachObject :: Monad m => ForeachObjectT v m v
getForeachObject = ForeachObjectT get

modifyObject :: Monad m => (s -> s) -> ForeachObjectT s m ()
modifyObject e = ForeachObjectT (modify e)

instance MonadState (GameData w) m => MonadState (GameData w) (ForeachObjectT v m) where
    state = lift . state

instance (Monad m, MonadWorld w m) => MonadWorld w (ForeachObjectT v m) where
    liftWorld = lift . liftWorld
    liftLogWorld = error "log error"

instance (MonadWorld w m, HasLog (Env (World w)) Message m) => HasLog (Env (World w)) Message (ForeachObjectT v m) where
    getLogAction e = liftLogAction (liftLogWorld $ _envLogAction e)
    overLogAction = error "log error"

instance MonadReader r m => MonadReader r (ForeachObjectT v m) where
    ask = lift ask
    local l m = ForeachObjectT $ mapStateT (local l) (unwrapForeachObjectT m)

newtype ForeachObjectT o m a = ForeachObjectT {unwrapForeachObjectT :: StateT o m a} deriving (Functor, Applicative, Monad, MonadTrans)

foreachObject :: (MonadState (GameData w) m) => Lens' w (Store c) -> ForeachObjectT c m a -> m ()
foreachObject st (ForeachObjectT monadLoop) = do
    v <- use $ gameWorld . st
    updatedMap <- mapM (execStateT monadLoop) v
    gameWorld . st .= updatedMap