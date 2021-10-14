import Yaifl.Prelude

{- TYPES -}

-- | Default IDs for some of the 'common' things - a player, directions, an empty room...
defaultPlayerID :: Entity
defaultPlayerID = -10

defaultdirectionBlockIDs :: Entity
defaultdirectionBlockIDs = -100

defaultVoidRoom :: Entity
defaultVoidRoom = -20

-- | A store holds components (i.e. small quantities of related data) of type a.
-- TODO: consider having more efficient storage (e.g. unique)
type RulebookStore w = Map.Map Text (BoxedRulebook w RuleOutcome)

-- | rules either give no outcome, true, or false.
type RuleOutcome = Bool

newtype ActionEvaluation w = CompiledAction ([Entity] -> World w RuleOutcome)

class HasStore w c where
  store :: Lens' w (Store c)

newtype Env m = Env {_envLogAction :: LogAction m Message}

data GameData w = GameData
  { _gameWorld :: w,
    _title :: Text,
    _firstRoom :: Maybe Entity,
    _entityCounter :: Entity,
    _messageBuffer :: MessageBuffer,
    _rulebookStore :: RulebookStore w,
    _actionStore :: Map.Map Text (BoxedAction w),
    _activityStore :: Map.Map Text (BoxedActivity w),
    _actionProcessing :: BoxedAction w -> [Entity] -> World w RuleOutcome,
    _dynamicStrings :: IntMap (Entity -> World w Text),
    _dynamicStringCounter :: Int,
    _roomDescriptions :: RoomDescriptions,
    _darknessWitnessed :: Bool,
    _currentActionVars :: (Entity, [Entity]),
    _localeData :: LocaleData,
    _mostRecentRoom :: Maybe Entity
  }

data LocaleData = LocaleData
  { _localePriorities :: DIM.IntMap Int,
    _mentionedThings :: Set Entity
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

setEntityCounter :: (MonadState (GameData w) m) => Entity -> m Entity
setEntityCounter e = do
  ec <- use entityCounter
  entityCounter .= e
  return ec

-- | Sometimes we require to construct several objects with specific IDs. TODO: maybe remove this?
withEntityIDBlock :: (MonadState (GameData w) m) => Entity -> m a -> m a
withEntityIDBlock idblock x = do
  ec <- setEntityCounter idblock
  v <- x
  _ <- setEntityCounter ec
  return v

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
