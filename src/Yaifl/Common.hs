{-|
Module      : Yaifl.Common
Description : Mostly defining types to be used everywhere and some helper functions.
Copyright   : (c) Avery, 2022
License     : MIT
Maintainer  : ppkfs@outlook.com
Stability   : No
-}
module Yaifl.Common
  (-- * Datatypes
  Entity(..)
  , Store(..)
  , Timestamp(..)
  , NoMissingObjects
  -- * Object querying
  , isThing
  , isRoom
  , HasID(..)
  {-, CanBeAny(..)
  , withoutMissingObjects
  , isBlankDescription

  -- * Lenses

  , objectL
  , containedBy

  -- * World lookups and modifications
  , getGlobalTime
  , tickGlobalTime
  , setTitle
  , newEntityID

  , reifyObject

  , runGame
  , module Yaifl.Types-}
  )
where

import Solitude
import qualified Data.EnumMap.Strict as EM
import qualified Data.IntMap.Strict as IM
import Control.Monad.Except (MonadError)

-- | An 'Entity' is an integer ID that is used to reference between objects.
newtype Entity = Entity
  { unID :: Int
  } deriving stock   (Show, Generic)
    deriving newtype (Eq, Num, Enum, Ord, Real, Integral)

-- | A way to extract an `Entity` from something.
class HasID n where
  getID :: n -> Entity

instance HasID Entity where
  getID = id

-- | This is kind of hacky, but it works: a `Thing` has ID above 0, and a `Room` has a negative ID.
isThing :: 
  (HasID a)
  => a
  -> Bool
isThing a = getID a >= 0

isRoom :: 
  (HasID a)
  => a
  -> Bool
isRoom = not . isThing

-- | A 'Store' is a map from 'Entity's to @a@s.
newtype Store a = Store
  { unStore :: EM.EnumMap Entity a
  } deriving stock   (Generic)
    deriving newtype (Show, Eq, Ord)

-- first let's define our own alterF for EnumMap...
alterEMF
  :: (Functor f, Enum k)
  => (Maybe a -> f (Maybe a))
  -> k
  -> EM.EnumMap k a -> f (EM.EnumMap k a)
alterEMF upd k m = EM.intMapToEnumMap <$> IM.alterF upd (fromEnum k) (EM.enumMapToIntMap m)

-- | alterF wrapper for Store, since it's a wrapper around a wrapper...
alterNewtypeEMF
  :: Functor f
  => Enum k
  => (Maybe a -> f (Maybe a))
  -> k
  -> (nt -> EM.EnumMap k a)
  -> (EM.EnumMap k a -> nt)
  -> nt
  -> f nt
alterNewtypeEMF upd k unwrap wrap' m = wrap' <$> alterEMF upd k (unwrap m)

instance At (Store a) where
  at k = lensVL $ \f -> alterNewtypeEMF f k unStore Store

type instance IxValue (Store a) = a
type instance Index (Store a) = Entity
instance Ixed (Store a)

-- | For now, a timestamp is simply an integer. The timestamp is updated whenever some
-- modification is made to the 'World'; therefore it does not directly correspond to
-- some sort of in-game turn counter. For example, throwing an object would result in
-- multiple timestamp jumps (an object moving, potential interactions on it hitting
-- something) whereas a sequence of 10 look actions will not (as the world does not
-- change). This is primarily used to ensure we can cache updates of objects that
-- change properties (e.g. strings).
newtype Timestamp = Timestamp
  { unTimestamp :: Int
  } deriving stock   (Show, Generic)
    deriving newtype (Eq, Num, Enum, Ord, Real, Integral)


data MissingObject s = MissingObject Text Entity
type NoMissingObjects s m = (MonadError (MissingObject s) m)

{-
containedBy :: forall s. Lens' (Thing s) Entity
containedBy = coercedTo @(Object s ThingData) % objData % thingContainedBy

isBlankDescription :: Text -> Bool 
isBlankDescription = (T.empty==)



reifyObject :: 
  MonadWorldNoLog s m
  => StoreLens' s d
  -> AbstractObject s d
  -> m (Object s d)
reifyObject _ (StaticObject v) = return v
reifyObject l (DynamicObject ts) = do
  let co = _tsCachedObject ts
  now <- getGlobalTime
  if
    _tsCacheStamp ts == now
  then
    return co
  else
    do
      -- update the object
      updatedObj <- updateObject (_tsUpdateFunc ts) co
      -- update the world
      t <- gets getGlobalTime
      l % at (getID co) ?= DynamicObject (updateCachedObject ts updatedObj t)
      return updatedObj



instance HasID (Object s d) where
  getID = _objID

instance HasID (AbstractObject s d) where
  getID (StaticObject o) = getID o
  getID (DynamicObject ts) = getID ts

instance HasID (TimestampedObject s d) where
  getID (TimestampedObject o _ _) = getID o

-- | Obtain the current timestamp. This is a function in case I want to change the
-- implementation in the future.
getGlobalTime
  :: MonadReader (World s) m
  => m Timestamp
getGlobalTime = asks _globalTime

tickGlobalTime
  :: MonadWorldNoLog s m
  => Bool
  -> m ()
--I have no idea what my plans were for this flag.
tickGlobalTime False = dirtyTime .= True
tickGlobalTime True = do
  dirtyTime .= False
  _ <- globalTime <%= (+1)
  pass
  -- debug (bformat ("Dong. The time is now " %! int %! ".") r)

-- | Update the game title.
setTitle
  :: MonadWorld s m
  => Text -- ^ New title.
  -> m ()
setTitle = (title .=)

-- | Generate a new entity ID.
newEntityID
  :: Bool
  -> World o
  -> (Entity, World o)
newEntityID True = entityCounter % _1 <<+~ 1
newEntityID False = entityCounter % _2 <<-~ 1


class CanBeAny o d where
  toAny :: o -> d
  fromAny :: d -> Maybe o

instance CanBeAny o o where
  toAny = id
  fromAny = Just

instance CanBeAny (Object s RoomData) (AnyObject s) where
  toAny = fmap Right
  fromAny = traverse rightToMaybe

instance CanBeAny (Object s ThingData) (AnyObject s) where
  toAny = fmap Left
  fromAny = traverse leftToMaybe

instance CanBeAny (AbstractObject s RoomData) (AnyAbstractObject s) where
  toAny (StaticObject s) = StaticObject $ toAny s
  toAny (DynamicObject (TimestampedObject tsobj tsts (ObjectUpdate tsf))) =
    DynamicObject $ TimestampedObject
    (toAny tsobj) tsts (ObjectUpdate $ \a -> maybe (return a) (\r' -> Right <$$> tsf r') (fromAny a))

  fromAny ((StaticObject s)) = fmap StaticObject (fromAny s)
  fromAny ((DynamicObject
    (TimestampedObject tsobj tsts (ObjectUpdate tsf)))) = case fromAny tsobj of
    Nothing -> Nothing
    Just s -> Just $ DynamicObject
      (TimestampedObject s tsts (ObjectUpdate $ \v -> do
        r' <- tsf $ toAny v
        return $ fromMaybe v (fromAny r') ))

instance CanBeAny (AbstractObject s ThingData) (AnyAbstractObject s) where
  toAny (StaticObject s) = StaticObject $ toAny s
  toAny (DynamicObject (TimestampedObject tsobj tsts (ObjectUpdate tsf))) =
    DynamicObject $ TimestampedObject
    (toAny tsobj) tsts (ObjectUpdate $ \a -> maybe (return a) (\r' -> Left <$$> tsf r') (fromAny a))

  fromAny ((StaticObject s)) = fmap StaticObject (fromAny s)
  fromAny ((DynamicObject
    (TimestampedObject tsobj tsts (ObjectUpdate tsf)))) = case fromAny tsobj of
    Nothing -> Nothing
    Just s -> Just $ DynamicObject
      (TimestampedObject s tsts (ObjectUpdate $ \v -> do
        r' <- tsf $ toAny v
        return $ fromMaybe v (fromAny r') ))

-- | Convert log item to its JSON representation while trimming its
-- payload based on the desired verbosity. Backends that push JSON
-- messages should use this to obtain their payload.
itemJsonYaifl
  :: LogItem a
  => Verbosity
  -> YaiflItem a
  -> A.Value
itemJsonYaifl verb (YaiflItem a) = A.toJSON
  $ YaiflItem $ a { _itemPayload = payloadObject verb (_itemPayload a) }

jsonFormatYaifl :: LogItem a => ItemFormatter a
jsonFormatYaifl withColor verb i =
  B.fromText $
  colorBySeverity withColor (_itemSeverity i) $
  toStrict $ decodeUtf8 $ A.encode $ itemJsonYaifl verb (YaiflItem i)

runGame :: Text -> Game s a -> World s -> IO a --World s -> IO (World s)
runGame t f i = do
  withFile "log.json" AppendMode \fh -> do
    handleScribe <- mkHandleScribeWithFormatter jsonFormatYaifl ColorIfTerminal fh (permitItem DebugS) V2
    let makeLogEnv = registerScribe "file" handleScribe defaultScribeSettings =<< initLogEnv "" ""
    -- closeScribes will stop accepting new logs, flush existing ones and clean up resources
    bracket makeLogEnv closeScribes $ \le -> do
      let initialContext = () -- this context will be attached to every log in your app and merged w/ subsequent contexts
      evalStateT (runKatipContextT le initialContext (Namespace [t]) (unGame f)) i

withoutMissingObjects :: (HasCallStack, Monad m) => (HasCallStack => ExceptT (MissingObject s) m a) -> (HasCallStack => MissingObject s -> m a) -> m a
withoutMissingObjects f def = do
  r <- runExceptT f
  case r of
    Left m -> def m
    Right x -> return x
-}