{-|
Module      : Yaifl.Common
Description : Mostly defining types to be used everywhere and some helper functions.
Copyright   : (c) Avery, 2022
License     : MIT
Maintainer  : ppkfs@outlook.com
Stability   : No
-}
module Yaifl.Common
  (-- * Smart constructors and default settings
  -- * Object querying
  isThing
  , isRoom
  , HasID(..)
  , CanBeAny(..)

  -- * Lenses

  , objectL
  , containedBy

  -- * World lookups and modifications
  , getGlobalTime
  , tickGlobalTime
  , setTitle
  , newEntityID

  , reifyObject

  , isType
  , runGame
  , module Yaifl.Types
  )
where

import Yaifl.Prelude
import Yaifl.Messages
import Yaifl.Types
import Katip
import qualified Data.Aeson as A
import qualified Data.Text.Lazy.Builder as B
import Control.Exception (bracket)
import Katip.Scribes.Handle (colorBySeverity)
import GHC.IO.Handle.FD

updateCachedObject
  :: TimestampedObject s d
  -> Object s d
  -> Timestamp
  -> TimestampedObject s d
updateCachedObject ts n t = ts & set tsCachedObject n
                                            & set tsCacheStamp t
objectL
  :: Timestamp
  -> Lens' (AbstractObject s d) (Object s d)
objectL t = lens
  (\case
    StaticObject o -> o
    DynamicObject (TimestampedObject o _ _) -> o)
  (\o n -> case o of
    StaticObject _ -> StaticObject n
    DynamicObject ts -> DynamicObject (updateCachedObject ts n t)
  )

containedBy :: forall s. Lens' (Thing s) Entity
containedBy = coercedTo @(Object s ThingData) % objData % thingContainedBy

isThing
  :: (HasID a)
  => a
  -> Bool
isThing a = getID a >= 0

isRoom
  :: (HasID a)
  => a
  -> Bool
isRoom = not . isThing

reifyObject
  :: MonadWorld s m
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

class HasID n where
  getID :: n -> Entity

instance HasID Entity where
  getID = id

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
  :: MonadWorld s m
  => m ()
tickGlobalTime = do
  r <- globalTime <%= (+1)
  debug (bformat ("Dong. The time is now " %! int %! ".") r)

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

-- | Calculate whether one object type is a subclass of another
isType
  :: Object s d
  -> ObjType
  -> World s
  -> Bool
isType _ _ _ = False

class CanBeAny o d where
  toAny :: o -> d
  fromAny :: d -> Maybe o

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
