-- ~\~ language=Haskell filename=src/Yaifl/Objects/Query.hs
-- ~\~ begin <<lit/worldmodel/objects/query.md|src/Yaifl/Objects/Query.hs>>[0] project://lit/worldmodel/objects/query.md:6
-- for ObjectLike wm Entity
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DefaultSignatures #-}

module Yaifl.Objects.Query
   {- ( -- * Get
  getAbstractThing
  , getAbstractRoom
  , getThing
  , getRoom
  , getObject
  , getAbstractObject
  , getThingMaybe
  , getRoomMaybe
  , getObjectFrom
  , asThingOrRoom

    -- * Set
  , setObject
  , setThing
  , setRoom
  , setAbstractThing
  , setAbstractRoom
  , setAbstractObjectFrom
  , setObjectFrom
    -- * Modify
  , modifyObject
  , modifyThing
  , modifyRoom
  ) -} where

import Yaifl.Common
import Yaifl.Objects.Dynamic
--import Yaifl.WorldInfo
--import Yaifl.Objects.Missing
import Yaifl.Objects.Object
import Solitude
import Control.Monad.Except (liftEither)
import Solitude
import Yaifl.Common (Entity)
import Yaifl.Logger hiding ( Error )
import qualified Data.Text.Lazy.Builder as TLB
import Cleff.Error

-- | A missing object is a textual representation of what the object was intended to be and the entity that was queried.
data MissingObject = MissingObject 
  { _moExpected :: Text
  , _moEntity :: Entity
  } deriving stock (Eq, Show, Read, Ord, Generic)

makeLenses ''MissingObject

withoutMissingObjects :: 
  (HasCallStack => Eff (Error MissingObject ': es) a) -- ^ the block
  -> (HasCallStack => MissingObject -> Eff es a)  -- ^ the handler
  -> Eff es a
withoutMissingObjects f def = do
  r <- runError f
  case r of
    Left err' -> def err'
    Right x -> return x

handleMissingObject :: 
  HasCallStack
  => Log :> es
  => TLB.Builder 
  -> Eff es a 
  -> MissingObject
  -> Eff es a
handleMissingObject msg def (MissingObject t o) = do
  err (msg <> bformat (stext %! "; Object ID: " %! stext) t (show o))
  def

failHorriblyIfMissing ::
  Log :> es
  => (HasCallStack => Eff (Error MissingObject ': es) a)
  -> Eff es a
failHorriblyIfMissing f = withoutMissingObjects f (\(MissingObject t o) -> do
  let msg = "Failing horribly and erroring out because we can't recover"
      emsg = msg <> bformat (stext %! "; Object ID: " %! stext) t (show o)
  err emsg
  error $ show emsg)

data ObjectQuery (wm :: WorldModel) :: Effect where
  GetAbstractThing :: HasID o => o -> ObjectQuery wm m (Either Text (AbstractThing wm))
  GetAbstractRoom :: HasID o => o -> ObjectQuery wm m (Either Text (AbstractRoom wm))

makeEffect ''ObjectQuery

type NoMissingObjects wm es = (Error MissingObject :> es, ObjectQuery wm :> es) 

class HasID o => ObjectLike wm o where
  getRoom :: NoMissingObjects wm es => o -> Eff es (Room wm)
  default getRoom :: NoMissingObjects wm es => o -> Eff es (Room wm)
  getRoom o = throwError $ MissingObject "Called getRoom on an object with no instance."  (getID o)

  getThing :: NoMissingObjects wm es => o -> Eff es (Thing wm)
  default getThing :: (NoMissingObjects wm es) => o -> Eff es (Thing wm)
  getThing o = throwError $ MissingObject "Called getThing on an object with no instance."  (getID o)

instance ObjectLike wm (Thing wm) where
  getThing = pure

instance ObjectLike wm (Room wm) where
  getRoom = pure

instance ObjectLike wm (AnyObject wm) where
  getThing t = fromEither
    (maybeToRight (MissingObject ("Tried to get a thing from " <> show (_objID t) <> " but it was a room.") (getID t))
      (preview _Thing t))
  getRoom t = fromEither
    (maybeToRight (MissingObject ("Tried to get a room from " <> show (_objID t) <> " but it was a thing.") (getID t))
      (preview _Room t))

{-
getAbstractObjectFrom :: 
  NoMissingObjects m
  => MonadReader (World wm) m
  => HasID o
  => StoreLens' wm d
  -> o
  -> m (AbstractObject wm d)
getAbstractObjectFrom l e = do
  w <- ask
  let i = getID e
  liftEither (maybeToRight
    (MissingObject ("Cannot find object with id " <> show i) i) (preview (l % ix i) w) )


instance ObjectLike wm Entity where
  getThing = getObjectFrom things
  getRoom = getObjectFrom rooms

getObjectFrom ::
  NoMissingObjects m
  => MonadWorld wm m
  => HasID o
  => StoreLens' wm d
  -> o
  -> m (Object wm d)
getObjectFrom l e = getAbstractObjectFrom l e >>= reifyObject l

-- * Get abstract

getAbstractThing :: 
  NoMissingObjects m
  => MonadReader (World wm) m
  => HasID o
  => o
  -> m (AbstractThing wm)
getAbstractThing = getAbstractObjectFrom things

getAbstractRoom ::
  NoMissingObjects m
  => MonadReader (World wm) m
  => HasID o
  => o
  -> m (AbstractRoom wm)
getAbstractRoom = getAbstractObjectFrom rooms

-- * Get any

getObject ::
  NoMissingObjects m
  => MonadWorld wm m
  => ObjectLike wm o
  => o
  -> m (AnyObject wm)
getObject e = if isThing e
  then (do
      o <- getThing e
      return $ review _Thing o)
  else (do
      o <- getRoom e
      return $ review _Room o)

getAbstractObject :: 
  NoMissingObjects m
  => MonadWorld wm m
  => HasID o
  => o
  -> m (AnyAbstractObject wm)
getAbstractObject e = 
  if isThing e
  then review _AbstractThing <$> getAbstractObjectFrom things e
  else review _AbstractRoom <$> getAbstractObjectFrom rooms e

-- * without a `NoMissingObjects` clause
getThingMaybe :: 
  ObjectLike wm o
  => MonadWorld wm m
  => o
  -> m (Maybe (Thing wm))
getThingMaybe o = withoutMissingObjects (getThing o <&> Just) (const (return Nothing))

getRoomMaybe ::
  ObjectLike wm o
  => MonadWorld wm m
  => o
  -> m (Maybe (Room wm))
getRoomMaybe o = withoutMissingObjects (getRoom o <&> Just) (const (return Nothing))

-- * Setting and modifying

modifyObjectFrom ::
  MonadWorld wm m
  => HasID o
  => StoreLens' wm d
  -> o
  -> (Object wm d -> Object wm d)
  -> m ()
modifyObjectFrom l o s = do
  ts <- gets getGlobalTime
  l % ix (getID o) % objectL ts %= s
  tickGlobalTime False
  pass

setObjectFrom :: 
  MonadWorld wm m
  => StoreLens' wm d
  -> Object wm d
  -> m ()
setObjectFrom l o = modifyObjectFrom l o id

setObject
  :: MonadWorld wm m
  => AnyObject wm
  -> m ()
setObject o = modifyObject o id

modifyObject ::
  MonadWorld wm m
  => HasID o
  => o
  -> (AnyObject wm -> AnyObject wm)
  -> m ()
modifyObject e s = 
  if isThing e
  then modifyObjectFrom things e (anyModifyToThing s)
  else modifyObjectFrom rooms e (anyModifyToRoom s)

anyModifyToThing :: 
  (AnyObject s -> AnyObject s)
  -> (Thing s -> Thing s)
anyModifyToThing f t = fromMaybe t (preview _Thing $ f (review _Thing t))

anyModifyToRoom :: 
  (AnyObject s -> AnyObject s)
  -> (Room s -> Room s)
anyModifyToRoom f t = fromMaybe t (preview _Room $ f (review _Room t))

-- * Setting and modifying rooms/things

modifyThing :: 
  MonadWorld wm m
  => HasID o
  => o
  -> (Thing wm -> Thing wm)
  -> m ()
modifyThing = modifyObjectFrom things

modifyRoom ::
  MonadWorld wm m
  => HasID o
  => o
  -> (Room wm -> Room wm)
  -> m ()
modifyRoom = modifyObjectFrom rooms

setThing :: 
  MonadWorld wm m
  => Thing wm
  -> m ()
setThing = setObjectFrom things

setRoom ::
  MonadWorld wm m
  => Room wm
  -> m ()
setRoom = setObjectFrom rooms

setAbstractObjectFrom :: 
  MonadWorld wm m
  => StoreLens' wm d
  -> AbstractObject wm d
  -> m ()
setAbstractObjectFrom l o = do
    l % at (getID o) ?= o
    tickGlobalTime False

setAbstractThing :: 
  MonadWorld wm m
  => AbstractThing wm
  -> m ()
setAbstractThing = setAbstractObjectFrom things

setAbstractRoom :: 
  MonadWorld wm m
  => AbstractRoom wm
  -> m ()
setAbstractRoom = setAbstractObjectFrom rooms

asThingOrRoom :: 
  NoMissingObjects m
  => MonadWorld wm m
  => ObjectLike wm o
  => o
  -> (Thing wm -> a)
  -> (Room wm -> a)
  -> m a
asThingOrRoom o tf rf =
  if isThing o
  then tf <$> getThing o
  else rf <$> getRoom o
  -}
-- ~\~ end
