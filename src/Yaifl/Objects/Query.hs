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
import Yaifl.Logger ( Log, err )
import qualified Data.Text.Lazy.Builder as TLB
import Cleff.Error ( Error, fromEither, runError, throwError )
import Cleff.State

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
  LookupThing :: HasID o => o -> ObjectQuery wm m (Either Text (Thing wm))
  LookupRoom :: HasID o => o -> ObjectQuery wm m (Either Text (Room wm))
  SetRoom :: Room wm -> ObjectQuery wm m ()
  SetThing :: Thing wm -> ObjectQuery wm m ()

makeEffect ''ObjectQuery

type NoMissingObjects wm es = (Error MissingObject :> es, ObjectQuery wm :> es, State (Metadata wm) :> es) 

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

instance ObjectLike wm Entity where
  getRoom e = lookupRoom e >>= either (throwError . flip MissingObject e) return
  getThing e = lookupThing e >>= either (throwError . flip MissingObject e) return

getObject ::
  NoMissingObjects wm es
  => ObjectLike wm o
  => o
  -> Eff es (AnyObject wm)
getObject e = if isThing e
  then (do
      o <- getThing e
      return $ review _Thing o)
  else (do
      o <- getRoom e
      return $ review _Room o)

getThingMaybe :: 
  NoMissingObjects wm es
  => ObjectLike wm o
  => o
  -> Eff es (Maybe (Thing wm))
getThingMaybe o = withoutMissingObjects (getThing o <&> Just) (const (return Nothing))

getRoomMaybe ::
  NoMissingObjects wm es
  => ObjectLike wm o
  => o
  -> Eff es (Maybe (Room wm))
getRoomMaybe o = withoutMissingObjects (getRoom o <&> Just) (const (return Nothing))

modifyObjectFrom :: 
  (o -> Eff es (Object wm any))
  -> (Object wm any -> Eff es ())
  -> o
  -> (Object wm any -> Object wm any)
  -> Eff es ()
modifyObjectFrom g s o u = do
  obj <- g o
  s (u obj)
  pass

modifyThing :: 
  NoMissingObjects wm es
  => ObjectLike wm o
  => o
  -> (Thing wm -> Thing wm)
  -> Eff es ()
modifyThing = modifyObjectFrom getThing setThing 

modifyRoom ::
  NoMissingObjects wm es
  => ObjectLike wm o
  => o
  -> (Room wm -> Room wm)
  -> Eff es ()
modifyRoom = modifyObjectFrom getRoom setRoom

modifyObject ::
  NoMissingObjects wm es
  => ObjectLike wm o
  => o
  -> (AnyObject wm -> AnyObject wm)
  -> Eff es ()
modifyObject e s = 
  if isThing e
  then modifyThing e (anyModifyToThing s)
  else modifyRoom e (anyModifyToRoom s)

anyModifyToThing :: 
  (AnyObject s -> AnyObject s)
  -> (Thing s -> Thing s)
anyModifyToThing f t = fromMaybe t (preview _Thing $ f (review _Thing t))

anyModifyToRoom :: 
  (AnyObject s -> AnyObject s)
  -> (Room s -> Room s)
anyModifyToRoom f t = fromMaybe t (preview _Room $ f (review _Room t))
asThingOrRoom :: 
  NoMissingObjects wm es
  => ObjectLike wm o
  => o
  -> (Thing wm -> a)
  -> (Room wm -> a)
  -> Eff es a
asThingOrRoom o tf rf =
  if isThing o
  then tf <$> getThing o
  else rf <$> getRoom o
-- ~\~ end
