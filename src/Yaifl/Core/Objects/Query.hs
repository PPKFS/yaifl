-- ~\~ language=Haskell filename=src/Yaifl/Core/Objects/Query.hs
-- ~\~ begin <<lit/worldmodel/objects/query.md|src/Yaifl/Core/Objects/Query.hs>>[0] project://lit/worldmodel/objects/query.md:6
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TemplateHaskell #-}

module Yaifl.Core.Objects.Query
  ( -- * Types
  ObjectLike(..)
  , MissingObject(..)
  , ObjectQuery
  , ObjectLookup(..)
  , ObjectUpdate(..)
  , ObjectTraverse(..)
  -- * Missing Objects
  , withoutMissingObjects
  , failHorriblyIfMissing
  , handleMissingObject
  , NoMissingObjects
  , NoMissingObject
  , NoMissingRead
  -- * Get
  , lookupThing
  , lookupRoom
  , getObject
  , getThingMaybe
  , getRoomMaybe
  , asThingOrRoom
  , getLocation
  -- * Modify
  , modifyObject
  , modifyThing
  , modifyRoom
  -- * Set
  , setThing
  , setRoom
  , traverseRooms
  , traverseThings

  , refreshRoom
  , refreshThing

  ,getCurrentPlayer
  ) where

import Cleff.Error ( Error, fromEither, runError, throwError )
import Cleff.State ( State )

import Yaifl.Core.Common ( Metadata, WorldModel, HasID(..), Entity, isThing, traceGuard, AnalysisLevel (..), noteError, currentPlayer, isRoom )
import Yaifl.Core.Logger ( Log, err )
import Yaifl.Core.Objects.Object ( _Room, _Thing, AnyObject, Object(_objID), Room, Thing, objData )
import Yaifl.Core.Objects.ObjectData
import Text.Interpolation.Nyan
import Yaifl.Core.Say

-- ~\~ begin <<lit/worldmodel/objects/query.md|missing-object>>[0] project://lit/worldmodel/objects/query.md:61
data MissingObject = MissingObject
  { _moExpected :: Text
  , _moEntity :: Entity
  } deriving stock (Eq, Show, Read, Ord, Generic)

type NoMissingObject = Error MissingObject

makeLenses ''MissingObject
-- ~\~ end
-- ~\~ begin <<lit/worldmodel/objects/query.md|handle-missing-objects>>[0] project://lit/worldmodel/objects/query.md:70
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
  Log :> es
  => State (Metadata wm) :> es
  => Text
  -> a
  -> MissingObject
  -> Eff es a
handleMissingObject msg def (MissingObject t o) = do
  noteError (const def) [int|t|When #{msg}, the object with ID #{o} could not be found because #{t}.|]

failHorriblyIfMissing ::
  Log :> es
  => (HasCallStack => Eff (NoMissingObject ': es) a)
  -> Eff es a
failHorriblyIfMissing f = withoutMissingObjects f (\(MissingObject t o) -> do
  let msg = [int|t|The object with ID #{o} could not be found because #{t}. We are failing horribly and erroring out because we can't recover.|]
  err msg
  error msg)
-- ~\~ end
-- ~\~ begin <<lit/worldmodel/objects/query.md|object-query-effect>>[0] project://lit/worldmodel/objects/query.md:107
data ObjectLookup (wm :: WorldModel) :: Effect where
  LookupThing :: (HasCallStack, HasID o) => o -> ObjectLookup wm m (Either Text (Thing wm))
  LookupRoom :: (HasCallStack, HasID o) => o -> ObjectLookup wm m (Either Text (Room wm))

data ObjectUpdate (wm :: WorldModel) :: Effect where
  SetRoom :: Room wm -> ObjectUpdate wm m ()
  SetThing :: Thing wm -> ObjectUpdate wm m ()

data ObjectTraverse (wm :: WorldModel) :: Effect where
  TraverseThings :: (Thing wm -> m (Maybe (Thing wm))) -> ObjectTraverse wm m ()
  TraverseRooms :: (Room wm -> m (Maybe (Room wm))) -> ObjectTraverse wm m ()

makeEffect ''ObjectLookup
makeEffect ''ObjectUpdate
makeEffect ''ObjectTraverse

type ObjectQuery wm es = (ObjectLookup wm :> es, ObjectUpdate wm :> es)
type NoMissingObjects wm es = (NoMissingObject :> es, ObjectLookup wm :> es, ObjectUpdate wm :> es, State (Metadata wm) :> es)
type NoMissingRead wm es = (NoMissingObject :> es, ObjectLookup wm :> es, State (Metadata wm) :> es)
type ObjectRead wm es = (ObjectLookup wm :> es, State (Metadata wm) :> es)
-- ~\~ end
-- ~\~ begin <<lit/worldmodel/objects/query.md|objectlike>>[0] project://lit/worldmodel/objects/query.md:127
class HasID o => ObjectLike wm o where
  getRoom :: NoMissingRead wm es => o -> Eff es (Room wm)
  default getRoom :: NoMissingRead wm es => o -> Eff es (Room wm)
  getRoom o = throwError $ MissingObject "called getRoom on an object with no instance"  (getID o)

  getThing :: NoMissingRead wm es => o -> Eff es (Thing wm)
  default getThing :: NoMissingRead wm es => o -> Eff es (Thing wm)
  getThing o = throwError $ MissingObject "called getThing on an object with no instance"  (getID o)
-- ~\~ end
-- ~\~ begin <<lit/worldmodel/objects/query.md|objectlike-instances>>[0] project://lit/worldmodel/objects/query.md:140
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
-- ~\~ end
-- ~\~ begin <<lit/worldmodel/objects/query.md|get-objects>>[0] project://lit/worldmodel/objects/query.md:166
getObject ::
  NoMissingRead wm es
  => ObjectLike wm o
  => o
  -> Eff es (AnyObject wm)
getObject e = if isThing e
  then review _Thing <$> getThing e
  else review _Room <$> getRoom e

getThingMaybe ::
  ObjectRead wm es
  => ObjectLike wm o
  => o
  -> Eff es (Maybe (Thing wm))
getThingMaybe o =
  if isThing (getID o)
  then withoutMissingObjects (getThing o <&> Just) (const (return Nothing))
  else return Nothing

getRoomMaybe ::
  ObjectRead wm es
  => ObjectLike wm o
  => o
  -> Eff es (Maybe (Room wm))
getRoomMaybe o = 
  if isRoom (getID o)
  then withoutMissingObjects (getRoom o <&> Just) (const (return Nothing))
  else return Nothing

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
-- ~\~ begin <<lit/worldmodel/objects/query.md|modify-objects>>[0] project://lit/worldmodel/objects/query.md:209
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

getLocation ::
  NoMissingObjects wm es
  => ObjectLike wm o
  => o
  -> Eff es (Room wm)
getLocation t = do
  t' <- getThing t
  let tcb = t' ^. objData % thingContainedBy
  o <- getObject tcb
  join $ asThingOrRoom o getLocation return

-- TODO: I'd like this to flag up an error if, somehow, it's not a no-op.
refreshRoom ::
  NoMissingObjects wm es
  => Room wm
  -> Eff es (Room wm)
refreshRoom r = do
  ifM (traceGuard Medium)
    (do
      r' <- getRoom $ _objID r
      when (r' /= r) $
        error "" --noteError $ "Refreshed room " <> " and found an outdated object; compare the two here: "  -- <> show r' <> show r
      return r)
    (pure r)

refreshThing ::
  NoMissingObjects wm es
  => Log :> es
  => Thing wm
  -> Eff es (Thing wm)
refreshThing r = do
  ifM (traceGuard Medium)
    (do
      r' <- getThing $ _objID r
      when (r' /= r) $ noteError (const ()) [int|t|Refreshed thing with ID #{_objID r} and found an outdated object|]
      return r)
    (pure r)

getCurrentPlayer ::
  Log :> es
  => ObjectLookup wm :> es
  => State (Metadata wm) :> es
  => Eff es (Thing wm)
getCurrentPlayer = failHorriblyIfMissing $ use currentPlayer >>= getThing
-- ~\~ end
-- ~\~ end
