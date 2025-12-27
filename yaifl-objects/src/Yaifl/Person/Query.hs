module Yaifl.Person.Query
  ( isNowCarriedBy
  , getPlayer
  , getPlayer'
  , getPlayerLocation
  , getActorLocation
  ) where

import Yaifl.Prelude
import Yaifl.TH (WMWithProperty)
import Yaifl.Effects.ObjectQuery
import Yaifl.Enclosing.Kind
import Yaifl.Tag
import Yaifl.Thing.Kind
import Yaifl.ObjectLike
import Yaifl.Move
import Yaifl.Refreshable
import Yaifl.Room.Kind
import Yaifl.Person.Kind
import Yaifl.Thing.Query
import Yaifl.Actions.Args

isNowCarriedBy ::
  WithoutMissingObjects wm es
  => WMWithProperty wm Person
  => WMWithProperty wm Enclosing
  => ThingLike wm t
  => t
  -> TaggedPerson wm
  -> Eff es ()
isNowCarriedBy t p = do
  t' <- getThing t
  p' <- refresh p
  void $ move t' p'

getPlayer ::
  WithoutMissingObjects wm es
  => Eff es (TaggedPerson wm)
getPlayer = do
  pr <- use #currentPlayer
  per <- getThing pr
  return (tagObject pr per)

getPlayer' ::
  WithoutMissingObjects wm es
  => Eff es (Thing wm)
getPlayer' = use #currentPlayer >>= getThing

getPlayerLocation ::
  WithoutMissingObjects wm es
  => Eff es (Room wm)
getPlayerLocation = do
  pl <- getPlayer
  getLocation pl

getActorLocation ::
  WithoutMissingObjects wm es
  => Args wm v
  -> Eff es (Room wm)
getActorLocation args = getLocation $ source args