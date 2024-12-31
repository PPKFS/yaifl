{-# OPTIONS_GHC -Wno-orphans #-}
module Yaifl.Model.Kinds.Person where

import Yaifl.Prelude
import Yaifl.Model.TH (makeSpecificsWithout)
import Yaifl.Core.Kinds.AnyObject
import Yaifl.Core.Effects
import Yaifl.Core.Query.Property
import Yaifl.Core.Kinds.Enclosing
import qualified Data.EnumSet as ES
import Yaifl.Core.Tag
import Yaifl.Core.Kinds.Thing
import Yaifl.Core.ObjectLike
import Yaifl.Game.Move
import Yaifl.Core.Entity
import Yaifl.Core.WorldModel
import Yaifl.Core.Query.Enclosing
import Yaifl.Core.Refreshable

data Gender = Male | Female | NonBinary | Other Text
  deriving stock (Eq, Ord, Show, Generic, Read)

data Person = Person
  { gender :: Gender
  , carrying :: Enclosing
  } deriving stock (Eq, Ord, Show, Generic, Read)


defaultPersonEnclosing :: Enclosing
defaultPersonEnclosing = Enclosing
  { contents = ES.empty
  , capacity = Just 100
  }

makeFieldLabelsNoPrefix ''Person
makeSpecificsWithout [] ''Person

isMale ::
  Person
  -> Bool
isMale = (== Male) . gender

isFemale ::
  Person
  -> Bool
isFemale = (== Female) . gender

instance Taggable Person PersonTag
instance Taggable Person EnclosingTag

tagPersonObject ::
  Person
  -> Thing wm
  -> TaggedObject (Thing wm) PersonTag
tagPersonObject _ds = unsafeTagObject

getPerson ::
  WMWithProperty wm Person
  => TaggedPerson wm
  -> Person
getPerson = fromMaybe (error "person property witness was violated") . getPersonMaybe . getTaggedObject

type TaggedPerson wm = TaggedObject (Thing wm) PersonTag

instance WMWithProperty wm Person => IsEnclosingObject (TaggedPerson wm) where
  getEnclosing = view #carrying . getPerson

isNowCarriedBy ::
  NoMissingObjects wm es
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
  NoMissingObjects wm es
  => Eff es (TaggedPerson wm)
getPlayer = do
  pr <- use #currentPlayer
  per <- getThing pr
  return $ (tagObject pr per)

getPlayer' ::
  NoMissingObjects wm es
  => Eff es (Thing wm)
getPlayer' = use #currentPlayer >>= getThing