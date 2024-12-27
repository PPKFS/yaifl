module Yaifl.Model.Kinds.Person where

import Yaifl.Prelude
import Yaifl.Model.TH (makeSpecificsWithout)
import Yaifl.Model.Kinds.AnyObject
import Yaifl.Model.Effects
import Yaifl.Model.HasProperty
import Yaifl.Model.Query
import Yaifl.Model.Kinds.Enclosing
import qualified Data.EnumSet as ES
import Yaifl.Model.Tag
import Yaifl.Model.Kinds.Thing
import Yaifl.Model.ObjectLike
import Yaifl.Game.Move
import Yaifl.Model.Entity

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

instance EnclosingObject Person where
  enclosingL = castOptic #carrying

isNowCarriedBy ::
  NoMissingObjects wm es
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
  let po = (tagObject pr per)
  return po