{-# OPTIONS_GHC -Wno-orphans #-}

module Yaifl.Person.Kind
  ( Gender(..)
  , Person(..)
  , TaggedPerson
  , getPersonMaybe
  , defaultPersonEnclosing
  ) where

import Yaifl.Prelude
import Yaifl.TH (WMWithProperty, makeGetMaybe)
import Yaifl.AnyObject
import Yaifl.Property.Query
import Yaifl.Enclosing.Kind
import qualified Data.EnumSet as ES
import qualified Data.Text.Lazy.Builder as TLB
import Yaifl.Tag
import Yaifl.Thing.Kind
import Yaifl.Entity
import Yaifl.Enclosing.Query

data Gender = Male | Female | NonBinary | Other Text
  deriving stock (Eq, Ord, Show, Generic, Read)

instance Display Gender where
  displayBuilder = TLB.fromText . \case
    Male -> "male"
    Female -> "female"
    NonBinary -> "non-binary"
    Other t -> t

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
makeGetMaybe ''Person

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


getPerson ::
  WMWithProperty wm Person
  => TaggedPerson wm
  -> Person
getPerson = fromMaybe (error "person property witness was violated") . getPersonMaybe . getTaggedObject

type TaggedPerson wm = TaggedObject (Thing wm) PersonTag

instance WMWithProperty wm Person => IsEnclosingObject (TaggedPerson wm) where
  getEnclosing = view #carrying . getPerson
