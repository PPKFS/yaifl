{-# OPTIONS_GHC -Wno-orphans #-}

{-|
Module      : Yaifl.Person.Kind
Copyright   : (c) Avery 2023-2026
License     : MIT
Maintainer  : ppkfs@outlook.com

Persons represent characters in the game world.

This module defines the `Person` type and its associated components:

- `Person`: The core person type with gender and inventory data
- `Gender`: Enumeration of gender identities
- `TaggedPerson`: Type-safe reference to person objects
- Functions for creating and manipulating persons
-}

module Yaifl.Person.Kind
  ( -- * Person types
    Gender(..)
  , Person(..)
  , TaggedPerson

    -- * Person functions
  , getPersonMaybe
  , defaultPersonEnclosing

    -- * Gender queries
  , isMale
  , isFemale
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

-- | Default enclosing component for people.
-- Provides inventory capacity of 100 items.
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
