{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Yaifl.Model.Properties.Enclosing (
  -- * Enclosing
    Enclosing(..)
  , blankEnclosing
  ) where

import Solitude

import Data.EnumSet ( EnumSet, empty )
import Yaifl.Model.Entity

-- | A component that contains other objects.
data Enclosing = Enclosing
  { contents :: EnumSet (TaggedEntity ThingTag) -- ^ The contained objects.
  , capacity :: Maybe Int -- ^ An optional number of items that can be contained.
  } deriving stock (Eq, Show, Read, Ord, Generic)

makeFieldLabelsNoPrefix ''Enclosing

-- | An enclosing component with nothing in it.
blankEnclosing :: Enclosing
blankEnclosing = Enclosing Data.EnumSet.empty Nothing

instance Taggable Enclosing EnclosingTag