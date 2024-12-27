module Yaifl.Model.Kinds.Person where

import Yaifl.Prelude
import Yaifl.Model.TH (makeSpecificsWithout)
import Yaifl.Model.Kinds.AnyObject
import Yaifl.Model.Effects
import Yaifl.Model.HasProperty
import Yaifl.Model.Query
import Yaifl.Model.Kinds.Enclosing
import qualified Data.EnumSet as ES

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