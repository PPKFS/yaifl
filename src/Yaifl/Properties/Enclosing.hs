{-|
Module      : Yaifl.Properties.Enclosing
Description : A component for objects that contain other things (though not necessarily a container; e.g. a supporter or a room has an enclosing part.)
Copyright   : (c) Avery, 2022
License     : MIT
Maintainer  : ppkfs@outlook.com
Stability   : No
-}

{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

module Yaifl.Properties.Enclosing 
  ( -- * Types
    Enclosing(..)
  , blankEnclosing
  
    -- * Lenses
  , enclosingContains
  , enclosingCapacity
  ) where

import Solitude hiding (empty)
import Data.EnumSet (EnumSet, empty)
import Yaifl.Common (Entity)

-- | A component that contains other objects.
data Enclosing = Enclosing
  { _enclosingContains :: EnumSet Entity
  , _enclosingCapacity :: Maybe Int
  } deriving stock (Eq, Show, Read, Ord, Generic)

blankEnclosing :: Enclosing
blankEnclosing = Enclosing empty Nothing

makeLenses ''Enclosing