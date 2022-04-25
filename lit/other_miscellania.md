# Module Headers and Other Miscellania

In the spirit of literate programming I needed to put the module import/export lists somewhere
but I didn't want to spam the other files. So they can all go in here.

## Common

```haskell file=src/Yaifl/Common.hs
{-# OPTIONS_GHC -Wno-orphans #-}

module Yaifl.Common
  (-- * Datatypes
  Entity(..)
  , Store(..)
  , HasID(..)
  , Timestamp(..)
  , WorldModel(..)
  , RoomDescriptions(..)
  , WorldStage(..)
  
  , defaultVoidID
  , emptyStore
  -- * Object querying
  , isThing
  , isRoom
    -- * Type family nonsense
  , WMObjSpecifics
  , WMValues
  , WMDirections

  , WMShow
  , WMRead
  , WMOrd
  , WMEq
  )
where

import Solitude
import qualified Data.EnumMap.Strict as EM
import qualified Data.IntMap.Strict as IM
import Display

instance {-# OVERLAPPABLE #-} Display a where
  display = const "No display instance"

<<entity-def>>
<<thing-or-room>>
<<has-id>>
<<base-ids>>
<<store-def>>
<<alter-store>>
<<store-at>>
<<store-instances>>

<<world-model>>
<<world-model-families>>
<<world-model-constraints>>
```