# Other Miscellania

There's some additional baggage hanging around that, in the literate programming style, I needed to put somewhere. So here will do for now.

## Common

As this just includes some basic types, it doesn't really have a section specifically for it and as such does not have its file outline described anywhere. So it's here.

```haskell file=src/Yaifl/Common.hs
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE TemplateHaskell #-}

module Yaifl.Common
  (-- * Datatypes
  Entity(..)
  , Store(..)
  , HasID(..)
  , Timestamp(..)
  , WorldModel(..)
  , RoomDescriptions(..)

  -- * Metadata
  , Metadata(..)
  , CurrentStage(..)
  , getGlobalTime
  , tickGlobalTime
  , previousRoom
  , firstRoom
  , setTitle
  , whenConstructingM

  -- * Some defaults
  , defaultVoidID
  , defaultPlayerID
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

import Cleff.State ( State )
import qualified Data.EnumMap.Strict as EM
import qualified Data.IntMap.Strict as IM
import Display ( Display(..) )

import Solitude

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

<<room-descriptions>>
<<timestamp>>
<<world-model>>
<<world-model-families>>
<<world-model-constraints>>
<<world-metadata>>
```