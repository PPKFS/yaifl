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

<<other-stuff1>>

<<world-model>>
<<world-model-families>>
<<world-model-constraints>>
```


```haskell id=other-stuff1
-- | Again lifted directly from Inform; this sets whether to always print room
-- descriptions (No..) even if the room is visited, to only print them on the first
-- entry (Sometimes..) or never.
data RoomDescriptions = SometimesAbbreviatedRoomDescriptions
  | AbbreviatedRoomDescriptions
  | NoAbbreviatedRoomDescriptions 
  deriving stock (Eq, Show, Read, Ord, Enum, Generic)

data WorldStage = Construction | Playing 
  deriving stock (Eq, Show, Read, Ord, Enum, Generic)

-- | For now, a timestamp is simply an integer. The timestamp is updated whenever some
-- modification is made to the 'World'; therefore it does not directly correspond to
-- some sort of in-game turn counter. For example, throwing an object would result in
-- multiple timestamp jumps (an object moving, potential interactions on it hitting
-- something) whereas a sequence of 10 look actions will not (as the world does not
-- change). This is primarily used to ensure we can cache updates of objects that
-- change properties (e.g. strings).
newtype Timestamp = Timestamp
  { unTimestamp :: Int
  } deriving stock   (Show, Read, Generic)
    deriving newtype (Eq, Num, Enum, Ord, Real, Integral)
```

```haskell id=other-stuff2
{-
instance Prettify Text where
  prettify = id
instance Prettify (Object s d) where
  prettify Object{..} = _objName <> " (ID: " <>  show (unID _objID) <> ")\n" <> toStrict (pString (toString s)) where
    s = "{ Description = " <> _objDescription <>
        ", Type = " <> prettify _objType <>
        -- F.% ", Creation Time = " F.% F.stext
        ", Specifics = " <> prettify _objSpecifics <>
        ", Data = " <> prettify _objData
-}
containedBy :: forall wm. Lens' (Thing wm) Entity
containedBy = coercedTo @(Object wm ThingData) % objData % thingContainedBy

-- | Calculate whether one object type is a subclass of another
isType
  :: MonadReader (World wm) m
  -- => ObjectLike wm o
  => o
  -> ObjType
  -> m Bool
isType _ _ = return False
```

```haskell file=src/Yaifl/Objects/Object.hs

{-|
Module      : Yaifl.Objects.Object
Description : A game entity.
Copyright   : (c) Avery, 2022
License     : MIT
Maintainer  : ppkfs@outlook.com
Stability   : No
-}

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE RecordWildCards #-}

module Yaifl.Objects.Object 
  ( -- * Types
    ObjType(..)
  , Object(..)
  , ObjectLike(..)
  , CanBeAny(..)
  , Thing
  , Room
  , AnyObject

  -- * Object Helpers
  , objectEquals
  , isType

  -- * Lenses
  , objName
  , objDescription
  , objID
  , objType
  , objCreationTime
  , objSpecifics
  , objData
  , containedBy
  , _Room
  , _Thing
  ) where

import Solitude
import Yaifl.Common
import Yaifl.ObjectSpecifics (ObjectSpecifics)
import Yaifl.Objects.ObjectData
import Yaifl.Objects.Missing
import {-# SOURCE #-} Yaifl.World
import Control.Monad.Except (liftEither, throwError)

<<obj-type>>
<thing-room-anyobject>
-- | An 'Object' is any kind of game object, where @a@ should either be ThingData/RoomData
-- or Either ThingData RoomData

<<obj-definition>>
<<obj-hasid>>

<<obj-eq>>

makeLenses ''Object

<<obj-functor>>

<<obj-prisms>>

<<other-stuff2>>


<<can-be-any>>

<<objectlike>>
```