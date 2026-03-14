module Yaifl.Container.Query
  ( setContainer
  , setEnterable
  , modifyContainer
  , modifyEnterable
  , thingIsOpenContainer
  , thingIsClosedContainer
  , isContainer
  ) where

import Yaifl.Prelude

import Yaifl.Effects.ObjectQuery
import Yaifl.AnyObject
import Yaifl.Object.Kind
import Yaifl.Thing.Kind
import Yaifl.Metadata
import Yaifl.ObjectLike
import Yaifl.Property.Query( defaultPropertySetter, modifyProperty )
import Yaifl.TH ( WMWithProperty, makeModify )
import Yaifl.Openable.Kind
import Yaifl.Container.Kind

makeModify ''Enterable
makeModify ''Container

-- | Check if @o@ is of the @container@ type.
isContainer ::
  WithoutMissingObjects wm es
  => ObjectLike wm o
  => o
  -> Eff es Bool
isContainer o = getObject o >>= (`isKind` "container")

thingIsOpenContainer ::
  WMWithProperty wm Container
  => Thing wm
  -> Bool
thingIsOpenContainer = (== Just Open) . fmap (view (#openable % #opened)) . getContainerMaybe

thingIsClosedContainer ::
  WMWithProperty wm Container
  => Thing wm
  -> Bool
thingIsClosedContainer = (== Just Closed) . fmap (view (#openable % #opened)) . getContainerMaybe
