{-# LANGUAGE RecordWildCards #-}
module Yaifl.Container.Create
  ( addContainer
  , ContainerConfig(..)
  , newContainer

  ) where

import Yaifl.Prelude

import Yaifl.Entity
import Yaifl.Object.Create
import Yaifl.Thing.Kind
import Yaifl.Container.Kind
import Yaifl.Openable.Kind
import Yaifl.Tag
import Yaifl.WorldModel
import Yaifl.ObjectSpecifics
import Yaifl.Thing.Create

data ContainerConfig wm = ContainerConfig
  { description :: WMText wm
  , initialAppearance :: WMText wm
  , location :: Maybe EnclosingEntity
  , thingModify :: Eff '[State (Thing wm)] ()
  , opacity :: Opacity
  , enterable :: Enterable
  , openStatus :: (Opened, Openable)
  , carryingCapacity :: Int
  } deriving stock (Generic)

makeFieldLabelsNoPrefix ''ContainerConfig

newContainer :: IsString (WMText wm) => ContainerConfig wm
newContainer = ContainerConfig
  { description = ""
  , initialAppearance = ""
  , thingModify = pass
  , opacity = Opaque
  , carryingCapacity = 100
  , enterable = NotEnterable
  , openStatus = (Open, NotOpenable)
  , location = Nothing
  }

addContainer ::
  forall wm es.
  AddObjects wm es
  => WMText wm
  -> ContainerConfig wm
  -> Eff es ContainerEntity
addContainer name ContainerConfig{..} = do
    let cs = makeContainer (Just carryingCapacity) (Just opacity) (Just enterable) (Just $ snd openStatus) (Just $ fst openStatus)
    c <- addThing name newThing
          { description
          , initialAppearance
          , specifics = inj (Proxy @wm) $ ContainerSpecifics cs
          , location
          , thingModify
          }
    pure $ tagEntity @Container @ContainerTag cs c