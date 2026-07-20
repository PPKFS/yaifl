{-# LANGUAGE RecordWildCards #-}
module Yaifl.Supporter.Create
  ( addSupporter
  , newSupporter
  , SupporterConfig(..)

  ) where

import Yaifl.Prelude

import Yaifl.Entity
import Yaifl.Object.Create
import Yaifl.Thing.Kind
import Yaifl.Supporter.Kind
import Yaifl.Enclosing.Kind ( Enclosing (..), blankEnclosing )
import Yaifl.Tag
import Yaifl.WorldModel
import Yaifl.ObjectSpecifics
import Yaifl.Thing.Create
import Yaifl.Builder
import Yaifl.Container.Kind
import Yaifl.Object.Kind (ObjectKind(ObjectKind))

data SupporterConfig wm p = SupporterConfig
  { description :: WMText wm
  , initialAppearance :: WMText wm
  , thingModify :: Eff '[State (Thing wm)] ()
  , enterable :: Enterable
  , location :: Maybe EnclosingEntity
  , carryingCapacity :: Int
  } deriving stock (Generic)

newSupporter :: IsString (WMText wm) => SupporterConfig wm 'Complete
newSupporter = SupporterConfig
  { description = ""
  , initialAppearance = ""
  , thingModify = pass
  , carryingCapacity = 100
  , enterable = NotEnterable
  , location = Nothing
  }

addSupporter ::
  forall wm es.
  AddObjects wm es
  => WMText wm
  -> SupporterConfig wm 'Complete
  -> Eff es SupporterEntity
addSupporter name SupporterConfig{..} = do
    let enc = (blankEnclosing { capacity = Just carryingCapacity })
        sup = Supporter enc enterable
    c <- addThing name newThing
          { description
          , initialAppearance
          , specifics = inj (Proxy @wm) $ SupporterSpecifics sup
          , location
          , thingModify
          , objType = ObjectKind "supporter"
          }
    pure $ tagEntity @Supporter @SupporterTag sup c

