-- ~\~ language=Haskell filename=src/Yaifl/Lamp/ObjectSpecifics.hs
-- ~\~ begin <<lit/worldmodel/objects/specifics.md|src/Yaifl/Lamp/ObjectSpecifics.hs>>[0] project://lit/worldmodel/objects/specifics.md:6
{-# LANGUAGE TemplateHaskell #-}

module Yaifl.Lamp.ObjectSpecifics
  ( -- * Specifics
  ObjectSpecifics(..)
  ) where


import Yaifl.Lamp.Properties.Container
import Yaifl.Core.Properties.Enclosing
import Yaifl.Lamp.Properties.Openable
import Yaifl.Core.Properties.Property ( HasProperty(..) )

data ObjectSpecifics =
  NoSpecifics
  | EnclosingSpecifics Enclosing
  | ContainerSpecifics Container
  | OpenableSpecifics Openable
  deriving stock (Eq, Show, Read)

makePrisms ''ObjectSpecifics

instance HasProperty ObjectSpecifics Enclosing where
  propertyL = _EnclosingSpecifics `thenATraverse` (_ContainerSpecifics % containerEnclosing)

instance HasProperty ObjectSpecifics Container where
  propertyL = castOptic _ContainerSpecifics

instance HasProperty ObjectSpecifics Enterable where
  propertyL = _ContainerSpecifics % containerEnterable

instance HasProperty ObjectSpecifics Openable where
  propertyL = _OpenableSpecifics `thenATraverse` (_ContainerSpecifics % containerOpenable)

-- ~\~ endccV
