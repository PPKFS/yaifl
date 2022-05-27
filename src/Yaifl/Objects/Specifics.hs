-- ~\~ language=Haskell filename=src/Yaifl/Objects/Specifics.hs
-- ~\~ begin <<lit/worldmodel/objects/specifics.md|src/Yaifl/Objects/Specifics.hs>>[0] project://lit/worldmodel/objects/specifics.md:4
{-# LANGUAGE TemplateHaskell #-}

module Yaifl.Objects.Specifics
  ( -- * Specifics
  ObjectSpecifics(..)
  ) where

import Solitude
import Yaifl.Properties.Enclosing
import Yaifl.Properties.Container
import Yaifl.Properties.Openable
import Yaifl.Properties.Property

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
-- ~\~ end
