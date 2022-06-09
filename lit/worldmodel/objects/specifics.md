# Object Specifics

Here we define the standard library of object specifics. It's not very exciting; mostly we just list all the properties we define and some lenses for our property querying code. 

```haskell file=src/Yaifl/Objects/Specifics.hs
{-# LANGUAGE TemplateHaskell #-}

module Yaifl.Objects.Specifics
  ( -- * Specifics
  ObjectSpecifics(..)
  ) where

import Solitude
import Yaifl.Properties.Container
import Yaifl.Properties.Enclosing
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

```
