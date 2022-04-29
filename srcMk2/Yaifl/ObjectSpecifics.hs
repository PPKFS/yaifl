{-|
Module      : Yaifl.ObjectSpecifics
Description : The 'standard' array of object specifics.
Copyright   : (c) Avery, 2021
License     : MIT
Maintainer  : ppkfs@outlook.com
Stability   : No
-}

{-# LANGUAGE TemplateHaskell #-}

module Yaifl.ObjectSpecifics
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