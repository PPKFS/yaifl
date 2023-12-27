{-|
Module      : Yaifl.Model.Properties.Supporter
Copyright   : (c) Avery 2022-2023
License     : MIT
Maintainer  : ppkfs@outlook.com

A property for things which can act as a supporter for other things (a container without the physical
enclosing).
-}

module Yaifl.Model.Properties.Supporter
  ( isSupporter
  ) where

import Solitude

import Yaifl.Model.Object
import Yaifl.Model.Objects.Query
import Yaifl.Model.Objects.Effects

-- | Check if @o@ is of the @supporter@ type.
isSupporter ::
  NoMissingObjects wm es
  => ObjectLike wm o
  => o
  -> Eff es Bool
isSupporter o = getObject o >>= (`isType` "supporter")