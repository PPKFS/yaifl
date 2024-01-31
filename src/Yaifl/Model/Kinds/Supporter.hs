{-|
Module      : Yaifl.Model.Properties.Supporter
Copyright   : (c) Avery 2022-2023
License     : MIT
Maintainer  : ppkfs@outlook.com

A property for things which can act as a supporter for other things (a container without the physical
enclosing).
-}

module Yaifl.Model.Kinds.Supporter
  ( isSupporter
  ) where

import Solitude

import Yaifl.Model.Kinds.Object
import Yaifl.Model.Query
import Yaifl.Model.Effects
import Yaifl.Model.Metadata

-- | Check if @o@ is of the @supporter@ type.
isSupporter ::
  NoMissingObjects wm es
  => ObjectLike wm o
  => o
  -> Eff es Bool
isSupporter o = getObject o >>= (`isKind` "supporter")