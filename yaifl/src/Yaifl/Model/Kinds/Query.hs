{-|
Module      : Yaifl.Model.Query
Copyright   : (c) Avery 2022-2023
License     : MIT
Maintainer  : ppkfs@outlook.com

Typeclasses for things which are XLike (can be resolved into an X in an @Eff es@ context with relevant
constraints/effects).
-}

module Yaifl.Model.Query (

) where

import Yaifl.Prelude

import Effectful.Error.Static ( Error, throwError )
import Yaifl.Model.Entity
import Yaifl.Model.Kinds.Object
import Yaifl.Model.Effects
import Yaifl.Model.Query
import Yaifl.Model.Kinds.Enclosing ( Enclosing )
import Yaifl.Model.HasProperty
