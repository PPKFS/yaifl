module Yaifl.MultiLocated.Query
  ( setMultiLocated
  , modifyMultiLocated
  ) where

import Yaifl.Prelude

import Yaifl.Effects.ObjectQuery
import Yaifl.TH

import Yaifl.MultiLocated.Kind

makeModify ''MultiLocated