{-# LANGUAGE RoleAnnotations #-}

module Yaifl.Objects.Dynamic where

  import Yaifl.Common
  import Solitude
  type role AbstractObject nominal nominal
  data AbstractObject (wm :: WorldModel) (d :: Type)