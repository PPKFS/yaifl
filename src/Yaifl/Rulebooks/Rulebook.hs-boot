{-# LANGUAGE RoleAnnotations #-}

module Yaifl.Rulebooks.Rulebook where
  
  import Solitude
  import Yaifl.Common
  type role Rulebook nominal representational nominal nominal
  data Rulebook (wm :: WorldModel) (ia :: Type) (v :: Type) (r :: Type) 