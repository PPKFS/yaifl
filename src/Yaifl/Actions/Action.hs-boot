{-# LANGUAGE RoleAnnotations #-}

module Yaifl.Actions.Action where
  
  import Yaifl.Common

  type role Action nominal
  data Action (s :: WorldModel)