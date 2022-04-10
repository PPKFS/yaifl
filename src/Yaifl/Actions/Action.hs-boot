{-# LANGUAGE RoleAnnotations #-}

module Yaifl.Actions.Action where
  
  import Yaifl.Common
  
  type role Action nominal
  data Action (wm :: WorldModel)
  type role ActionProcessing nominal
  data ActionProcessing (wm :: WorldModel)