{-# LANGUAGE RoleAnnotations #-}

module Yaifl.Activities.Activity where
  import Yaifl.Common
  
  type role ActivityCollection nominal
  data ActivityCollection (wm :: WorldModel)