{-# LANGUAGE RoleAnnotations #-}
module Yaifl.Core.Actions.Activity where
import Yaifl.Core.Common

type role ActivityCollection nominal
data ActivityCollection (wm :: WorldModel)