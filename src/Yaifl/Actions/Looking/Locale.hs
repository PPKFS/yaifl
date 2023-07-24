{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Yaifl.Actions.Looking.Locale where
import Yaifl.Model.Object
import Solitude
import Data.Text.Display
import Yaifl.Model.Entity

-- | Some state we thread through printing out locale information.
data LocaleVariables wm = LocaleVariables
  { localePriorities :: LocalePriorities wm
  , domain :: AnyObject wm
  , paragraphCount :: Int
  } deriving stock (Generic)

instance Display (LocaleVariables wm) where
  displayBuilder = const "locale variables"

-- | Locale priorities
type LocalePriorities wm = Store (LocaleInfo wm)

instance Display (LocalePriorities wm) where
  displayBuilder = const "locale priorities"

data LocaleInfo wm = LocaleInfo
  { priority :: Int
  , localeObject :: AnyObject wm
  , isMentioned :: Bool
  } deriving stock (Generic)

instance Display (LocaleInfo wm) where
  displayBuilder = const "locale info"

makeFieldLabelsNoPrefix ''LocaleInfo
makeFieldLabelsNoPrefix ''LocaleVariables
