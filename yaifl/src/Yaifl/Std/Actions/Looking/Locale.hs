
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RecordWildCards #-}

module Yaifl.Std.Actions.Looking.Locale where

import Yaifl.Prelude
import Yaifl.Store
import Yaifl.AnyObject
import Yaifl.Refreshable
import Yaifl.Thing.Kind

-- | Some state we thread through printing out locale information.
data LocaleVariables wm = LocaleVariables
  { localePriorities :: LocalePriorities wm
  , domain :: AnyObject wm
  , paragraphCount :: Int
  } deriving stock (Generic)

instance Display (LocaleVariables wm) where
  displayBuilder = const "locale variables"

instance Refreshable wm (LocaleVariables wm) where
  refresh LocaleVariables{..} = do
    lp <- refresh localePriorities
    dom <- refresh domain
    return $ LocaleVariables lp dom paragraphCount

-- | Locale priorities
type LocalePriorities wm = Store (LocaleInfo wm)

instance Display (LocalePriorities wm) where
  displayBuilder = const "locale priorities"

data LocaleInfo wm = LocaleInfo
  { priority :: Int
  , localeObject :: Thing wm
  , isMentioned :: Bool
  } deriving stock (Generic)

instance Refreshable wm (LocaleInfo wm) where
  refresh li = refreshThing (localeObject li) >>= \t -> return li { localeObject = t }

instance Display (LocaleInfo wm) where
  displayBuilder = const "locale info"

makeFieldLabelsNoPrefix ''LocaleInfo
makeFieldLabelsNoPrefix ''LocaleVariables
