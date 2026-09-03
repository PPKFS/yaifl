
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RecordWildCards #-}

module Yaifl.Locale
  ( LocaleVariables(..)
  , LocalePriorities
  , LocaleInfo(..)
  , markAsMentioned
  , clearMentioned
  ) where

import Yaifl.Prelude

import qualified Data.Set as S
import Yaifl.Store
import Yaifl.AnyObject
import Yaifl.Effects.ObjectQuery (WithoutMissingObjects)
import Yaifl.Refreshable
import Yaifl.Thing.Kind
import Yaifl.Metadata
import Yaifl.ObjectLike

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

-- | Mark a thing as mentioned in the current turn.
-- Used by the visibility system to track which objects have been referenced.
markAsMentioned ::
  WithoutMissingObjects wm es
  => ThingLike wm o
  => o
  -> Eff es ()
markAsMentioned thing = getThing thing >>= \t -> #mentionedThings %= S.insert (tagThingEntity t)

-- | Clear all mentioned things from the current turn.
-- Called before each look action to reset the mentioned state.
clearMentioned ::
  State (Metadata wm) :> es
  => Eff es ()
clearMentioned = #mentionedThings .= S.empty
