{-|
Module      : Yaifl.Activities.Activity
Description : An activity is a modular function.
Copyright   : (c) Avery, 2022
License     : MIT
Maintainer  : ppkfs@outlook.com
Stability   : No
-}

{-# LANGUAGE TemplateHaskell #-}

module Yaifl.Activities.Activity
  ( Activity(..)
  , ActivityCollection(..)
  , makeActivity
  , doActivity
  , LocaleVariables(..)
  , LocaleInfo(..)
  , LocalePriorities
    -- * Lenses
  , localePriorities
  , localeDomain
  , priority
  ) where

import Solitude
import Yaifl.Rulebooks.Rulebook
import Yaifl.WorldInfo
import Yaifl.Objects.Missing
import Yaifl.Objects.Object
import Yaifl.Common

data Activity wm v r = Activity
    { _activityName :: !Text
    , _activityDefault :: Maybe r
    , _activityBeforeRules :: !(Rulebook wm v v ())
    , _activityCarryOutRules :: !(Rulebook wm v v r)
    , _activityAfterRules :: !(Rulebook wm v v ())
    }


-- | Some state we thread through printing out locale information.
data LocaleVariables wm = LocaleVariables
  { _localePriorities :: LocalePriorities wm
  , _localeDomain :: !(AnyObject wm)
  , _localeParagraphCount :: Int
  }

-- | Locale priorities 
type LocalePriorities wm = Store (LocaleInfo wm)

data LocaleInfo wm = LocaleInfo
  { _priority :: Int
  , _localeObject :: AnyObject wm
  , _isMentioned :: Bool
  }

data ActivityCollection wm = ActivityCollection
  { printingNameOfADarkRoom :: !(Activity wm () ())
  , printingNameOfSomething :: !(Activity wm (AnyObject wm) ())
  , printingDescriptionOfADarkRoom :: !(Activity wm () ())
  , choosingNotableLocaleObjects :: !(Activity wm (AnyObject wm) (LocalePriorities wm))
  , printingLocaleParagraphAbout :: !(Activity wm (LocaleVariables wm, LocaleInfo wm) (LocaleVariables wm))
  , describingLocale :: !(Activity wm (LocaleVariables wm) ())
  }


makeActivity :: 
  Text
  -> Rule wm v r
  -> Activity wm v r
makeActivity n r = Activity n Nothing
  (blankRulebook ("Before " <> n))
  ((blankRulebook ("Carry Out " <> n)) { _rbRules = [r]})
  (blankRulebook ("After " <> n))

doActivity ::
  MonadWorld s m
  => (ActivityCollection s -> Activity s v r)
  -> v
  -> m (Maybe r)
doActivity f v = do
  x <- use (activities % to f)
  doActivity' x v
doActivity' :: 
  MonadWorld s m
  => Activity s v r
  -> v
  -> m (Maybe r)
doActivity' ac c = withoutMissingObjects (do
  x <- runRulebookAndReturnVariables (_activityBeforeRules ac) c
  mr <- runRulebookAndReturnVariables (_activityCarryOutRules ac) (maybe c fst x)
  _ <- runRulebookAndReturnVariables (_activityAfterRules ac) (maybe c fst mr)
  return $ snd =<< mr) (handleMissingObject "" Nothing)

makeLenses ''LocaleVariables
makeLenses ''LocaleInfo
