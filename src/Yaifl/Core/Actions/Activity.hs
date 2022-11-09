{-|
Module      : Yaifl.Activities.Activity
Description : An activity is a modular function.
Copyright   : (c) Avery, 2022
License     : MIT
Maintainer  : ppkfs@outlook.com
Stability   : No
-}

{-# LANGUAGE TemplateHaskell #-}

module Yaifl.Core.Actions.Activity
  ( Activity(..)
  , ActivityCollection(..)
  , makeActivity
  , LocaleVariables(..)
  , LocaleInfo(..)
  , LocalePriorities

  , doActivity
  , doActivity'

    -- * Lenses
  , localePriorities
  , localeDomain
  , priority
  ) where

import Solitude

import Yaifl.Core.Entity ( Store )
import Yaifl.Core.Object ( AnyObject )
import Yaifl.Core.Objects.Query ( withoutMissingObjects, handleMissingObject )
import Yaifl.Core.Rulebooks.Args ( Refreshable )
import Yaifl.Core.Rulebooks.Rule ( Rule, RuleEffects )
import Yaifl.Core.Rulebooks.Rulebook ( Rulebook(..), blankRulebook )
import Yaifl.Core.Rulebooks.Run ( runRulebookAndReturnVariables )
import Breadcrumbs

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

instance Display (LocaleVariables wm) where
  displayBuilder = const "locale variables"

-- | Locale priorities
type LocalePriorities wm = Store (LocaleInfo wm)

instance Display (LocalePriorities wm) where
  displayBuilder = const "locale priorities"

data LocaleInfo wm = LocaleInfo
  { _priority :: Int
  , _localeObject :: AnyObject wm
  , _isMentioned :: Bool
  }

instance Display (LocaleInfo wm) where
  displayBuilder = const "locale info"

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
  (RuleEffects wm es, Display r, Display v)
  => Refreshable wm v
  => (ActivityCollection wm -> Activity wm v r)
  -> v
  -> Eff es (Maybe r)
doActivity = (. flip doActivity') . (>>=) . gets

doActivity' ::
  (RuleEffects wm es, Display r, Display v)
  => Refreshable wm v
  => Activity wm v r
  -> v
  -> Eff es (Maybe r)
doActivity' ac c = withSpan "activity" (_activityName ac) $ \aSpan -> withoutMissingObjects (do
  x <- runRulebookAndReturnVariables (Just aSpan) (_activityBeforeRules ac) c
  mr <- runRulebookAndReturnVariables (Just aSpan) (_activityCarryOutRules ac) (maybe c fst x)
  _ <- runRulebookAndReturnVariables (Just aSpan) (_activityAfterRules ac) (maybe c fst mr)
  return $ snd =<< mr) (handleMissingObject "running an activity" Nothing)

makeLenses ''LocaleVariables
makeLenses ''LocaleInfo
