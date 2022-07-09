-- ~\~ language=Haskell filename=src/Yaifl/Core/Actions/Activity.hs
-- ~\~ begin <<lit/actions/activity.md|src/Yaifl/Core/Actions/Activity.hs>>[0] project://lit/actions/activity.md:4
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
import Yaifl.Core.Rulebooks.Rulebook
import Yaifl.Core.Objects.Object
import Yaifl.Core.Common
import Yaifl.Core.Rulebooks.Rule
import Yaifl.Core.Objects.Query
import Yaifl.Core.Rulebooks.Run

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
  RuleEffects wm es
  => (ActivityCollection wm -> Activity wm v r)
  -> v
  -> Eff es (Maybe r)
doActivity f v = do
  x <- error "" --use (activities % to f)
  doActivity' x v
doActivity' :: 
  RuleEffects wm es
  => Activity wm v r
  -> v
  -> Eff es (Maybe r)
doActivity' ac c = withoutMissingObjects (do
  x <- runRulebookAndReturnVariables (_activityBeforeRules ac) c
  mr <- runRulebookAndReturnVariables (_activityCarryOutRules ac) (maybe c fst x)
  _ <- runRulebookAndReturnVariables (_activityAfterRules ac) (maybe c fst mr)
  return $ snd =<< mr) (handleMissingObject "" $ return Nothing)

makeLenses ''LocaleVariables
makeLenses ''LocaleInfo

-- ~\~ end