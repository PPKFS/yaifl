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

  , beginActivity
  , endActivity
  , whenHandling
  , whenHandling'

    -- * Lenses
  , localePriorities
  , localeDomain
  , priority

  , printingNameOfADarkRoom
  , printingDescriptionOfADarkRoom
  , printingNameOfSomething
  , choosingNotableLocaleObjects
  , printingLocaleParagraphAbout
  , describingLocale
  ) where

import Solitude

import Yaifl.Core.Entity ( Store )
import Yaifl.Core.Object ( AnyObject )
import Yaifl.Core.Objects.Query ( withoutMissingObjects, handleMissingObject, failHorriblyIfMissing )
import Yaifl.Core.Rulebooks.Args ( Refreshable )
import Yaifl.Core.Rulebooks.Rule ( Rule, RuleEffects )
import Yaifl.Core.Rulebooks.Rulebook ( Rulebook(..), blankRulebook )
import Yaifl.Core.Rulebooks.Run ( runRulebookAndReturnVariables )
import Breadcrumbs ( withSpan )
import Effectful.Optics ( use )
import Data.Text.Display

data Activity wm v r = Activity
    { _activityName :: !Text
    , _activityDefault :: Maybe r
    , _activityCurrentVariables :: Maybe v
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
  { _printingNameOfADarkRoom :: !(Activity wm () ())
  , _printingNameOfSomething :: !(Activity wm (AnyObject wm) ())
  , _printingDescriptionOfADarkRoom :: !(Activity wm () ())
  , _choosingNotableLocaleObjects :: !(Activity wm (AnyObject wm) (LocalePriorities wm))
  , _printingLocaleParagraphAbout :: !(Activity wm (LocaleVariables wm, LocaleInfo wm) (LocaleVariables wm))
  , _describingLocale :: !(Activity wm (LocaleVariables wm) ())
  }

makeLenses ''Activity
makeLenses ''ActivityCollection

type ActivityLens wm v r = Lens' (ActivityCollection wm) (Activity wm v r)
makeActivity ::
  Text
  -> [Rule wm v r]
  -> Activity wm v r
makeActivity n rs = Activity n Nothing Nothing
  (blankRulebook ("Before " <> n))
  ((blankRulebook ("Carry Out " <> n)) { _rbRules = rs})
  (blankRulebook ("After " <> n))

beginActivity ::
  (RuleEffects wm es, Display v)
  => Refreshable wm v
  => ActivityLens wm v r
  -> v
  -> Eff es v
beginActivity acF c = do
  ac <- use acF
  withSpan "begin activity" (_activityName ac) $ \aSpan ->
    withoutMissingObjects
      (do
        r <- runRulebookAndReturnVariables (Just aSpan) (_activityBeforeRules ac) c
        modify (acF % activityCurrentVariables ?~ maybe c fst r)
        pure $ maybe c fst r)
      (handleMissingObject "beginning an activity" c)

whenHandling' ::
  (RuleEffects wm es, Display v, Display r)
  => Refreshable wm v
  => ActivityLens wm v r
  -> Eff es a
  -> Eff es (Either a (Maybe r))
whenHandling' acF f = whenHandling acF (const f)

whenHandling ::
  (RuleEffects wm es, Display v, Display r)
  => Refreshable wm v
  => ActivityLens wm v r
  -> (v -> Eff es a)
  -> Eff es (Either a (Maybe r))
whenHandling acF f = do
  ac <- use acF
  withSpan "handling activity" (_activityName ac) $ \aSpan ->
    case _activityCurrentVariables ac of
      Nothing -> pure (Right Nothing)
      Just c -> do
        r <- failHorriblyIfMissing $ runRulebookAndReturnVariables (Just aSpan) (_activityCarryOutRules ac) c
        modify (acF % activityCurrentVariables ?~ maybe c fst r)
        let runBlock = do
              a <- f c
              pure $ Left a
        case r of
        -- no result, so run our block
          Nothing -> runBlock
          -- no result but we did update our variables
          Just (v, Nothing) -> do
            modify (acF % activityCurrentVariables ?~ v)
            runBlock
          Just (_, Just x) -> pure (Right (Just x))

endActivity ::
  (RuleEffects wm es, Display v)
  => Refreshable wm v
  => ActivityLens wm v r
  -> Eff es v
endActivity acF = do
  ac <- use acF
  withSpan "end activity" (_activityName ac) $ \aSpan ->
    failHorriblyIfMissing
      (do
        case _activityCurrentVariables ac of
          Nothing -> error "ended without beginning"
          Just c -> do
            r <- runRulebookAndReturnVariables (Just aSpan) (_activityAfterRules ac) c
            modify (acF % activityCurrentVariables .~ Nothing)
            pure $ maybe c fst r)

doActivity ::
  (RuleEffects wm es, Display r, Display v)
  => Refreshable wm v
  => ActivityLens wm v r
  -> v
  -> Eff es (Maybe r)
doActivity acL c = do
  ac <- use acL
  withSpan "activity" (_activityName ac) $ \aSpan -> withoutMissingObjects (do
    modify (acL % activityCurrentVariables ?~ c)
    x <- runRulebookAndReturnVariables (Just aSpan) (_activityBeforeRules ac) c
    mr <- runRulebookAndReturnVariables (Just aSpan) (_activityCarryOutRules ac) (maybe c fst x)
    _ <- runRulebookAndReturnVariables (Just aSpan) (_activityAfterRules ac) (maybe c fst mr)
    modify (acL % activityCurrentVariables .~ Nothing)
    return $ snd =<< mr) (handleMissingObject "running an activity" Nothing)

makeLenses ''LocaleVariables
makeLenses ''LocaleInfo
