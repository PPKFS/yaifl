{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Yaifl.Core.Actions.Activity
  ( Activity(..)
  , LocaleInfo(..)
  , LocalePriorities
  , LocaleVariables(..)
  , WithActivity
  , WithPrintingDescriptionOfADarkRoom
  , WithPrintingNameOfADarkRoom
  , beginActivity
  , doActivity
  , endActivity
  , makeActivity
  , whenHandling
  , whenHandling'
  ) where

import Solitude

import Breadcrumbs ( withSpan )
import Data.Text.Display
import Effectful.Optics ( use )
import GHC.TypeLits
import Yaifl.Core.Entity ( Store )
import Yaifl.Core.Object ( AnyObject )
import Yaifl.Core.Objects.Query ( withoutMissingObjects, handleMissingObject, failHorriblyIfMissing )
import Yaifl.Core.Rulebooks.Args ( Refreshable )
import Yaifl.Core.Rulebooks.Rule
import Yaifl.Core.Rulebooks.Rulebook ( Rulebook(..), blankRulebook )
import Yaifl.Core.Rulebooks.Run ( runRulebookAndReturnVariables )
import Yaifl.Core.WorldModel

-- | A nicer wrapper around label optics for activities.
type WithActivity (name :: Symbol) wm v r =
  LabelOptic' name A_Lens (WMActivities wm) (Activity wm v r)

type WithPrintingNameOfADarkRoom wm = (WithActivity "printingNameOfADarkRoom" wm () ())
type WithPrintingDescriptionOfADarkRoom wm = WithActivity "printingDescriptionOfADarkRoom" wm () ()

data Activity wm v r = Activity
    { name :: Text
    , defaultOutcome :: Maybe r
    , currentVariables :: Maybe v
    , beforeRules :: Rulebook wm v v ()
    , carryOutRules :: Rulebook wm v v r
    , afterRules :: Rulebook wm v v ()
    } deriving stock (Generic)

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
makeFieldLabelsNoPrefix ''Activity
makeFieldLabelsNoPrefix ''LocaleVariables

type ActivityLens wm v r = Lens' (WMActivities wm) (Activity wm v r)

makeActivity ::
  Text
  -> [Rule wm v r]
  -> Activity wm v r
makeActivity n rs = Activity n Nothing Nothing
  (blankRulebook ("Before " <> n))
  ((blankRulebook ("Carry Out " <> n)) { rules = rs })
  (blankRulebook ("After " <> n))

beginActivity ::
  forall wm v r es.
  (RuleEffects wm es, Display v)
  => Refreshable wm v
  => ActivityLens wm v r
  -> v
  -> Eff es v
beginActivity acL c = do
  ac <- use @(ActivityCollector wm) (#activityCollection % acL)
  withSpan "begin activity" (ac ^. #name) $ \aSpan ->
    withoutMissingObjects
      (do
        -- run the before rules only.
        r <- runRulebookAndReturnVariables (Just aSpan) (beforeRules ac) c
        modify @(ActivityCollector wm) (#activityCollection % acL % #currentVariables ?~ maybe c fst r)
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
  forall wm v r a es.
  (RuleEffects wm es, Display v, Display r)
  => Refreshable wm v
  => ActivityLens wm v r
  -> (v -> Eff es a)
  -> Eff es (Either a (Maybe r))
whenHandling acL f = do
  ac <- use @(ActivityCollector wm) (#activityCollection % acL)
  withSpan "handling activity" (ac ^. #name) $ \aSpan ->
    case currentVariables ac of
      Nothing -> pure (Right Nothing)
      Just c -> do
        r <- failHorriblyIfMissing $ runRulebookAndReturnVariables (Just aSpan) (carryOutRules ac) c
        modify @(ActivityCollector wm) (#activityCollection % acL % #currentVariables ?~ maybe c fst r)
        let runBlock = do
              a <- f c
              pure $ Left a
        case r of
        -- no result, so run our block
          Nothing -> runBlock
          -- no result but we did update our variables
          Just (v, Nothing) -> do
            modify @(ActivityCollector wm) (#activityCollection % acL % #currentVariables ?~ v)
            runBlock
          Just (_, Just x) -> pure (Right (Just x))

endActivity ::
  forall wm v r es.
  (RuleEffects wm es, Display v)
  => Refreshable wm v
  => ActivityLens wm v r
  -> Eff es v
endActivity acF = do
  ac <- use @(ActivityCollector wm) (#activityCollection % acF)
  withSpan "end activity" (ac ^. #name) $ \aSpan ->
    failHorriblyIfMissing
      (do
        case currentVariables ac of
          Nothing -> error "ended without beginning"
          Just c -> do
            r <- runRulebookAndReturnVariables (Just aSpan) (afterRules ac) c
            modify @(ActivityCollector wm) (#activityCollection % acF % #currentVariables .~ Nothing)
            pure $ maybe c fst r)

doActivity ::
  forall wm r v es.
  (RuleEffects wm es, Display r, Display v)
  => Refreshable wm v
  => ActivityLens wm v r
  -> v
  -> Eff es (Maybe r)
doActivity acL c = do
  ac <- use @(ActivityCollector wm) (#activityCollection % acL)
  withSpan "activity" (ac ^. #name) $ \aSpan -> withoutMissingObjects (do
    modify @(ActivityCollector wm) (#activityCollection % acL % #currentVariables ?~ c)
    x <- runRulebookAndReturnVariables (Just aSpan) (beforeRules ac) c
    mr <- runRulebookAndReturnVariables (Just aSpan) (carryOutRules ac) (maybe c fst x)
    _ <- runRulebookAndReturnVariables (Just aSpan) (afterRules ac) (maybe c fst mr)
    modify @(ActivityCollector wm) (#activityCollection % acL % #currentVariables .~ Nothing)
    return $ snd =<< mr) (handleMissingObject "running an activity" Nothing)

makeLenses ''LocaleVariables
makeLenses ''LocaleInfo
