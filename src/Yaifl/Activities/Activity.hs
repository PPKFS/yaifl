
{-# LANGUAGE UndecidableInstances #-}

module Yaifl.Activities.Activity
  ( Activity(..)
  , ActivityRule
  , ActivityRule'
  , WithActivity
  , WithPrintingDescriptionOfADarkRoom
  , WithPrintingNameOfADarkRoom
  , WithListingNondescriptItems
  , blankActivity
  , beginActivity
  , doActivity
  , endActivity
  , makeActivity
  , whenHandling
  , whenHandling'
  ) where

import Solitude hiding ( Reader, runReader )

import Breadcrumbs ( withSpan )
import Data.Text.Display
import Effectful.Optics ( use )
import GHC.TypeLits
import Yaifl.Model.Objects.Query ( failHorriblyIfMissing )
import Yaifl.Rules.Args ( Refreshable )
import Yaifl.Rules.Rule
import Yaifl.Rules.RuleEffects
import Yaifl.Rules.Rulebook ( Rulebook(..), blankRulebook )
import Yaifl.Rules.Run ( runRulebookAndReturnVariables )
import Yaifl.Model.WorldModel
import Yaifl.Model.Object
import Yaifl.Text.Responses
import Effectful.Reader.Static

-- | A nicer wrapper around label optics for activities.
type WithActivity (name :: Symbol) wm resps v r = LabelOptic' name A_Lens (WMActivities wm) (Activity wm resps v r)

type WithPrintingNameOfADarkRoom wm = (WithActivity "printingNameOfADarkRoom" wm () () ())
type WithPrintingDescriptionOfADarkRoom wm = WithActivity "printingDescriptionOfADarkRoom" wm () () ()
type WithListingNondescriptItems wm = WithActivity "listingNondescriptItems" wm () (AnyObject wm) ()

type ActivityRulebook wm resps v re r = Rulebook wm ((:>) (Reader (Activity wm resps v re))) v r
type ActivityRule wm resps v r = ActivityRule' wm resps v r r
type ActivityRule' wm resps v re r = Rule wm ((:>) (Reader (Activity wm resps v re))) v r
data Activity wm resps v r = Activity
    { name :: Text
    , defaultOutcome :: Maybe r
    , currentVariables :: Maybe v
    , responses :: resps -> Response wm v
    , beforeRules :: ActivityRulebook wm resps v r ()
    , carryOutRules :: ActivityRulebook wm resps v r r
    , afterRules :: ActivityRulebook wm resps v r ()
    } deriving stock (Generic)

makeFieldLabelsNoPrefix ''Activity

type ActivityLens wm resps v r = Lens' (WMActivities wm) (Activity wm resps v r)

blankActivity ::
  Text
  -> Activity wm resps v r
blankActivity n = makeActivity n []

makeActivity ::
  Text
  -> [Rule wm ((:>) (Reader (Activity wm resps v r))) v r]
  -> Activity wm resps v r
makeActivity n rs = Activity n Nothing Nothing (const $ notImplementedResponse "")
  (blankRulebook ("Before " <> n))
  ((blankRulebook ("Carry Out " <> n)) { rules = rs })
  (blankRulebook ("After " <> n))

beginActivity ::
  forall wm resps v r es.
  RuleEffects wm es
  => Display v
  => Refreshable wm v
  => ActivityLens wm resps v r
  -> v
  -> Eff es v
beginActivity acL c = do
  ac <- use @(ActivityCollector wm) (#activityCollection % acL)
  withSpan "begin activity" (ac ^. #name) $ \aSpan ->
    failHorriblyIfMissing
      (do
        modify @(ActivityCollector wm) (#activityCollection % acL % #currentVariables ?~ c)
        -- run the before rules only.
        r <- runReader ac $ runRulebookAndReturnVariables (Just aSpan) (beforeRules ac) c
        whenJust r $ \r' -> modify @(ActivityCollector wm) (#activityCollection % acL % #currentVariables ?~ fst r')
        pure $ maybe c fst r)

whenHandling' ::
  RuleEffects wm es
  => Display v
  => Display r
  => Refreshable wm v
  => ActivityLens wm resps v r
  -> Eff es a
  -> Eff es (Either a (Maybe r))
whenHandling' acF f = whenHandling acF (const f)

whenHandling ::
  forall wm resps v r a es.
  RuleEffects wm es
  => Display v
  => Display r
  => Refreshable wm v
  => ActivityLens wm resps v r
  -> (v -> Eff es a)
  -> Eff es (Either a (Maybe r))
whenHandling acL f = do
  ac <- use @(ActivityCollector wm) (#activityCollection % acL)
  withSpan "handling activity" (ac ^. #name) $ \aSpan ->
    case currentVariables ac of
      Nothing -> pure (Right Nothing)
      Just c -> do
        r <- failHorriblyIfMissing $ runReader ac $ runRulebookAndReturnVariables (Just aSpan) (carryOutRules ac) c
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
  forall wm resps v r es.
  HasCallStack
  => RuleEffects wm es
  => Display v
  => Refreshable wm v
  => ActivityLens wm resps v r
  -> Eff es (Maybe v)
endActivity acF = do
  ac <- use @(ActivityCollector wm) (#activityCollection % acF)
  withSpan "end activity" (ac ^. #name) $ \aSpan ->
    failHorriblyIfMissing
      (do
        case currentVariables ac of
          Nothing -> pure Nothing
          Just c -> do
            r <- runReader ac $ runRulebookAndReturnVariables (Just aSpan) (afterRules ac) c
            modify @(ActivityCollector wm) (#activityCollection % acF % #currentVariables .~ Nothing)
            pure $ maybe (Just c) (Just . fst) r)

doActivity ::
  forall wm resps r v es.
  (RuleEffects wm es, Display r, Display v)
  => Refreshable wm v
  => ActivityLens wm resps v r
  -> v
  -> Eff es (Maybe r)
doActivity acL c = do
  ac <- use @(ActivityCollector wm) (#activityCollection % acL)
  withSpan "activity" (ac ^. #name) $ \aSpan -> runReader ac $ failHorriblyIfMissing (do
    modify @(ActivityCollector wm) (#activityCollection % acL % #currentVariables ?~ c)
    x <- runRulebookAndReturnVariables (Just aSpan) (beforeRules ac) c
    mr <- runRulebookAndReturnVariables (Just aSpan) (carryOutRules ac) (maybe c fst x)
    _ <- runRulebookAndReturnVariables (Just aSpan) (afterRules ac) (maybe c fst mr)
    modify @(ActivityCollector wm) (#activityCollection % acL % #currentVariables .~ Nothing)
    return $ snd =<< mr)
