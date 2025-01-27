module Yaifl.Core.Activity
  ( Activity(..)
  , ActivityRule
  , ActivityRule'
  , WithActivity
  , ActivityLens
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
  , afterActivityRules
  ) where

import Yaifl.Prelude hiding ( Reader, runReader )

import Breadcrumbs ( withSpan )
import GHC.TypeLits
import Effectful.Reader.Static
import Yaifl.Core.Refreshable ( Refreshable )
import Yaifl.Core.Effects
import Yaifl.Core.Kinds.AnyObject
import Yaifl.Core.WorldModel
import Yaifl.Core.Rules.Rulebook
import Yaifl.Core.Rules.Run
import Yaifl.Text.Responses
import Yaifl.Core.Rules.RuleEffects
import Yaifl.Text.SayableValue

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
    , afterRules :: ActivityRulebook wm resps v r r
    , combineResults :: Maybe r -> Maybe r -> Maybe r
    } deriving stock (Generic)

makeFieldLabelsNoPrefix ''Activity

afterActivityRules :: Lens' (Activity wm resps v r) (ActivityRulebook wm resps v r r)
afterActivityRules = #afterRules

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
  (const)

beginActivity ::
  forall wm resps v r es.
  RuleEffects wm es
  => SayableValue (WMText wm) wm
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
        r <- runReader ac $ runRulebookAndReturnVariables (Just aSpan) True (beforeRules ac) c
        whenJust r $ \r' -> modify @(ActivityCollector wm) (#activityCollection % acL % #currentVariables ?~ fst r')
        pure $ maybe c fst r)

whenHandling' ::
  RuleEffects wm es
  => SayableValue (WMText wm) wm
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
  => SayableValue (WMText wm) wm
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
        r <- failHorriblyIfMissing $ runReader ac $ runRulebookAndReturnVariables (Just aSpan) True (carryOutRules ac) c
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
  => SayableValue (WMText wm) wm
  => Display v
  => Display r
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
            r <- runReader ac $ runRulebookAndReturnVariables (Just aSpan) True (afterRules ac) c
            modify @(ActivityCollector wm) (#activityCollection % acF % #currentVariables .~ Nothing)
            pure $ maybe (Just c) (Just . fst) r)

doActivity ::
  forall wm resps r v es.
  (RuleEffects wm es, Display r, Display v)
  => SayableValue (WMText wm) wm
  => Refreshable wm v
  => ActivityLens wm resps v r
  -> v
  -> Eff es (Maybe r)
doActivity acL c = do
  ac <- use @(ActivityCollector wm) (#activityCollection % acL)
  withSpan "activity" (ac ^. #name) $ \aSpan -> runReader ac $ failHorriblyIfMissing (do
    modify @(ActivityCollector wm) (#activityCollection % acL % #currentVariables ?~ c)
    x <- runRulebookAndReturnVariables (Just aSpan) True (beforeRules ac) c
    mr <- runRulebookAndReturnVariables (Just aSpan) True (carryOutRules ac) (maybe c fst x)
    er <- runRulebookAndReturnVariables (Just aSpan) True (afterRules ac) (maybe c fst mr)
    modify @(ActivityCollector wm) (#activityCollection % acL % #currentVariables .~ Nothing)
    return $ (combineResults ac) (snd =<< mr) (snd =<< er))
