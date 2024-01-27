module Yaifl.Game.Activities.PrintingLocaleParagraphAbout
  ( WithPrintingLocaleParagraphAbout
  , printingLocaleParagraphAboutImpl
  ) where

import Solitude

import Yaifl.Model.Activity
import Yaifl.Model.Kinds.Thing
import Yaifl.Model.Kinds.Object
import Yaifl.Model.Rules.Rulebook
import Yaifl.Game.Actions.Looking.Locale
import Yaifl.Model.Entity
import Yaifl.Model.HasProperty
import Yaifl.Model.Kinds.Enclosing
import Yaifl.Text.SayQQ
import Yaifl.Model.Rules.RuleEffects
import Yaifl.Text.Say
import Yaifl.Text.AdaptiveNarrative
import Yaifl.Text.Responses
import Yaifl.Model.Kinds.AnyObject
import Yaifl.Model.ObjectKind

setLocalePriority ::
  AnyObject s
  -> LocaleVariables v
  -> Int
  -> LocaleVariables v
setLocalePriority e lv i = lv & #localePriorities % at (getID e) % _Just % #priority .~ i

removeFromLocale ::
  Applicative m
  => AnyObject v
  -> LocaleVariables v
  -> LocaleInfo v
  -> m (Maybe (LocaleVariables v, LocaleInfo v), Maybe (LocaleVariables v))
removeFromLocale e lv li = do
  pure (Just (lv & #localePriorities % at (getID e) .~ Nothing, li & #priority .~ 0), Nothing)

mentionItemAndIncreaseParagraphCount ::
  Applicative m
  => AnyObject v
  -> LocaleVariables v
  -> LocaleInfo v
  -> m (Maybe (LocaleVariables v, LocaleInfo v), Maybe (LocaleVariables v))
mentionItemAndIncreaseParagraphCount e lv li = do
  let nowMentionedItem = li & #isMentioned .~ True
  pure (Just (lv &
    #localePriorities % at (getID e) ?~ nowMentionedItem
    & #paragraphCount %~ (+1), nowMentionedItem), Nothing)

type WithPrintingLocaleParagraphAbout wm =
  ( WithActivity "printingLocaleParagraphAbout" wm () (LocaleVariables wm, LocaleInfo wm) (LocaleVariables wm)
  , WMWithProperty wm Enclosing
  )

type LocaleParagraphAboutRule wm = ActivityRule wm () (LocaleVariables wm, LocaleInfo wm) (LocaleVariables wm)
{-
  (a) Print a paragraph about the item and mark it as mentioned — this is good for interesting items deserving a paragraph of their own.
  (b) Print a paragraph, but do not mark it as mentioned — this is only likely to be useful if we want to print information related to the item without mentioning the thing itself. (For instance, if the presence of a mysterious parcel resulted in a ticking noise, we could print a paragraph about the ticking noise without mentioning the parcel, which would then appear later.)
  (c) Mark the item as mentioned but print nothing — this gets rid of the item, ensuring that it will not appear in the final "you can also see" sentence, and will not be considered by subsequent rules.
  (d) Do nothing at all — the item then becomes "nondescript" and appears in the final "you can also see" sentence, unless somebody else mentions it in the mean time.
-}
printingLocaleParagraphAboutImpl :: Activity wm () (LocaleVariables wm, LocaleInfo wm) (LocaleVariables wm)
printingLocaleParagraphAboutImpl = Activity "printing a locale paragraph about something" Nothing Nothing
  (const $ notImplementedResponse "localeparagraph")
  (blankRulebook "before printing a locale paragraph")
  ((blankRulebook "carry out printing a locale paragraph")
    { rules =
      [ dontMentionUndescribed
      , dontMentionSupporter
      , dontMentionScenery
      , offerItems
      , useInitialAppearance
      , describeOnScenery
      , setPronounsFromItems
      , useInitialAppearanceOnSupporters
      -- TODO: this needs to be forced last.
      , finishActivity
      ]
    })
  (blankRulebook "after printing a locale paragraph")

-- normally this just removes "you can see yourself"
dontMentionUndescribed :: LocaleParagraphAboutRule wm
dontMentionUndescribed = Rule "don’t mention undescribed items in room descriptions rule" []
        (\(v, li@(LocaleInfo _ e _)) ->
          forThing e $ \thing ->
          ruleGuard ((thing ^. #objectData % #described) == Undescribed) $ do
            removeFromLocale e v li
        )

dontMentionSupporter :: LocaleParagraphAboutRule wm
dontMentionSupporter = notImplementedRule "don't mention player's supporter in room descriptions rule"

dontMentionScenery :: LocaleParagraphAboutRule wm
dontMentionScenery = Rule "don't mention scenery in room descriptions rule" []
  (\(v, li@(LocaleInfo _ e _)) -> ruleGuardM (e `isKind` "scenery") $ removeFromLocale e v li)

offerItems :: LocaleParagraphAboutRule wm
offerItems = notImplementedRule "offer items to writing a paragraph about rule"

useInitialAppearance :: LocaleParagraphAboutRule wm
useInitialAppearance = Rule "use initial appearance in room descriptions rule" []
  (\(v, li@(LocaleInfo _ e isMentioned)) ->
    forThing e $ \thing ->
    ruleGuard (not isMentioned) $ do
      regarding (Just thing)
      ia <- sayText $ thing ^. #objectData % #initialAppearance
      -- if the item provides the property initial appearance and the
      -- item is not handled and the initial appearance of the item is not "":
      ruleGuard ((thing ^. #objectData % #handled == NotHandled) && (ia /= "")) $ do
        -- say "[initial appearance of the item]";
        [saying|{ia}|]
        -- say "[paragraph break]";
        [saying|#{paragraphBreak}|]

        {-
        TODO:
        -- if a locale-supportable thing is on the item:
        repeat with possibility running through things on the item:
                  now the possibility is marked for listing;
                  if the possibility is mentioned:
                      now the possibility is not marked for listing;
              say "On [the item] " (A);
              list the contents of the item, as a sentence, including contents,
                  giving brief inventory information, tersely, not listing
                  concealed items, prefacing with is/are, listing marked items only;
              say ".[paragraph break]";
        -}
        -- increase the locale paragraph count by 1;
        -- now the item is mentioned;
        mentionItemAndIncreaseParagraphCount e v li
  )

useInitialAppearanceOnSupporters :: LocaleParagraphAboutRule wm
useInitialAppearanceOnSupporters = notImplementedRule "initial appearance on supporters rule"

describeOnScenery :: LocaleParagraphAboutRule wm
describeOnScenery = notImplementedRule "describe what's on scenery supporters in room descriptions rule"

describeOnMentionedSupporters :: LocaleParagraphAboutRule wm
describeOnMentionedSupporters = notImplementedRule "describe what's on mentioned supporters in room descriptions rule"

setPronounsFromItems :: LocaleParagraphAboutRule wm
setPronounsFromItems = notImplementedRule "set pronouns from items in room descriptions rule"

finishActivity :: LocaleParagraphAboutRule wm
finishActivity = Rule "finish printing locale paragraph about rule" []
  (\(v, li) -> pure (Just (v, li), Just v))