module Yaifl.Activities.PrintingLocaleParagraphAbout
  ( WithPrintingLocaleParagraphAbout
  , printingLocaleParagraphAboutImpl
  ) where

import Solitude

import Yaifl.Activities.Activity
import Yaifl.Model.Object
import Yaifl.Model.Objects.Query (getThingMaybe)
import Yaifl.Model.Objects.ThingData
import Yaifl.Rules.Rule ( Rule, makeRule, notImplementedRule )
import Yaifl.Rules.Rulebook ( Rulebook(..), blankRulebook )
import Yaifl.Actions.Looking.Locale
import Yaifl.Model.Entity
import Yaifl.Model.Properties.Has
import Yaifl.Model.Properties.Enclosing

setLocalePriority ::
  AnyObject s
  -> LocaleVariables v
  -> Int
  -> LocaleVariables v
setLocalePriority e lv i = lv & #localePriorities % at (getID e) % _Just % #priority .~ i

removeFromLocale ::
  AnyObject s
  -> LocaleVariables v
  -> LocaleVariables v
removeFromLocale e lv = lv & #localePriorities % at (getID e) .~ Nothing

type WithPrintingLocaleParagraphAbout wm =
  ( WithActivity "printingLocaleParagraphAbout" wm (LocaleVariables wm, LocaleInfo wm) (LocaleVariables wm)
  , WMWithProperty wm Enclosing
  )
type LocaleParagraphAboutRule wm = Rule wm (LocaleVariables wm, LocaleInfo wm) (LocaleVariables wm)
{-
  (a) Print a paragraph about the item and mark it as mentioned — this is good for interesting items deserving a paragraph of their own.
  (b) Print a paragraph, but do not mark it as mentioned — this is only likely to be useful if we want to print information related to the item without mentioning the thing itself. (For instance, if the presence of a mysterious parcel resulted in a ticking noise, we could print a paragraph about the ticking noise without mentioning the parcel, which would then appear later.)
  (c) Mark the item as mentioned but print nothing — this gets rid of the item, ensuring that it will not appear in the final "you can also see" sentence, and will not be considered by subsequent rules.
  (d) Do nothing at all — the item then becomes "nondescript" and appears in the final "you can also see" sentence, unless somebody else mentions it in the mean time.
-}
printingLocaleParagraphAboutImpl :: Activity wm (LocaleVariables wm, LocaleInfo wm) (LocaleVariables wm)
printingLocaleParagraphAboutImpl = Activity "printing a locale paragraph about something" Nothing Nothing
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
      ]
    })
  (blankRulebook "after printing a locale paragraph")

-- normally this just removes "you can see yourself"
dontMentionUndescribed :: LocaleParagraphAboutRule wm
dontMentionUndescribed = makeRule "don’t mention undescribed items in room descriptions rule" []
        (\(v, LocaleInfo _ e _) -> do
          asThing <- getThingMaybe e
          let isDesc = asThing ^? _Just % #objectData % #described
          if
            isDesc == Just Undescribed
          then
            return . Just $ removeFromLocale e v
          else
            return Nothing
        )

dontMentionSupporter :: LocaleParagraphAboutRule wm
dontMentionSupporter = makeRule "don't mention player's supporter in room descriptions rule" []
  (\(v, LocaleInfo _ e _) -> do
          asThing <- getThingMaybe e
          let isDesc = asThing ^? _Just % #objectData % #described
          if
            isDesc == Just Undescribed
          then
            return . Just $ removeFromLocale e v --setLocalePriority e v 0
          else
            return Nothing
  )

dontMentionScenery :: LocaleParagraphAboutRule wm
dontMentionScenery = makeRule "don't mention scenery in room descriptions rule" []
  (\(v, LocaleInfo _ e _) -> do
          isScenery <- e `isType` "scenery"
          if
            isScenery
          then
            return . Just $ removeFromLocale e v
          else
            return Nothing
  )

offerItems :: LocaleParagraphAboutRule wm
offerItems = notImplementedRule "offer items to writing a paragraph about rule"

useInitialAppearance :: LocaleParagraphAboutRule wm
useInitialAppearance = notImplementedRule "use initial appearance in room descriptions rule"

useInitialAppearanceOnSupporters :: LocaleParagraphAboutRule wm
useInitialAppearanceOnSupporters = notImplementedRule "initial appearance on supporters rule"

describeOnScenery :: LocaleParagraphAboutRule wm
describeOnScenery = notImplementedRule "describe what's on scenery supporters in room descriptions rule"

describeOnMentionedSupporters :: LocaleParagraphAboutRule wm
describeOnMentionedSupporters = notImplementedRule "describe what's on mentioned supporters in room descriptions rule"

setPronounsFromItems :: LocaleParagraphAboutRule wm
setPronounsFromItems = notImplementedRule "set pronouns from items in room descriptions rule"