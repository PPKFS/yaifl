module Yaifl.Activities.PrintingLocaleParagraphAbout
  ( WithPrintingLocaleParagraphAbout
  , printingLocaleParagraphAboutImpl
  ) where

import Solitude

import Yaifl.Activities.Activity
import Yaifl.Model.Object
import Yaifl.Model.Objects.Query (getThingMaybe)
import Yaifl.Model.Objects.ThingData
import Yaifl.Rules.Rule ( Rule, makeRule )
import Yaifl.Rules.Rulebook ( Rulebook(..), blankRulebook )
import Yaifl.Actions.Looking.Locale
import Yaifl.Model.Entity

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

type WithPrintingLocaleParagraphAbout wm = WithActivity "printingLocaleParagraphAbout" wm (LocaleVariables wm, LocaleInfo wm) (LocaleVariables wm)
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
  ((blankRulebook "Carry out printing a locale paragraph")
    { rules =
      [ dontMentionUndescribed
      , dontMentionSupporter
      , dontMentionScenery
      , offerItems
      , useInitialAppearance
      , describeOnScenery
      ]
    })
  (blankRulebook "After printing a locale paragraph")

-- normally this just removes "you can see yourself"
dontMentionUndescribed :: LocaleParagraphAboutRule wm
dontMentionUndescribed = makeRule "don’t mention undescribed items in room descriptions rule" []
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
          asThing <- getThingMaybe e
          let isDesc = asThing ^? _Just % #objectData % #described
          if
            isDesc == Just Undescribed
          then
            return . Just $ removeFromLocale e v --setLocalePriority e v 0
          else
            return Nothing
  )

{-
      [ Rule
        "don’t mention player’s supporter in room descriptions rule"
        ( \v -> runState $ do
            playerID <- gets _currentPlayer
            get1st >>= (\e -> whenM (playerID `isEnclosedBy` e) (setLocalePriority e 0))
            return (v, Nothing)
        ),
        Rule
        "don’t mention scenery in room descriptions rule"
        ( do
            get1st >>= (\e -> whenM (e `isType` "scenery") (setLocalePriority e 0))
            return Nothing
        ),
        Rule
        "offer items to writing a paragraph about rule"
        ( do
            e <- get1st
            unlessM
              (isMentioned e)
              ( do
                  whenM
                    (doActivity writingParagraphAboutName [e] <==> return (Just True))
                    ( do
                        modifyRulebookVariables (\(e', v) -> (e', v + 1))
                        mentionThing e
                    )
              )
            return Nothing
        )
      -- oh boy this looks LONG
      -- if it's not mentioned (otherwise we ignore it)
      -- if it has an initial appearance and it is not handled
      -- we have to do the "" check just in case the initial description is dynamic
      -- increase the locale paragraph count - we only care if it's >0
      -- say the initial appearance and a paragraph break
      -- if a locale-supportable thing is on the item (???) then mark all of them for listing
      -- locale supportable is not scenery, not mentioned, not undescribed
      -- if any of them are mentioned, unmark them
      -- say "On the item..." and list everything, full stop, paragraph break, mention it
      Rule
        "use initial appearance in room descriptions rule"
        ( do
            e <- get1st
            phy <- getPhysical e
            -- double maybeM - first ensuring we have a physical component, then seeing if we have an initial appearance to evaluate
            ism <- isMentioned e
            unless ism $ do
              desc <- maybeM "" (maybeM "" (evalDescription' e) . _initialAppearance) phy
              unless
                (desc == "" || (fmap _handled phy == Just NotHandled))
                ( do
                    say desc
                    modifyRulebookVariables (\(e', v) -> (e', v + 1))
                    enclosing <- getComponent @Enclosing e
                    isSup <- e `isType` "supporter"
                    -- get the things (maybe) on the supporter
                    let enc = if isSup then maybe DS.empty _encloses enclosing else DS.empty
                    -- filter to just things that are locale supported
                    ls <- filterM isLocaleSupported (toList enc)
                    --mark everything for listing, except if it's been mentioned
                    --in which case we...unmark it?
                    -- originally I ignored the first part but idk why
                    mapM_
                      ( \itemOnSupporter -> do
                          adjustComponent @(Physical w) itemOnSupporter (markedForListing .~ True)
                          whenM
                            (isMentioned itemOnSupporter)
                            (adjustComponent @(Physical w) itemOnSupporter (markedForListing .~ False))
                      )
                      ls
                    unless
                      (null ls)
                      ( do
                          say "On "
                          modifyRulebookVariables (\(e', v) -> (e', v + 1))
                          printNameEx e (SayOptions Definite Uncapitalised)
                          doActivity listingContentsOfSomethingName [e, markedOnlyFlag, noConcealedFlag]
                          adjustComponent @(Physical w) e (markedForListing .~ True)
                          say "."
                          paragraphBreak
                      )
                    pass
                )
            return Nothing
        )
    ]

                makeRule " describe what’s on scenery supporters in room descriptions rule" (do
                    (w, (e, _)) <- get
                    phy <- use $ component' physicalComponent e
                    when (_scenery phy && not (ifMaybe (DS.member (getPlayer' w) . _encloses) (getComponent w enclosingComponent e))) (do
                        let isSup = getComponent w supporterComponent e
                            enc = fmap (const $ _encloses $ getComponent' w enclosingComponent e) isSup
                            locSuppStuff = fmap (DS.filter (\it -> not $ isComponent w sceneryComponent it || isX True _mentioned physicalComponent w it || isX NotDescribed _described physicalComponent w it)) enc
                            isAnyStuff = (\ e' -> if null e' then Nothing else Just e') =<< locSuppStuff
                        --whenJust enc (mapM_ (\i -> when (isX True _mentioned physicalComponent w i) (component' physicalComponent i . markedForListing .= False)))

                        whenJust isAnyStuff (\_ -> do -- we don't care if there's actual stuff, we do that later.
                            say "On "
                            _2 . _2 .= True
                            _ <- printName e (SayOptions Definite Uncapitalised)
                            doActivity listingContentsOfSomethingName [e, markedOnlyFlag, noConcealedFlag]
                            mentionedLens' e .= True
                            say ".\n\n"
                        pass
-}