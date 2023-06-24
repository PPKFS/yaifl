module Yaifl.Lamp.Activities.PrintingTheLocaleDescription
( printingTheLocaleDescriptionImpl
, WithPrintingTheLocaleDescription
, youCanAlsoSeeAImpl
, youCanAlsoSeeBImpl
, youCanAlsoSeeCImpl
) where

import Solitude

import Breadcrumbs
import Data.Text.Display
import Data.List ( groupBy )
import Yaifl.Core.Actions.Activity
import Yaifl.Core.Entity ( Store(unStore), Entity )
import Yaifl.Core.Object
import Yaifl.Core.Objects.Query
import Yaifl.Core.Objects.ThingData
import Yaifl.Core.Properties.Enclosing ( Enclosing(..) )
import Yaifl.Core.Properties.Has ( WMHasProperty )
import Yaifl.Core.Properties.Query ( getEnclosing )
import Yaifl.Core.Rules.Rule
import Yaifl.Core.Rules.Rulebook ( Rulebook(..), blankRulebook )
import Yaifl.Core.Print
import Yaifl.Lamp.Say
import Yaifl.Lamp.Properties.Container
import Yaifl.Lamp.Properties.Openable ( Openable(..), getOpenable )
import qualified Data.EnumMap.Strict as DEM
import qualified Data.EnumSet as DES
import Yaifl.Lamp.Activities.ChoosingNotableLocaleObjects
import Yaifl.Lamp.Activities.PrintingLocaleParagraphAbout
import Yaifl.Lamp.Locale
import Yaifl.Core.Rules.RuleEffects
import Yaifl.Core.Responses
import Yaifl.Core.SayQQ


type WithPrintingTheLocaleDescription wm = (
  WithChoosingNotableLocaleObjects wm
  , WithPrintingNameOfSomething wm
  , WithListingNondescriptItems wm
  , WithPrintingLocaleParagraphAbout wm
  , WithResponse wm "youCanAlsoSeeA" ()
  , WithResponse wm "youCanAlsoSeeC" ()
  , WithResponse wm "youCanAlsoSeeB" ()
  , WithActivity "printingTheLocaleDescription" wm (LocaleVariables wm) ())

printingTheLocaleDescriptionImpl ::
  WMHasProperty wm Enclosing
  => WMHasProperty wm Container
  => WMHasProperty wm Openable
  => WithPrintingTheLocaleDescription wm
  => Activity wm (LocaleVariables wm) ()
printingTheLocaleDescriptionImpl = Activity "Printing the locale description of something" Nothing Nothing
  (blankRulebook "Before printing the locale description")
  ((blankRulebook "Carry out printing the locale description")
    { rules =
      [ findNotable
      , interestingLocale
      , alsoSee]
    })
  (blankRulebook "After printing the locale description")

findNotable ::
  WithChoosingNotableLocaleObjects wm
  => Rule wm (LocaleVariables wm) r
findNotable = Rule "Find notable objects" [] (\v ->
  do
    -- carry out the choosing notable locale objects activity with the domain;
    o <- doActivity #choosingNotableLocaleObjects (v ^. #domain)
    return ((\x -> set #localePriorities x v) <$> o, Nothing))

interestingLocale ::
  WithPrintingLocaleParagraphAbout wm
  => Rule wm (LocaleVariables wm) r
interestingLocale = Rule "Interesting locale paragraphs" [] (\v ->
  do
    let tb = v ^. #localePriorities
        --sort the Table of Locale Priorities in locale description priority order;
        sorted = sortBy (compare `on` priority) (toList $ unStore tb)
    addTag "interesting things" (length sorted)
    -- carry out the printing a locale paragraph about activity with the notable-object entry;
    -- for each thing, we offer it to write a paragraph
    -- then it is either no longer needed to be written about (Just Mentioned)
    -- mentioned, but still hanging around (Just Unmentioned)
    -- or ignored (Nothing)
    -- the printing a locale paragraph activity will instead modify
    -- the locale variables and pass those through
    newP <- foldlM (\v' li -> do
        r <- doActivity #printingLocaleParagraphAbout (v', li)
        return $ fromMaybe v' r) v sorted
    addTag "interesting things after printingLocaleParagraphAbout" (length (unStore $ localePriorities newP))
    return (Just newP, Nothing))

youCanAlsoSeeAImpl :: Response wm ()
youCanAlsoSeeAImpl = Response $ const [sayingTell|Darkness|]

youCanAlsoSeeCImpl :: Response wm ()
youCanAlsoSeeCImpl = Response $ const [sayingTell|Darkness|]

youCanAlsoSeeBImpl :: Response wm ()
youCanAlsoSeeBImpl = Response $ const [sayingTell|Darkness|]

alsoSee ::
  WMHasProperty wm Enclosing
  => WMHasProperty wm Container
  => WMHasProperty wm Openable
  => WithPrintingNameOfSomething wm
  => WithListingNondescriptItems wm
  => WithResponse wm "youCanAlsoSeeA" ()
  => WithResponse wm "youCanAlsoSeeC" ()
  => WithResponse wm "youCanAlsoSeeB" ()
  => Rule wm (LocaleVariables wm) r
alsoSee = Rule "You can also see" [] (\v ->
  do
    -- lp is everything that has a locale priority
    -- i.e. things which haven't been removed as they've been mentioned
    -- or otherwise been removed (by setting priority to 0 somehow)
    -- and it's not already mentioned.
    let lp = DEM.filter (\(LocaleInfo x _ m) -> x > 0 && not m) (unStore $ v ^. #localePriorities)

      -- if the domain is a thing and the domain holds the player and the domain is falsely-unoccupied, continue the activity;
      -- TODO: I have no idea what this is meant to guard against.
      -- https://ganelson.github.io/inform/standard_rules/S-act.html#SP26
      -- each item in the table of lps contributes 1 to the mentionable count (maybe it's meant to only be things with lp > 0).
      -- these are marked for listing.
      -- if we have mentionables, then we unmark the ones which are already mentioned. this is the filter above.

      --if the number of marked for listing things is 0:
      --      abandon the listing nondescript items activity with the domain;
      -- unless (null lp) $ do


    -- begin the listing nondescript items activity with the domain; 
    beginActivity #listingNondescriptItems (v ^. #domain)
    -- if the number of marked for listing things is 0:
    -- abandon the listing nondescript items activity with the domain;
    when (null lp) $ void $ endActivity #listingNondescriptItems
    -- if handling the listing nondescript items activity with the domain:
    whenHandling' #listingNondescriptItems $ do
      let (LocaleVariables prior dom p) = v
      pl <- getCurrentPlayer
      playerLocE <- getLocation pl
      plRoom <- getRoomMaybe playerLocE
      isASupporter <- dom `isType` "supporter"
      isAnAnimal <- dom `isType` "animal"

      let isInLoc = maybe False (dom `objectEquals`) plRoom
      if
        --if the domain is the location: say "[We] " (A);
        | isRoom dom && isInLoc ->
          sayResponse #youCanAlsoSeeA ()
        -- otherwise if the domain is a supporter or the domain is an animal:
        -- say "On [the domain] [we] " (B);
        | isASupporter || isAnAnimal ->
          sayResponse #youCanAlsoSeeB ()
        -- otherwise: say "In [the domain] [we] " (C);
        | otherwise ->
          sayResponse #youCanAlsoSeeC ()
      -- if the locale paragraph count is greater than 0:
      --say "[regarding the player][can] also see " (D);
      {-
      otherwise:
                    say "[regarding the player][can] see " (E);
                let the common holder be nothing;
                let contents form of list be true;
                repeat with list item running through marked for listing things:
                    if the holder of the list item is not the common holder:
                        if the common holder is nothing,
                            now the common holder is the holder of the list item;
                        otherwise now contents form of list is false;
                    if the list item is mentioned, now the list item is not marked for listing;
                filter list recursion to unmentioned things;
                if contents form of list is true and the common holder is not nothing,
                    list the contents of the common holder, as a sentence, including contents,
                        giving brief inventory information, tersely, not listing
                        concealed items, listing marked items only;
                otherwise say "[a list of marked for listing things including contents]";
                if the domain is the location, say " here" (F);
                say ".[paragraph break]";
                unfilter list recursion;
            end the listing nondescript items activity with the domain;
    continue the activity.


      -}
    {-





      addTag "player location" plRoom
      let
      {- -}
      printText "can "
      when (p > 0) $ printText "also "
      printText "see "
      --I'm going to completely ignore what inform does here because trying to parse their
      --object list handling is a pain.
      --so instead I think it makes the most sense, to me, to run two groupings
      --first, identical things should be grouped as "there are 2 Xs"
      --no idea how to decide if two things are equal.
      --inform decries this as "they have identical parser rhetoric"
      --then see if anything wants to tag itself as part of a group (groupablecomponent)
      --and then group them according to that?)

      --current me: thank god past me left notes because otherwise I would've gone and dug into
      -- the inform6 source again and cried

      --first group the marked for listing elements
      --then group the groups by the grouping
      --this second thing can be a TODO.
      {- groupingProps <- mapM (getGroupingProperties . localeObject . snd) (DEM.toList (unStore prior))
      let groupedList = groupBy groupingEquivalenceRelation groupingProps

      mapM_
        ( \(objGrp, num) -> do
            case objGrp of
              [] -> pass -- nothing to print
              [e'] -> do
                printName (grpObj e')
                pass
              e' : _ -> do
                addTag "Group of multiple objects: " (e', num)
                printText $ show $ length objGrp
                printName (grpObj e')
                pass
            when (num < length groupedList - 1) (printText ", ")
            when (num == length groupedList - 2) (printText "and ")
        )
        $ zip groupedList [0 ..]
      when isInLoc (printText " here")
      paragraphBreak -} -}
    return (Nothing, Nothing))
