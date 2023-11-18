module Yaifl.Activities.PrintingTheLocaleDescription
( printingTheLocaleDescriptionImpl
, WithPrintingTheLocaleDescription
, YouCanAlsoSeeResponses(..)
, youCanAlsoSeeResponsesImpl
) where

import Solitude

import Breadcrumbs
import Data.Text.Display

import Yaifl.Activities.Activity
import Yaifl.Text.AdaptiveNarrative (regardingThePlayer)
import Yaifl.Model.Entity ( Store(unStore) )
import Yaifl.Model.Object
import Yaifl.Model.Objects.Query
import Yaifl.Model.Objects.ThingData
import Yaifl.Text.Responses
import Yaifl.Rules.Rule
import Yaifl.Rules.RuleEffects
import Yaifl.Rules.Rulebook ( Rulebook(..), blankRulebook )
import Yaifl.Text.SayQQ
import Yaifl.Model.WorldModel
import Yaifl.Activities.ChoosingNotableLocaleObjects
import Yaifl.Activities.PrintingLocaleParagraphAbout
import Yaifl.Actions.Looking.Locale
import Yaifl.Text.Say
import qualified Data.EnumMap.Strict as DEM
import Yaifl.Text.ListWriter

data YouCanAlsoSeeResponses wm = YCAS
  { youCanAlsoSeeA :: Response wm ()
  , youCanAlsoSeeB :: Response wm (AnyObject wm)
  , youCanAlsoSeeC :: Response wm (AnyObject wm)
  , youCanAlsoSeeD :: Response wm ()
  , youCanAlsoSeeE :: Response wm ()
  , youCanAlsoSeeF :: Response wm ()
  } deriving stock (Generic)

type WithPrintingTheLocaleDescription wm = (
  WithChoosingNotableLocaleObjects wm
  , WithListWriting wm
  , WithListingNondescriptItems wm
  , WithPrintingLocaleParagraphAbout wm
  , WithResponseSet wm "youCanAlsoSee" (YouCanAlsoSeeResponses wm)
  , WithActivity "printingTheLocaleDescription" wm (LocaleVariables wm) ()
  )

youCanAlsoSeeResponsesImpl ::
  WithPrintingNameOfSomething wm
  => YouCanAlsoSeeResponses wm
youCanAlsoSeeResponsesImpl = YCAS
  { youCanAlsoSeeA = youCanAlsoSeeAImpl
  , youCanAlsoSeeB = youCanAlsoSeeBImpl
  , youCanAlsoSeeC = youCanAlsoSeeCImpl
  , youCanAlsoSeeD = youCanAlsoSeeDImpl
  , youCanAlsoSeeE = youCanAlsoSeeEImpl
  , youCanAlsoSeeF = youCanAlsoSeeFImpl
  }

sayYCASResponse ::
  WithResponseSet wm "youCanAlsoSee" (YouCanAlsoSeeResponses wm)
  => RuleEffects wm es
  => Lens' (YouCanAlsoSeeResponses wm) (Response wm v)
  -> v
  -> Eff es ()
sayYCASResponse l = sayResponse (#youCanAlsoSee % l)

printingTheLocaleDescriptionImpl ::
  WithPrintingTheLocaleDescription wm
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
youCanAlsoSeeAImpl = Response $ const [sayingTell|#{We} |]

youCanAlsoSeeBImpl ::
  WithPrintingNameOfSomething wm
  => Response wm (AnyObject wm)
youCanAlsoSeeBImpl = Response $ \domain -> [sayingTell|On {the domain} #{we} |]

youCanAlsoSeeCImpl ::
  WithPrintingNameOfSomething wm
  => Response wm (AnyObject wm)
youCanAlsoSeeCImpl = Response $ \domain -> [sayingTell|In {the domain} #{we} |]

youCanAlsoSeeDImpl :: Response wm ()
youCanAlsoSeeDImpl = Response $ const $ do
  regardingThePlayer
  [sayingTell|#{can} also see |]

youCanAlsoSeeEImpl ::
  Display (WMSayable wm)
  => Response wm ()
youCanAlsoSeeEImpl = Response $ const $ do
  regardingThePlayer
  [sayingTell|#{can} see |]

youCanAlsoSeeFImpl :: Response wm ()
youCanAlsoSeeFImpl = Response $ const [sayingTell| here|]

alsoSee ::
  WithPrintingTheLocaleDescription wm
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

    -- begin the listing nondescript items activity with the domain; 
    beginActivity #listingNondescriptItems (v ^. #domain)
    -- if the number of marked for listing things is 0:
    -- abandon the listing nondescript items activity with the domain;
    when (null lp) $ void $ endActivity #listingNondescriptItems
    -- if handling the listing nondescript items activity with the domain:
    whenHandling' #listingNondescriptItems $ do
      let (LocaleVariables _ dom paragraphCount) = v
      pl <- getCurrentPlayer
      playerLocE <- getLocation pl
      plRoom <- getRoomMaybe playerLocE
      isASupporter <- dom `isType` "supporter"
      isAnAnimal <- dom `isType` "animal"

      let isInLoc = maybe False (dom `objectEquals`) plRoom
      if
        --if the domain is the location: say "[We] " (A);
        | isInLoc ->
          sayYCASResponse #youCanAlsoSeeA ()
        -- otherwise if the domain is a supporter or the domain is an animal:
        -- say "On [the domain] [we] " (B);
        | isASupporter || isAnAnimal ->
          sayYCASResponse #youCanAlsoSeeB dom
        -- otherwise: say "In [the domain] [we] " (C);
        | otherwise ->
          sayYCASResponse #youCanAlsoSeeC dom
      -- if the locale paragraph count is greater than 0:
      if paragraphCount > 0
        --say "[regarding the player][can] also see " (D);
        then sayYCASResponse #youCanAlsoSeeD ()
        -- otherwise: say "[regarding the player][can] see " (E);
        else sayYCASResponse #youCanAlsoSeeE ()
      -- there is a big mess of looping to see if everything
      -- has a common parent and therefore we are listing the contents
      -- of something. this will happen unless the author
      -- manually adds some notable object that isn't present in the room
      let allHolders = DEM.foldl' (\xs li -> flip cons xs $ asThingOrRoom
            (\t -> Just $ t ^. #objectData % #containedBy)
            (const Nothing) (localeObject li)) [] lp
      case allHolders of
        -- no items
        [] -> error "impossible - no items found?"
        (x:xs) -> do
          let objects = map localeObject $ DEM.elems lp
          if all (== x) xs && isJust x
          -- list the contents of the common holder, as a sentence, including contents,
          -- giving brief inventory information, tersely, not listing
          -- concealed items, listing marked items only;
          -- this is via inter
          -- https://ganelson.github.io/inform/WorldModelKit/S-lst.html#SP17
          -- most of this is hell imho but we shall try
          then
            void $ doActivity #listingContents (withContents objects)
          else
            --otherwise say "[a list of marked for listing things including contents]";
            --which is the same as above ^ but not as an activity
            let listWithContents = (withContents objects) { asListingActivity = False } in
            [saying|{aListWithContents listWithContents}|]
      --if the domain is the location, say " here" (F);
      when (isRoom dom && isInLoc) $ sayYCASResponse #youCanAlsoSeeF ()
      --say ".[paragraph break]";
      [saying|.#{paragraphBreak}|]
    endActivity #listingNondescriptItems
    return (Nothing, Nothing))
