module Yaifl.Game.Activities.PrintingTheLocaleDescription
( printingTheLocaleDescriptionImpl
, WithPrintingTheLocaleDescription
, YouCanAlsoSeeResponses(..)
, youCanAlsoSeeResponsesImpl
) where

import Yaifl.Prelude

import Breadcrumbs

import Yaifl.Model.Activity
import Yaifl.Text.AdaptiveNarrative (regardingThePlayer)
import Yaifl.Model.Store ( Store(unStore) )
import Yaifl.Model.Kinds.Object
import Yaifl.Model.Query
import Yaifl.Text.Responses
import Yaifl.Model.Rules.RuleEffects
import Yaifl.Model.Rules.Rulebook ( Rulebook(..), blankRulebook, Rule (..) )
import Yaifl.Text.SayQQ
import Yaifl.Game.Activities.ChoosingNotableLocaleObjects
import Yaifl.Game.Activities.PrintingLocaleParagraphAbout
import Yaifl.Game.Actions.Looking.Locale
import Yaifl.Text.Say
import qualified Data.EnumMap.Strict as DEM
import Yaifl.Text.ListWriter
import Yaifl.Model.Metadata
import Yaifl.Model.Kinds.AnyObject
import Yaifl.Model.Kinds.Thing (thingContainedBy)

data YouCanAlsoSeeResponses =
  YouCanAlsoSeeA
  | YouCanAlsoSeeB
  | YouCanAlsoSeeC
  | YouCanAlsoSeeD
  | YouCanAlsoSeeE
  | YouCanAlsoSeeF
  deriving stock (Generic)

type WithPrintingTheLocaleDescription wm = (
  WithChoosingNotableLocaleObjects wm
  , WithListWriting wm
  , WithListingNondescriptItems wm
  , WithPrintingLocaleParagraphAbout wm
  , WithActivity "printingTheLocaleDescription" wm YouCanAlsoSeeResponses (LocaleVariables wm) ()
  )

youCanAlsoSeeResponsesImpl ::
  WithPrintingNameOfSomething wm
  => YouCanAlsoSeeResponses
  -> Response wm (LocaleVariables wm)
youCanAlsoSeeResponsesImpl = \case
  YouCanAlsoSeeA -> Response $ const [sayingTell|#{We} |]
  YouCanAlsoSeeB -> Response $ \LocaleVariables{domain} -> [sayingTell|On {the domain} #{we} |]
  YouCanAlsoSeeC -> Response $ \LocaleVariables{domain} -> [sayingTell|In {the domain} #{we} |]
  YouCanAlsoSeeD -> Response $ const $ do
    regardingThePlayer
    [sayingTell|#{can} also see |]
  YouCanAlsoSeeE -> Response $ const $ do
    regardingThePlayer
    [sayingTell|#{can} see |]
  YouCanAlsoSeeF -> Response $ const [sayingTell| here|]

printingTheLocaleDescriptionImpl ::
  WithPrintingTheLocaleDescription wm
  => Activity wm YouCanAlsoSeeResponses (LocaleVariables wm) ()
printingTheLocaleDescriptionImpl = Activity "Printing the locale description of something" Nothing Nothing
  youCanAlsoSeeResponsesImpl
  ((blankRulebook "before printing the locale description") { rules = [ findNotable ] })
  ((blankRulebook "carry out printing the locale description")
    { rules =
      [ interestingLocale
      , alsoSee
      ]
    })
  (blankRulebook "After printing the locale description")
  (const)

findNotable ::
  WithChoosingNotableLocaleObjects wm
  => ActivityRule wm YouCanAlsoSeeResponses (LocaleVariables wm) r
findNotable = Rule "find notable objects" [] (\v ->
  do
    -- carry out the choosing notable locale objects activity with the domain;
    o <- doActivity #choosingNotableLocaleObjects (v ^. #domain)
    return ((\x -> set #localePriorities x v) <$> o, Nothing))

interestingLocale ::
  WithPrintingLocaleParagraphAbout wm
  => ActivityRule wm YouCanAlsoSeeResponses (LocaleVariables wm) r
interestingLocale = Rule "Interesting locale paragraphs" [] (\v ->
  do
    let tb = v ^. #localePriorities
        --sort the Table of Locale Priorities in locale description priority order;
        sorted = sortBy (compare `on` priority) (toList $ unStore tb)
    addTag "interesting things" (length sorted)
    -- for each thing, we offer it to write a paragraph
    -- then it is either no longer needed to be written about (Just Mentioned)
    -- mentioned, but still hanging around (Just Unmentioned)
    -- or ignored (Nothing)
    -- the printing a locale paragraph activity will instead modify
    -- the locale variables and pass those through
    newP <- foldlM (\v' li -> do
        o <- getObject (localeObject li)
        addAnnotation $ "printing locale paragraph about " <> display (view #name o)
        r <- doActivity #printingLocaleParagraphAbout (v', li)
        return $ fromMaybe v' r) v sorted
    addTag "interesting things after printingLocaleParagraphAbout" (length (unStore $ localePriorities newP))
    return (Just newP, Nothing))

alsoSee ::
  WithPrintingTheLocaleDescription wm
  => ActivityRule wm YouCanAlsoSeeResponses (LocaleVariables wm) r
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
      isASupporter <- dom `isKind` "supporter"
      isAnAnimal <- dom `isKind` "animal"

      let isInLoc = maybe False (dom `objectEquals`) plRoom
      if
        --if the domain is the location: say "[We] " (A);
        | isInLoc ->
          sayResponse YouCanAlsoSeeA v
        -- otherwise if the domain is a supporter or the domain is an animal:
        -- say "On [the domain] [we] " (B);
        | isASupporter || isAnAnimal ->
          sayResponse YouCanAlsoSeeB v
        -- otherwise: say "In [the domain] [we] " (C);
        | otherwise ->
          sayResponse YouCanAlsoSeeC v
      -- if the locale paragraph count is greater than 0:
      if paragraphCount > 0
        --say "[regarding the player][can] also see " (D);
        then sayResponse YouCanAlsoSeeD v
        -- otherwise: say "[regarding the player][can] see " (E);
        else sayResponse YouCanAlsoSeeE v
      -- there is a big mess of looping to see if everything
      -- has a common parent and therefore we are listing the contents
      -- of something. this will happen unless the author
      -- manually adds some notable object that isn't present in the room
      let allHolders = DEM.foldl' (\xs li -> flip cons xs $ asThingOrRoom
            (Just . thingContainedBy)
            (const Nothing) (toAny $ localeObject li)) [] lp
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
      when (isRoom dom && isInLoc) $ sayResponse YouCanAlsoSeeF v
      --say ".[paragraph break]";
      [saying|.#{paragraphBreak}|]
    endActivity #listingNondescriptItems
    return (Nothing, Nothing))
