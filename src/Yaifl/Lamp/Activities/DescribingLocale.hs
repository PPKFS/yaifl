{-|
Module      : Yaifl.Actions.Action
Description : An action is a verb that is carried out by the player (or an NPC).
Copyright   : (c) Avery, 2022
License     : MIT
Maintainer  : ppkfs@outlook.com
Stability   : No
-}
module Yaifl.Lamp.Activities.DescribingLocale
( describingLocaleImpl
) where

import qualified Data.EnumMap.Strict as DEM
import Yaifl.Lamp.Activities.PrintingNameOfSomething
import Yaifl.Core.Common
import Data.List (groupBy)
import qualified Data.EnumSet as DES
import Yaifl.Core.Properties.Property
import Yaifl.Core.Actions.Activity
import Yaifl.Core.Properties.Enclosing
import Yaifl.Lamp.Properties.Container
import Yaifl.Lamp.Properties.Openable
import Yaifl.Core.Rulebooks.Rulebook
import Yaifl.Core.Objects.Object

import Yaifl.Core.Say
import Yaifl.Core.Properties.Query
import Yaifl.Core.Logger
import Yaifl.Core.Objects.ObjectData
import Yaifl.Core.Objects.Query
import Yaifl.Core.Rulebooks.Rule

describingLocaleImpl ::
  WMHasProperty s Enclosing
  => WMHasProperty s Container
  => WMHasProperty s Openable
  => Activity s (LocaleVariables s) ()
describingLocaleImpl = Activity "Describing the locale of something" Nothing
  (blankRulebook "Before describing locale")
  ((blankRulebook "Carry out describing locale")
    { _rbRules =
      [ findNotable
      , interestingLocale
      , alsoSee]
    })
  (blankRulebook "After describing locale")

findNotable :: Rule s (LocaleVariables s) r
findNotable = Rule "Find notable objects" (\v ->
  do
    -- by default, pick every object in the domain and assign them '5'
    o <- doActivity choosingNotableLocaleObjects (v ^. localeDomain)
    return ((\x -> set localePriorities x v) <$> o, Nothing))

interestingLocale :: Rule s (LocaleVariables s) r
interestingLocale = Rule "Interesting locale paragraphs" (\v ->
  do
    let tb = v ^. localePriorities
        sorted = sortBy (compare `on` _priority) (toList $ unStore tb)
    debug $ "Found a total of " <> fromString (show $ length sorted) <> " potentially interesting things"
    --for each thing, we offer it to write a paragraph
    --then it is either no longer needed to be written about (Just Mentioned)
    --mentioned, but still hanging around (Just Unmentioned)
    --or ignored (Nothing)
    -- update: so now the printing a locale paragraph activity will instead modify 
    --the locale variables and pass those through
    newP <- foldlM (\v' li -> do
        r <- doActivity printingLocaleParagraphAbout (v', li)
        return $ fromMaybe v' r) v sorted
    debug $ "After handing off to printingLocaleParagraphAbout, we still have "
      <> fromString (show $ length (unStore $ _localePriorities newP)) <> " potentially interesting things"
    return (Just newP, Nothing))

sayDomain ::
  NoMissingObjects wm es
  => RuleEffects wm es
  => ObjectLike wm o
  => Text
  -> o
  -> Eff es ()
sayDomain x e = do
  say x
  printNameEx (SayOptions Definite Uncapitalised) e 
  say " you "

alsoSee ::
  WMHasProperty s Enclosing
  => WMHasProperty s Container
  => WMHasProperty s Openable
  => Rule s (LocaleVariables s) r
alsoSee = Rule "You can also see" (\v ->
  do
    -- lp is everything that has a locale priority
    -- i.e. things which haven't been removed as they've been mentioned
    -- or otherwise been removed (by setting priority to 0 somehow)
    -- I think now we're including the mentioned flag it's worth putting in here too
    let lp = DEM.filter (\(LocaleInfo x _ m) -> x > 0 && not m) (unStore $ v ^. localePriorities)

    unless (null lp) $ do
      let (LocaleVariables prior dom p) = v
      plID <- use currentPlayer
      isASupporter <- dom `isType` "supporter"
      isAnAnimal <- dom `isType` "animal"
      playerLocE <- getLocation plID
      plRoom <- getRoomMaybe playerLocE
      let isInLoc = maybe False (dom `objectEquals`) plRoom
      if
        | isRoom dom ->
          if isInLoc then say "You " else sayDomain "In " dom
        | isASupporter || isAnAnimal -> sayDomain "On " dom
        | otherwise -> sayDomain "In " dom
      say "can "
      when (p > 0) $ say "also "
      say "see "
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
      groupingProps <- mapM (getGroupingProperties . _localeObject . snd) (DEM.toList (unStore prior))
      let groupedList = groupBy groupingEquivalenceRelation groupingProps

      mapM_
        ( \(objGrp, num) -> do
            case objGrp of
              [] -> pass -- nothing to print
              [e'] -> do
                printNameEx (SayOptions Indefinite Uncapitalised) (grpObj e') 
                pass
              e' : _ -> do
                say $ show $ length objGrp
                printNameEx (SayOptions Indefinite Uncapitalised) (grpObj e') 
                pass
            when (num < length groupedList - 1) (say ", ")
            when (num == length groupedList - 2) (say "and ")
        )
        $ zip groupedList [0 ..]
      when isInLoc (say " here")
      paragraphBreak
    return (Nothing, Nothing))

--something about having omit contents from listing here
--are identically named TODO??? about matching containers???
groupingEquivalenceRelation ::
  GroupingProperties s
  -> GroupingProperties s
  -> Bool
groupingEquivalenceRelation e1 e2 =
  --so they can be put in the same group as long as none of these properties hold
  --and ALSO they have the same name.
  (grpName e1 == grpName e2) &&
  --either both are equal containers, or both are not containers
  --if neither are containers but one is open somehow, also don't group them
  isMatchingContainers e1 e2 &&
  not (
    --if either will give rise to a "in the blah you can also see..."
    (grpHasChildren e1 && grpWillRecurse e1)
      || (grpHasChildren e2 && grpWillRecurse e2)
      --one is worn by Dave and the other by Bob
      || (grpWornBy e1 /= grpWornBy e2)
      --one is lit and one is not
      || (grpIsThingLit e1 /= grpIsThingLit e2))

isMatchingContainers ::
  GroupingProperties s
  -> GroupingProperties s
  -> Bool
isMatchingContainers e1 e2 = (grpIsContainer e1 == grpIsContainer e2) && (grpOpenable e1 == grpOpenable e2)

data GroupingProperties s = GroupingProperties
  { grpObj :: AnyObject s
  , grpName :: Text
  , grpHasChildren :: Bool
  , grpWillRecurse :: Bool
  , grpWornBy :: Maybe Entity
  , grpIsThingLit :: Bool
  , grpIsContainer :: Bool
  , grpOpenable :: Maybe Openable
  }

getGroupingProperties ::
  NoMissingObjects wm es
  => WMHasProperty wm Enclosing
  => WMHasProperty wm Container
  => WMHasProperty wm Openable
  => AnyObject wm
  -> Eff es (GroupingProperties wm)
getGroupingProperties o = do
  hc <- hasChildren o
  wr <- willRecurse o
  mbThing <- getThingMaybe o
  let gwb = join $ mbThing ^? _Just % objData % thingWearable % _Wearable
      gtl = mbThing ^? _Just % objData % thingLit
  (ic, op) <- getContainerProps o
  n <- evalName o
  return $ GroupingProperties o n hc wr gwb (Just Lit == gtl) op ic

hasChildren
  :: NoMissingObjects wm es
  => WMHasProperty wm Enclosing
  => ObjectLike wm o
  => o
  -> Eff es Bool
hasChildren e = maybe False (\e' -> DES.size (_enclosingContains e') == 0) <$> getEnclosing e

willRecurse ::
  NoMissingObjects wm es
  => WMHasProperty wm Container
  => ObjectLike wm o
  => o
  -> Eff es Bool
willRecurse e = do
  o <- getObject e
  isSup <- o `isType` "supporter"
  cont <- getContainer e
  return $ isSup || maybe False (\c -> _containerOpacity c == Transparent || _containerOpenable c == Open) cont

paragraphBreak ::
  Saying :> es
  => Eff es ()
paragraphBreak = say ".\n\n"

--p2275 of the complete program, in B/lwt
getContainerProps ::
  NoMissingObjects wm es
  => ObjectLike wm (Object wm d)
  => WMHasProperty wm Openable
  => Object wm d
  -> Eff es (Maybe Openable, Bool)
getContainerProps e1 = do
  o <- getOpenable e1
  o1c <- e1 `isType` "container"
  return (o, o1c)

{-}

isLocaleSupported :: forall w m. WithStandardWorld w m => Entity -> m Bool
isLocaleSupported it = do
  isScenery <- it `isType` "scenery"
  ism <- isMentioned it
  isd <- isDescribed it
  return $ not $ isScenery || ism || isd

isMentioned :: WithGameData w m => Entity -> m Bool
isMentioned e = uses (localeData . mentionedThings) (e `DS.member`)

data ListingContentsArgs = ListingContentsArgs
  { _entity :: Entity,
    _isMarkedOnly :: Bool,
    _isIgnoringConcealed :: Bool
  }
  deriving (Eq, Show)

listingContentsArgs :: (Int -> Bool, [Entity] -> Maybe ListingContentsArgs)
listingContentsArgs =
  ( \x -> x >= (1 :: Int) && x < 4,
    \case
      [x] -> Just $ ListingContentsArgs x False False
      x : xs -> Just $ ListingContentsArgs x (markedOnlyFlag `elem` xs) (noConcealedFlag `elem` xs)
      _ -> Nothing
  )

listingContentsOfSomethingImpl :: forall w. HasStandardWorld w => Activity w ListingContentsArgs
listingContentsOfSomethingImpl =
  makeActivity
    listingContentsOfSomethingName
    listingContentsArgs
    ( do
        ListingContentsArgs e markedOnly ignoreConcealedItems <- getRulebookVariables
        l <- (toList . _encloses) <<$>> getComponent @Enclosing e
        contentsToList <-
          maybe
            (return [])
            ( filterM
                ( \x -> do
                    ml <- isX @(Physical w) True _markedForListing x
                    c <- isConcealed x
                    return $ (not markedOnly || ml) && (not ignoreConcealedItems || c)
                )
            )
            l
        mapM_
          ( \(a, v) -> do
              printNameEx a (SayOptions Indefinite Uncapitalised)
              when (v < length contentsToList - 1) (say ", ")
              when (v == length contentsToList - 2) (say "and ")
              mentionThing a
          )
          $ zip (toList contentsToList) [0 ..]
        return $ Just True
    )
-}
