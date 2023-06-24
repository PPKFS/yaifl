module Yaifl.Lamp.Activities.DescribingLocale
( describingLocaleImpl
, WithDescribingLocale
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


    newP <- foldlM (\v' li -> do
        r <- doActivity #printingLocaleParagraphAbout (v', li)
        return $ fromMaybe v' r) v sorted
    addTag "interesting things after printingLocaleParagraphAbout" (length (unStore $ localePriorities newP))
    return (Just newP, Nothing))

sayDomain ::
  RuleEffects wm es
  => ObjectLike wm o
  => WithPrintingNameOfSomething wm
  => Text
  -> o
  -> Eff es ()
sayDomain x e = do
  printText x
  printName e
  printText " you "


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

instance Display (GroupingProperties wm) where
  displayBuilder = const "group properties"

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
  let gwb = join $ mbThing ^? _Just % #objectData % #wearable % #_Wearable
      gtl = mbThing ^? _Just % #objectData % #lit
  (ic, op) <- getContainerProps o
  let n = display $ o ^. #name -- TODO
  return $ GroupingProperties o n hc wr gwb (Just Lit == gtl) op ic

hasChildren
  :: NoMissingObjects wm es
  => WMHasProperty wm Enclosing
  => ObjectLike wm o
  => o
  -> Eff es Bool
hasChildren e = maybe False (\e' -> DES.size (contents e') == 0) <$> getEnclosing e

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
  Print :> es
  => Eff es ()
paragraphBreak = printText ".\n\n"

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
