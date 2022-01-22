module Yaifl.Activities.DescribingLocale
( describingLocaleImpl
) where
import Yaifl.Types
import Yaifl.Activities.Common
import Yaifl.Prelude
import qualified Data.EnumMap.Strict as DEM
import Yaifl.ObjectLookup
import Yaifl.Activities.PrintingNameOfSomething
import Yaifl.Messages

describingLocaleImpl :: Activity s (LocaleVariables s) ()
describingLocaleImpl = Activity "Describing the locale of something" Nothing
  (blankRulebook "Before describing locale")
  ((blankRulebook "Carry out describing locale")
    { _rbRules = 
      [ findNotable
      , interestingLocale]
      --, alsoSee]
    })
  (blankRulebook "After describing locale")


findNotable :: Rule s (LocaleVariables s) r
findNotable = Rule "Find notable objects" (\v ->
  do
    -- by default, pick every object in the domain and assign them '5'
    o <- doActivity choosingNotableLocaleObjects (v ^. localeDomain)
    return (maybe v (\x -> v & localePriorities .~ x) o, Nothing))

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
        r <- doActivity printingLocaleParagraphAbout li
        return $ fromMaybe v' r) v sorted
    return (newP, Nothing))

alsoSee :: forall s r. Rule s (LocaleVariables s) r
alsoSee = Rule "You can also see" (\v ->
  do
    -- lp is everything that has a locale priority
    -- i.e. things which haven't been removed as they've been mentioned
    -- or otherwise been removed (by setting priority to 0 somehow)
    let lp = DEM.filter (\(LocaleInfo x _ m) -> x > 0 && m) (unStore $ v ^. localePriorities)
        sayDomain :: ObjectLike s o => Text -> o -> m ()
        sayDomain x e = do say x; printNameEx e (SayOptions Definite Uncapitalised); say " you "

    unless
      (null lp)
      ( do
          let (LocaleVariables prior dom p) = v
              isARoom = isRoom dom
          plID <- gets _currentPlayer
          isASupporter <- gets $ dom `isType` ObjType "supporter"
          isAnAnimal <- gets $ dom `isType` ObjType "animal"
          playerLocE <- getLocation plID
          plRoom <- maybe (return Nothing) getRoom playerLocE
          let isInLoc = maybe False (dom `eqObject`) plRoom
          if
            | isARoom ->
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
          w <- get
          let groupedList = groupBy (groupingEquivalenceRelation w) (map (snd . snd) (DEM.toList (unStore prior)))
          mapM_
            ( \(a, v') -> do
                case a of
                  [] -> pass -- nothing to print
                  [e'] -> do printNameEx e' (SayOptions Indefinite Uncapitalised); pass
                  e' : _ -> do say $ show $ length a; printNameEx e' (SayOptions Indefinite Uncapitalised); pass
                when (v' < length groupedList - 1) (say ", ")
                when (v' == length groupedList - 2) (say "and ")
            )
            $ zip groupedList [0 ..]
          when isInLoc (say " here")
          paragraphBreak
      )
    return (v, Nothing))

{-

removeFromLocale
  :: Entity
  -> LocaleVariables s
  -> LocaleVariables s
removeFromLocale o = localePriorities % at o .~ Nothing

type HasLocaleProperties s = (HasProperty s Enclosing, HasProperty s Container, HasProperty s Openable, HasProperty s ThingLit)
--something about having omit contents from listing here
--are identically named TODO??? about matching containers???
groupingEquivalenceRelation
  :: HasLocaleProperties s
  => ObjectLike s o1
  => ObjectLike s o2
  => World s
  -> o1
  -> o2
  -> Bool
groupingEquivalenceRelation w e1 e2 = not (
    (hasChildren e1 w && willRecurse e1 w)
    || (hasChildren e2 w && willRecurse e2 w)
    || (getWornBy' e1 w /= getWornBy' e2 w)
    || (getThingLit' e1 w /= getThingLit' e2 w)
    || isMatchingContainers e1 e2 w)
    && (getName' e1 w == getName' e2 w)

--p2275 of the complete program, in B/lwt
isMatchingContainers
  :: HasProperty s Openable
  => ObjectLike s o1
  => ObjectLike s o2
  => o1
  -> o2
  -> World s
  -> Bool
isMatchingContainers e1 e2 = evalState $ do
  o <- getOpenable e1
  o2 <- getOpenable e2
  o1c <- gets $ e1 `isType` ObjType "container"
  o2c <- gets $ e2 `isType` ObjType "container"
  return $ (o1c == o2c) && o1c && (o == o2)

hasChildren
  :: HasProperty s Enclosing
  => ObjectLike s o
  => o
  -> World s
  -> Bool
hasChildren e = maybe False (\e' -> DES.size (_enclosingContains e') == 0) <$> getEnclosing' e

willRecurse
  :: HasProperty s Container
  => ObjectLike s o
  => o
  -> World s
  -> Bool
willRecurse e = evalState $ do
  isSup <- gets $ e `isType` ObjType "supporter"
  cont <- getContainer e
  return $ isSup || maybe False (\c -> _containerOpacity c == Transparent || _containerOpenable c == Open) cont

-}

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