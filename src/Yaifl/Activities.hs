module Yaifl.Activities
  ( defaultActivities
    {-

    printingNameOfADarkRoomName,
    printingDescriptionOfADarkRoomName,
    describingLocaleActivityName,
    printNameEx,
    printName,
    capitalThe,
    printingLocaleParagraphAboutActivityImpl,
    printingLocaleParagraphAboutActivityName,-}
  )
where

import Yaifl.Types
import Yaifl.Properties (HasProperty)

import Yaifl.Activities.PrintingADarkRoom
import Yaifl.Activities.PrintingNameOfSomething
import Yaifl.Activities.PrintingDescriptionOfADarkRoom
import Yaifl.Activities.ChoosingNotableLocaleObjects
import Yaifl.Activities.PrintingLocaleParagraphAbout
import Yaifl.Activities.DescribingLocale

defaultActivities :: 
  HasProperty s Enclosing
  => HasProperty s Container 
  => HasProperty s Openable
  => ActivityCollection s
defaultActivities = ActivityCollection
  { printingNameOfADarkRoom = printingNameOfADarkRoomImpl
  , printingNameOfSomething = printingNameOfSomethingImpl
  , printingDescriptionOfADarkRoom = printingDescriptionOfADarkRoomImpl
  , choosingNotableLocaleObjects = choosingNotableLocaleObjectsImpl
  , printingLocaleParagraphAbout = printingLocaleParagraphAboutImpl
  , describingLocale = describingLocaleImpl
  }
{-}
listingContentsOfSomethingName :: Text
listingContentsOfSomethingName = "listing contents of something activity"

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

mentionThing :: forall w m. WithStandardWorld w m => Entity -> m ()
mentionThing e = do
  -- adjustComponent @(Physical w) e (mentioned .~ True)
  localeData . mentionedThings %= DS.insert e
  pass

writingParagraphAboutName :: Text
writingParagraphAboutName = "writing a paragraph about activity"

data LocaleDescription = LocaleDescription
  { _domain :: Entity,
    _paragraphCount :: Int
  }
  deriving (Eq, Show)

describeLocaleArgs :: [Entity] -> Maybe LocaleDescription
describeLocaleArgs [x] = Just $ LocaleDescription x 0
describeLocaleArgs _ = Nothing

describingLocaleActivityImpl :: forall w. HasStandardWorld w => Activity w LocaleDescription
describingLocaleActivityImpl =
  makeActivityEx
    describingLocaleActivityName
    (== 1)
    describeLocaleArgs
    []
    [ RuleWithVariables
        "initialise locale description rule"
        ( do
            clearLocale
            return Nothing
        ),
      RuleWithVariables
        "find notable objects rule"
        ( do
            LocaleDescription e _ <- getRulebookVariables
            doActivity choosingNotableLocaleObjectsActivityName [e]
        ),
      RuleWithVariables
        "interesting locale paragraphs rule"
        ( do
            localeTable <- use $ localeData . localePriorities
            let sorted = sortBy (compare `on` snd) (DM.toList localeTable)
            logDebug $ "Found a total of " <> fromString (show $ length sorted) <> " potentially interesting things"
            res <- mapM (doActivity printingLocaleParagraphAboutActivityName . one . fst) sorted
            modifyRulebookVariables (\(LocaleDescription d p) -> LocaleDescription d (p + length (filter (Just True ==) res)))
            return Nothing
        ),
      RuleWithVariables
        "you can also see rule"
        ( do
            LocaleDescription e p <- getRulebookVariables
            ld <- use localeData
            -- lp is everything that has a locale priority
            let lp = (DM.keys . DM.filter (> 0)) (ld ^. localePriorities)
                -- we then partition the list into things that have been mentioned and those which have not
                (ment, notMent) = DL.partition (`DS.member` (ld ^. mentionedThings)) lp
                sayDomain :: WithStandardWorld w m => Text -> m ()
                sayDomain x = do say x; printNameEx e (SayOptions Definite Uncapitalised); say " you "
                setMarked :: WithStandardWorld w m => Entity -> Bool -> m ()
                setMarked e' b = adjustComponent @(Physical w) e' (markedForListing .~ b)
            mapM_ (`setMarked` False) ment
            mapM_ (`setMarked` True) notMent
            -- so we only care about things marked for listing...which should be, at this point,
            -- only things in the not mentioned list.
            unless
              (null notMent)
              ( do
                  isARoom <- e `isType` "room"
                  isASupporter <- e `isType` "supporter"
                  isAnAnimal <- e `isType` "animal"
                  playerLoc <- getPlayer' >>= getLocation
                  if
                      | isARoom ->
                        if Just e == playerLoc then say "You " else sayDomain "In "
                      | isASupporter || isAnAnimal -> sayDomain "On "
                      | otherwise -> sayDomain "In "
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
                  groupedList <- LM.groupByM groupingEquivalenceRelation notMent
                  mapM_
                    ( \(a, v) -> do
                        case a of
                          [] -> pass -- nothing to print
                          [e'] -> do printNameEx e' (SayOptions Indefinite Uncapitalised); pass
                          e' : _ -> do say $ show $ length a; printNameEx e' (SayOptions Indefinite Uncapitalised); pass
                        when (v < length groupedList - 1) (say ", ")
                        when (v == length groupedList - 2) (say "and ")
                    )
                    $ zip groupedList [0 ..]
                  when (Just e == playerLoc) (say " here")
                  paragraphBreak
              )
            return Nothing
        )
    ]
    []

choosingNotableLocaleObjectsActivityName :: Text
choosingNotableLocaleObjectsActivityName = "choosing notable locale objects activity"

choosingNotableLocaleObjectsActivityImpl :: HasStandardWorld w => Activity w Entity
choosingNotableLocaleObjectsActivityImpl =
  makeActivity
    choosingNotableLocaleObjectsActivityName
    singleArg
    ( do
        encl <- getRulebookVariables >>= getComponent @Enclosing
        whenJust
          encl
          ( mapM_
              ( \x -> do
                  logDebug $ "Found a " <> show x
                  setLocalePriority x 5
              )
              . _encloses
          )
        return Nothing
    )

(</=>) :: (Eq a, Monad m) => m a -> m a -> m Bool
(</=>) a b = do
  x1 <- a
  x2 <- b
  return $ x1 /= x2

(<==>) :: (Eq a, Monad m) => m a -> m a -> m Bool
(<==>) a b = do
  x1 <- a
  x2 <- b
  return $ x1 == x2

--something about having omit contents from listing here
--are identically named TODO??? about matching containers???
groupingEquivalenceRelation :: WithStandardWorld w m => Entity -> Entity -> m Bool
groupingEquivalenceRelation e1 e2 =
  notM $
    (hasChildren e1 <&=> willRecurse e1)
      <|=> (hasChildren e2 <&=> willRecurse e2)
      <|=> (isWorn e1 </=> isWorn e2)
      <|=> (isLit e1 </=> isLit e2)
      <|=> isMatchingContainers e1 e2
      <|=> (getName e1 <==> getName e2)

hasChildren :: (HasStore w Enclosing, WithGameData w m) => Entity -> m Bool
hasChildren e = do
  enc <- getComponent @Enclosing e
  return $ maybe False (null . _encloses) enc

willRecurse :: (WithStandardWorld w m) => Entity -> m Bool
willRecurse e = do
  isSup <- e `isType` "supporter"
  cont <- getComponent @ContainerData e
  op' <- getComponent @Openable e
  return $ isSup || Just (ContainerData Transparent) == cont || Just Open == op'

isMatchingContainers :: (HasStore w Openable, WithGameData w m) => Entity -> Entity -> m Bool
isMatchingContainers e1 e2 = getComponent @Openable e2 <==> getComponent @Openable e1
-}