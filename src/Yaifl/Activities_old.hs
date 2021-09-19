module Yaifl.Activities
  ( doActivity,
    addBaseActivities,
    makeActivityEx,
    makeActivity,
    doActivity',
    printingNameOfADarkRoomName,
    printingDescriptionOfADarkRoomName,
    describingLocaleActivityName,
    printNameEx,
    printName,
    capitalThe,
    printingLocaleParagraphAboutActivityImpl,
    printingLocaleParagraphAboutActivityName,
  )
where

import Colog
import Control.Bool hiding (unlessM, whenM)
import qualified Control.Monad.ListM as LM
import qualified Data.IntMap.Strict as DM
import qualified Data.List as DL
import qualified Data.Set as DS
import Yaifl.Common
import Yaifl.Components
import Yaifl.Prelude
import Yaifl.Rulebooks
import Yaifl.Utils

addActivity :: (Show v, WithGameData w m) => Activity w v -> m ()
addActivity ac = activityStore . at (_activityName ac) ?= BoxedActivity ac

addBaseActivities :: WithStandardWorld w m => m ()
addBaseActivities = do
  addActivity printingNameOfADarkRoomImpl
  addActivity printingNameOfSomethingImpl
  addActivity printingDescriptionOfADarkRoomImpl
  addActivity describingLocaleActivityImpl
  addActivity choosingNotableLocaleObjectsActivityImpl
  addActivity printingLocaleParagraphAboutActivityImpl
  addActivity listingContentsOfSomethingImpl

makeActivityEx ::
  Text ->
  (Int -> Bool) ->
  ([Entity] -> Maybe v) ->
  [Rule w v RuleOutcome] ->
  [Rule w v RuleOutcome] ->
  [Rule w v RuleOutcome] ->
  Activity w v
makeActivityEx n appliesTo setVars before forRules after =
  Activity
    n
    appliesTo
    (\v -> makeRulebook "set activity variables rulebook" [Rule "set activity variables" (return $ setVars v)])
    (makeRulebookWithVariables "before activity rulebook" before)
    (makeRulebookWithVariables "for activity rulebook" forRules)
    (makeRulebookWithVariables "after activity rulebook" after)

makeActivity :: Text -> (Int -> Bool, [Entity] -> Maybe v) -> RuleEvaluation w v -> Activity w v
makeActivity n (app, setVars) fo = makeActivityEx n app setVars [] [RuleWithVariables "" fo] []

doActivity' :: WithGameData w m => Text -> m (Maybe RuleOutcome)
doActivity' n = doActivity n []

doActivity :: WithGameData w m => Text -> [Entity] -> m (Maybe RuleOutcome)
doActivity n args = do
  ac <- use $ activityStore . at n
  maybe
    ( do
        logError $ "couldn't find activity " <> n
        return $ Just False
    )
    ( \(BoxedActivity (Activity _ app setRb bef fo aft)) -> do
        iv <- if app (length args) then runRulebook (setRb args) else return Nothing
        maybe
          ( do
              logError "Couldn't parse activity arguments.."
              return $ Just False
          )
          ( \x -> do
              (r1, _) <- runRulebookEx (bef x)
              ry <- runRulebookEx . fo $ fromMaybe x r1
              rz <- runRulebookEx . aft $ fromMaybe x (fst ry)
              return (snd rz)
          )
          iv
    )
    ac

printingNameOfADarkRoomName :: Text
printingNameOfADarkRoomName = "printing the name of a dark room activity"

printingNameOfADarkRoomImpl :: Activity w ()
printingNameOfADarkRoomImpl =
  makeActivity
    printingNameOfADarkRoomName
    ignoreArgs
    ( do
        say "Darkness"
        return $ Just True
    )

printingDescriptionOfADarkRoomName :: Text
printingDescriptionOfADarkRoomName = "printing the description of a dark room activity"

printingDescriptionOfADarkRoomImpl :: Activity w ()
printingDescriptionOfADarkRoomImpl =
  makeActivity
    printingDescriptionOfADarkRoomName
    ignoreArgs
    ( do
        sayLn "It is pitch dark, and you can't see a thing."
        return $ Just True
    )

printingNameOfSomethingName :: Text
printingNameOfSomethingName = "printing the name of something activity"

printingNameOfSomethingImpl :: HasObjectStore w => Activity w Entity
printingNameOfSomethingImpl =
  makeActivity
    printingNameOfSomethingName
    singleArg
    ( do
        o <- getRulebookVariables >>= getObject'
        say (_name o)
        return $ Just True
    )

data SayOptions = NoOptions | SayOptions Article Capitalisation

data Article = Indefinite | Definite

data Capitalisation = Capitalised | Uncapitalised

noSayOptions :: SayOptions
noSayOptions = NoOptions

capitalThe :: SayOptions
capitalThe = SayOptions Definite Capitalised

printName :: (WithGameData w m, HasID w e m) => e -> m ()
printName o = printNameEx o noSayOptions

printNameEx :: (WithGameData w m, HasID w e m) => e -> SayOptions -> m ()
printNameEx o p = do
  e <- getID o
  let pr = doActivity printingNameOfSomethingName [e]
  case p of
    NoOptions -> pr
    SayOptions Indefinite Capitalised -> do say "A "; pr
    SayOptions Definite Capitalised -> do say "The "; pr
    SayOptions Indefinite Uncapitalised -> do say "a "; pr
    SayOptions Definite Uncapitalised -> do say "the "; pr
  pass

describingLocaleActivityName :: Text
describingLocaleActivityName = "describing locale activity"

printingLocaleParagraphAboutActivityName :: Text
printingLocaleParagraphAboutActivityName = "printing locale paragraph about activity"

singleArgAugmented :: [a] -> Maybe (a, Int)
singleArgAugmented [x] = Just (x, 0)
singleArgAugmented _ = Nothing

markedOnlyFlag :: Entity
markedOnlyFlag = -1000

noConcealedFlag :: Entity
noConcealedFlag = -1001

get1st :: Monad m => RuleVarsT (a, b) m a
get1st = fst <$> getRulebookVariables

--printingLocaleParagraphAboutActivityImpl :: HasStd w w => UncompiledActivity w Bool Entity
printingLocaleParagraphAboutActivityImpl :: forall w. HasStandardWorld w => Activity w (Entity, Int)
printingLocaleParagraphAboutActivityImpl =
  makeActivityEx
    printingLocaleParagraphAboutActivityName
    (== 1)
    singleArgAugmented
    []
    [ RuleWithVariables
        "don’t mention player’s supporter in room descriptions rule"
        ( do
            playerID <- getPlayer'
            get1st >>= (\e -> whenM (playerID `isEnclosedBy` e) (setLocalePriority e 0))
            return Nothing
        ),
      RuleWithVariables
        "don’t mention scenery in room descriptions rule"
        ( do
            get1st >>= (\e -> whenM (e `isType` "scenery") (setLocalePriority e 0))
            return Nothing
        ),
      RuleWithVariables
        "don’t mention undescribed items in room descriptions rule"
        ( do
            e <- get1st
            b <- isDescribed e
            unless b (setLocalePriority e 0)
            return Nothing
        ),
      RuleWithVariables
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
        ),
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

      RuleWithVariables
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
    {-
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
                        pass-}
    []

listingContentsOfSomethingName :: Text
listingContentsOfSomethingName = "listing contents of something activity"

paragraphBreak :: WithGameData w m => m ()
paragraphBreak = say ".\n\n"

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
