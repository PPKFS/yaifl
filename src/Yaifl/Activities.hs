module Yaifl.Activities
(
    doActivity, addBaseActivities,

    makeActivityEx, makeActivity,
    doActivity',
    printingNameOfADarkRoomName, printingDescriptionOfADarkRoomName,

    describingLocaleActivityName, --choosingNotableLocaleObjectsActivityName,
    --LocaleDescription(..),
    printNameEx, printName,
    capitalThe,
    printingLocaleParagraphAboutActivityImpl, printingLocaleParagraphAboutActivityName
) where

import Yaifl.Prelude
import Yaifl.Common
import Yaifl.Components
import Yaifl.Rulebooks
import Yaifl.Utils
import Colog
import qualified Data.Set as DS
import qualified Data.IntMap.Strict as DM
import qualified Data.List as DL


addActivity :: (Show v, WithGameData w m) => Activity w v -> m ()
addActivity ac = do
    activityStore . at (_activityName ac) ?= BoxedActivity ac

addBaseActivities :: WithStandardWorld w m => m ()
addBaseActivities = do
    addActivity printingNameOfADarkRoomImpl
    addActivity printingNameOfSomethingImpl
    addActivity printingDescriptionOfADarkRoomImpl
    addActivity describingLocaleActivityImpl


makeActivityEx :: Text -> (Int -> Bool) -> ([Entity] -> Maybe v) -> [Rule w v RuleOutcome] ->
    [Rule w v RuleOutcome] -> [Rule w v RuleOutcome] -> Activity w v
makeActivityEx n appliesTo setVars before forRules after = Activity n appliesTo (\v -> makeRulebook "set activity variables rulebook" [Rule "set activity variables" (return $ setVars v)])
                        (makeRulebookWithVariables "before activity rulebook" before)
                        (makeRulebookWithVariables "for activity rulebook" forRules)
                        (makeRulebookWithVariables "after activity rulebook" after)

makeActivity :: Text -> (Int -> Bool, [Entity] -> Maybe v) -> RuleEvaluation w v -> Activity w v
makeActivity n (app, setVars) fo = makeActivityEx n app setVars [] [RuleWithVariables "" fo] []

doActivity' :: WithGameData w m => Text -> m (Maybe RuleOutcome)
doActivity' n = doActivity n []

doActivity :: WithGameData w m => Text -> [Entity] -> m (Maybe RuleOutcome)
doActivity n args = do
    --sayDbgLn $ "Running activity " <> n
    ac <- use $ activityStore . at n
    maybe (do
        logError $ "couldn't find activity " <> n
        return $ Just False)
          (\(BoxedActivity (Activity _ app setRb bef fo aft)) -> do
              iv <- if app (length args) then runRulebook (setRb args) else return Nothing
              maybe (do
                  logError "Couldn't parse activity arguments.."
                  return $ Just False
                ) (\x -> do
                    (r1, _) <- runRulebookEx (bef x)
                    ry <- (runRulebookEx . fo) $ fromMaybe x r1
                    rz <- (runRulebookEx . aft) $ fromMaybe x (fst ry)
                    return (snd rz)
                  ) iv) ac

printingNameOfADarkRoomName :: Text
printingNameOfADarkRoomName = "printing the name of a dark room activity"
printingNameOfADarkRoomImpl :: Activity w ()
printingNameOfADarkRoomImpl = makeActivity printingNameOfADarkRoomName ignoreArgs (do
            say "Darkness"
            return Nothing)

printingDescriptionOfADarkRoomName :: Text
printingDescriptionOfADarkRoomName = "printing the description of a dark room activity"
printingDescriptionOfADarkRoomImpl :: Activity w ()
printingDescriptionOfADarkRoomImpl = makeActivity printingDescriptionOfADarkRoomName ignoreArgs (do
            sayLn "It is pitch dark, and you can't see a thing."
            return Nothing)

printingNameOfSomethingName :: Text
printingNameOfSomethingName = "printing the name of something activity"
printingNameOfSomethingImpl :: forall w. HasObjectStore w => Activity w Entity
printingNameOfSomethingImpl = makeActivity printingNameOfSomethingName singleArg (do
        e <- getRulebookVariables
        o <- getComponent @(Object w) e
        traverse_ say (_name <$> o)
        return $ Just True)

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
--printingLocaleParagraphAboutActivityImpl :: HasStd w w => UncompiledActivity w Bool Entity
printingLocaleParagraphAboutActivityImpl :: forall w. HasStandardWorld w => Activity w (Entity, Int)
printingLocaleParagraphAboutActivityImpl = makeActivityEx printingLocaleParagraphAboutActivityName (==1) singleArgAugmented []
        [
            RuleWithVariables "don’t mention player’s supporter in room descriptions rule" (do
                playerID <- getPlayer
                (e, _) <- getRulebookVariables
                playerEnclosedBy <- playerID `isEnclosedBy` e
                when playerEnclosedBy (setLocalePriority e 0)
                return Nothing
                ),
            RuleWithVariables "don’t mention scenery in room descriptions rule" (do
                (e, _) <- getRulebookVariables
                isScenery <- e `isType` "scenery"
                when isScenery (setLocalePriority e 0)
                return Nothing),
            RuleWithVariables "don’t mention undescribed items in room descriptions rule" (do
                (e, _) <- getRulebookVariables
                isDescribed <- isX @(Physical w) NotDescribed _described e
                unless isDescribed (setLocalePriority e 0)
                return Nothing),
            RuleWithVariables "offer items to writing a paragraph about rule" (do
                (e, _) <- getRulebookVariables
                ism <- isMentioned e
                unless ism (do
                    res <- doActivity writingParagraphAboutName [e]
                    when (res == Just True) (do
                        modifyRulebookVariables (\(e', v) -> (e', v+1))
                        --adjustComponent @(Physical w) e (\x -> x & set mentioned True))
                        mentionThing e))
                return Nothing),

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

            RuleWithVariables "use initial appearance in room descriptions rule" (do
                (e, _) <- getRulebookVariables
                phy <- getComponent @(Physical w) e
                -- double maybeM - first ensuring we have a physical component, then seeing if we have an initial appearance to evaluate
                ism <- isMentioned e
                desc <- maybeM "" (maybeM "" (evalDescription' e) . _initialAppearance) phy
                unless (desc == "" || maybe False _handled phy) (do
                    say desc
                    modifyRulebookVariables (\(e', v) -> (e', v+1))
                    enclosing <- getComponent @Enclosing e
                    isSup <- e `isType` "supporter"
                    -- get the things (maybe) on the supporter
                    let enc = if isSup then maybe DS.empty _encloses enclosing else DS.empty
                    -- filter to just things that are locale supported
                    ls <- filterM isLocaleSupported (toList enc)
                    --mark everything for listing, except if it's been mentioned
                    --in which case we...unmark it?
                    -- originally I ignored the first part but idk why
                    mapM_ (\itemOnSupporter -> do
                        adjustComponent @(Physical w) itemOnSupporter (markedForListing .~ True)
                        whenM (isMentioned itemOnSupporter)
                            (adjustComponent @(Physical w) itemOnSupporter (markedForListing .~ False))
                        ) ls
                    unless (null ls) (do
                        say "On "
                        modifyRulebookVariables (\(e', v) -> (e', v+1))
                        printNameEx e (SayOptions Definite Uncapitalised)
                        doActivity listingContentsOfSomethingName [e, markedOnlyFlag, noConcealedFlag]
                        adjustComponent @(Physical w) e (markedForListing .~ True)
                        say "."
                        paragraphBreak
                        )
                    pass)
                return Nothing)]

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
paragraphBreak = say "\n\n"

isLocaleSupported :: forall w m. WithStandardWorld w m => Entity -> m Bool
isLocaleSupported it = do
    isScenery <- it `isType` "scenery"
    ism <- isMentioned it
    isDescribed <- isX @(Physical w) NotDescribed _described it
    return $ not $ isScenery || ism|| isDescribed

isMentioned :: WithGameData w m => Entity -> m Bool
isMentioned e = uses (localeData . mentionedThings) (e `DS.member`)

data ListingContentsArgs = ListingContentsArgs
    {
        _entity :: Entity,
        _isMarkedOnly :: Bool,
        _isIgnoringConcealed :: Bool
    } deriving (Eq, Show)

listingContentsArgs :: (Int -> Bool, [Entity] -> Maybe ListingContentsArgs)
listingContentsArgs = (\x -> x >=(1 :: Int) && x < 4, \case
    [x] -> Just $ ListingContentsArgs x False False
    x:xs -> Just $ ListingContentsArgs x (markedOnlyFlag `elem` xs) (noConcealedFlag `elem` xs)
    _ -> Nothing)

listingContentsOfSomethingImpl :: forall w. HasStandardWorld w => Activity w ListingContentsArgs
listingContentsOfSomethingImpl = makeActivity listingContentsOfSomethingName listingContentsArgs (do
    ListingContentsArgs e markedOnly ignoreConcealedItems <- getRulebookVariables
    l <- getComponent @Enclosing e
    let markedPred x = (do
            ml <- isX @(Physical w) True _markedForListing x
            return $ not markedOnly || ml)
        ignoreConcealedPred x = (do
            c <- isConcealed x
            return $ not ignoreConcealedItems || c)
        ls = toList . _encloses <$> l
    contentsToList <- maybe (return []) (filterM (\x -> do
        m <- markedPred x
        c <- ignoreConcealedPred x
        return $ m && c)) ls
    mapM_ (\(a, v) -> do
        printNameEx a (SayOptions Indefinite Uncapitalised)
        when (v < length contentsToList - 1) (say ", ")
        when (v == length contentsToList - 2) (say "and ")
        mentionThing a
        ) $ zip (toList contentsToList) [0..]
    return $ Just True
    )

mentionThing :: forall w m. WithStandardWorld w m => Entity -> m ()
mentionThing e = do
    -- adjustComponent @(Physical w) e (mentioned .~ True)
    localeData . mentionedThings %= DS.insert e
    pass

-- | have I just missed this off somewhere
-- TODO: keep a list of mentioned things so we can clear them later
writingParagraphAboutName :: Text
writingParagraphAboutName = "writing a paragraph about activity"

data LocaleDescription = LocaleDescription {
      _domain :: Entity
    , _paragraphCount :: Int } deriving (Eq, Show)

describeLocaleArgs :: [Entity] -> Maybe LocaleDescription
describeLocaleArgs [x] = Just $ LocaleDescription x 0
describeLocaleArgs _ = Nothing

describingLocaleActivityImpl :: Activity w LocaleDescription
describingLocaleActivityImpl = makeActivityEx describingLocaleActivityName (==1) describeLocaleArgs [] [
    RuleWithVariables "initialise locale description rule" (do
        clearLocale
        return Nothing
        ),
    RuleWithVariables "find notable objects rule" (do
        LocaleDescription e _ <- getRulebookVariables
        doActivity choosingNotableLocaleObjectsActivityName [e]
        ),
    RuleWithVariables "interesting locale paragraphs rule" (do
        localeTable <- use $ localeData . localePriorities
        let sorted = sort (DM.elems localeTable)
        logDebug $ "Found a total of " <> fromString (show $ length sorted) <> " potentially interesting things"
        res <- mapM (doActivity printingLocaleParagraphAboutActivityName . one) sorted
        modifyRulebookVariables (\(LocaleDescription d p) -> LocaleDescription d (p + length (filter (Just True==) res)))
        return Nothing
        ),
    RuleWithVariables "you can also see rule" (do
        LocaleDescription e p <- getRulebookVariables
        ld <- use localeData
        -- lp is everything that has a locale priority
        let lp =  (DM.keys . DM.filter (>0)) (ld ^. localePriorities)
        -- we then partition the list into things that have been mentioned and those which have not
            (ment, notMent) = DL.partition (`DS.member` (ld ^. mentionedThings)) lp
        return Nothing)
        {-
                    
                        
                        sayNm x = do say x; printName p (SayOptions Definite Uncapitalised); say " you "
                        setMarked e' b = component' physicalComponent e' . markedForListing .= b
                    mapM_ (`setMarked` False) ment
                    mapM_ (`setMarked` True) notMent
                    -- so we only care about things marked for listing...which should be, at this point,
                    -- only things in the not mentioned list.
                    unless (null notMent) (do
                        if  | isComponent w roomComponent p -> if p == playerLocation' w
                                then say "You " 
                                else sayNm "In "
                            -- TODO: replace the second supporter with animal
                            | isComponent w supporterComponent p || isComponent w supporterComponent p -> sayNm "On "
                            | otherwise -> sayNm "In "
                        say "can "
                        when (e > 0) $ say "also "
                        say "see "
                        --I'm going to completely ignore what inform does here because trying to parse their
                        --object list handling is a pain. 
                        --so instead I think it makes the most sense, to me, to run two groupings
                        --first, identical things should be grouped as "there are 2 Xs"
                        --no idea how to decide if two things are equal.
                        --inform decries this as "they have identical parser rhetoric"
                        --then see if anything wants to tag itself as part of a group (groupablecomponent)
                        --and then group them according to that?

                        --first group the marked for listing elements
                        --then group the groups by the grouping
                        --this second thing can be a TODO.
                        let groupedList = groupBy (groupingEquivalenceRelation w) notMent
                        mapM_ (\(a, v) -> do
                            case a of
                                [] -> pass
                                [e'] -> do printName e' (SayOptions Indefinite Uncapitalised); pass
                                e':_ -> do say $ show $ length a; printName e' (SayOptions Indefinite Uncapitalised); pass
                            when (v < length groupedList - 1) (say ", ")
                            when (v == length groupedList - 2) (say "and ")) $ zip groupedList [0..]
                        when (p == playerLocation' w) (say " here")
                        say ".\n\n"
                        )
                    return Nothing -}
    ] []

choosingNotableLocaleObjectsActivityName :: Text
choosingNotableLocaleObjectsActivityName = "choosing notable locale objects activity"

choosingNotableLocaleObjectsActivityImpl :: HasStandardWorld w => Activity w Entity
choosingNotableLocaleObjectsActivityImpl = makeActivity choosingNotableLocaleObjectsActivityName singleArg (do
    e <- getRulebookVariables
    encl <- getComponent @Enclosing e
    whenJust encl (mapM_ (`setLocalePriority` 5) . _encloses)
    return Nothing)

{-
groupingEquivalenceRelation :: HasStd u w => u -> Entity -> Entity -> Bool
groupingEquivalenceRelation u e1 e2 = 
    if | hasChildren u e1 && willRecurse u e1 -> False
       | hasChildren u e2 && willRecurse u e2 -> False
       --something about having omit contents from listing here
       | isWorn u e1 /= isWorn u e2 -> False
       | isLit u e1 /= isLit u e2 -> False
       | isMatchingContainers u e1 e2 -> False --are identically named TODO
       | otherwise -> getName u e1 == getName u e2

hasChildren :: (HasComponent u w Enclosing) => u -> Entity -> Bool
hasChildren u = ifMaybe (not . null . _encloses) . getComponent u enclosingComponent

willRecurse :: (HasComponent u w Supporter, HasComponent u w Openable, HasComponent u w Container) => u -> Entity -> Bool
willRecurse u e1 = isComponent u supporterComponent e1 || 
                        ifMaybe (\e -> _opacity e == Transparent || getComponent' u openableComponent e1 == Open)
                                        (getComponent u containerComponent e1)

isMatchingContainers :: (HasComponent u w Openable) => u -> Entity -> Entity -> Bool
isMatchingContainers u e1 e2 = getComponent u openableComponent e2 == getComponent u openableComponent e1

--there's something about making sure they can be pluralised which I'm ignoring
--if it has children and won't make the parser recurse?
--the parser will recurse if "always" is true (idfk) or if it's a supporter
--or if it's an open/transparent container.
--if one is worn or is giving off light and the other isn't
--if one's a container and the other isn't and the containers are different


combineStuff [] _ _ ys rj = pass
combineStuff ((_, Nothing):xs) h f ys rj = pass

combineStuff ((e, Just p):xs) h f ys rj = if (_enclosedBy p) \= h 
    then 
        if isNothing h
            then combineStuff xs (_enclosedBy p) f ((e, Just p):ys) rj
            else combineStuff xs (_enclosedBy p) False ys ((e, Just p):rj)
    else 

-}