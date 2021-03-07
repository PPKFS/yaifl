module Yaifl.Activities
(
    doActivity, addBaseActivities,

    makeActivityEx, makeActivity,
    doActivity',
    printingNameOfADarkRoomName, --printingDescriptionOfADarkRoomName,
    {-
    describingLocaleActivityName, choosingNotableLocaleObjectsActivityName,
    LocaleDescription(..),
    printName', printName,
    capitalThe,
    printingNameOfSomethingImpl, printingDescriptionOfADarkRoomImpl, printingNameOfADarkRoomImpl,
     describingLocaleActivityImpl, choosingNotableLocaleObjectsActivityImpl,
    printingLocaleParagraphAboutActivityImpl, printingLocaleParagraphAboutActivityName-}
) where

import Yaifl.Prelude
import Yaifl.Common
import Yaifl.Components
import Yaifl.Rulebooks
import Colog


addActivity :: (Show v, Monad m) => Activity w v m -> World w m ()
addActivity ac = do
    activityStore . at (_activityName ac) ?= BoxedActivity ac

addBaseActivities :: Monad m => World w m ()
addBaseActivities = do
    addActivity printingNameOfADarkRoomImpl

makeActivityEx :: Monad m => Text -> Int -> ([Entity] -> Maybe v) -> [Rule w v m RuleOutcome] ->
    [Rule w v m RuleOutcome] -> [Rule w v m RuleOutcome] -> Activity w v m
makeActivityEx n appliesTo setVars before forRules after = Activity n appliesTo (\v -> makeRulebook "set activity variables rulebook" [Rule "set activity variables" (return $ setVars v)])
                        (makeRulebookWithVariables "before activity rulebook" before)
                        (makeRulebookWithVariables "for activity rulebook" forRules)
                        (makeRulebookWithVariables "after activity rulebook" after)

makeActivity :: Monad m => Text -> (Int, [Entity] -> Maybe v) -> RuleEvaluation w v m -> Activity w v m
makeActivity n (app, setVars) fo = makeActivityEx n app setVars [] [RuleWithVariables "" fo] []

doActivity' :: Monad m => Text -> World w m (Maybe RuleOutcome)
doActivity' n = doActivity n []

doActivity :: WithGameLog w m' m => Text -> [Entity] -> m (Maybe RuleOutcome)
doActivity n args = do
    --sayDbgLn $ "Running activity " <> n
    ac <- use $ activityStore . at n
    maybe (do
        logError $ "couldn't find activity " <> n
        return $ Just False)
          (\(BoxedActivity (Activity _ app setRb bef fo aft)) -> do
              iv <- if length args == app then runRulebook (setRb args) else return Nothing
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
printingNameOfADarkRoomImpl :: Monad m => Activity w () m
printingNameOfADarkRoomImpl = makeActivity printingNameOfADarkRoomName ignoreArgs (do
            say "Darkness"
            return Nothing)

printingDescriptionOfADarkRoomName :: Text
printingDescriptionOfADarkRoomName = "printing the description of a dark room activity"
printingDescriptionOfADarkRoomImpl :: Monad m => Activity w () m
printingDescriptionOfADarkRoomImpl = makeActivity printingDescriptionOfADarkRoomName ignoreArgs (do
            sayLn "It is pitch dark, and you can't see a thing."
            return Nothing)

printingNameOfSomethingName :: Text
printingNameOfSomethingName = "printing the name of something activity"
printingNameOfSomethingImpl :: (HasStore w Object, Monad m) => Activity w Entity m
printingNameOfSomethingImpl = makeActivity printingNameOfSomethingName singleArg (do
        e <- getRulebookVariables 
        o <- getComponent @Object e
        traverse_ say (_name <$> o)
        return $ Just True)


data SayOptions = NoOptions | SayOptions Article Capitalisation
data Article = Indefinite | Definite
data Capitalisation = Capitalised | Uncapitalised

noSayOptions :: SayOptions
noSayOptions = NoOptions

capitalThe :: SayOptions
capitalThe = SayOptions Definite Capitalised

class HasID e where
    getID :: e -> Entity
printName :: HasID e => e -> m ()
printName o = printNameEx o noSayOptions

printNameEx ::  HasID e => e -> SayOptions -> m ()
printNameEx o p = do
    let pr = doActivity printingNameOfSomethingName [getID o]
    case p of
        NoOptions -> pr 
        SayOptions Indefinite Capitalised -> do say "A "; pr
        SayOptions Definite Capitalised -> do say "The "; pr
        SayOptions Indefinite Uncapitalised -> do say "a "; pr
        SayOptions Definite Uncapitalised -> do say "the "; pr
    pass
{-
printingLocaleParagraphAboutActivityName :: Text
printingLocaleParagraphAboutActivityName = "printing locale paragraph about activity"

printingLocaleParagraphAboutActivityImpl :: HasStd w w => UncompiledActivity w Bool Entity
printingLocaleParagraphAboutActivityImpl = Activity printingLocaleParagraphAboutActivityName (const False)
        (\r1 -> makeRulebook "" r1 [])
        (\r1 -> makeRulebook "" r1 [
            makeRule "don’t mention player’s supporter in room descriptions rule" (do
                (w, (e, _)) <- get
                when (isEnclosedBy w (getPlayer' w) e) (setLocalePriority e 0)
                return Nothing),
            makeRule "don’t mention scenery in room descriptions rule" (do
                (w, (e, _)) <- get
                when (isComponent w sceneryComponent e) (setLocalePriority e 0)
                return Nothing),
            makeRule "don’t mention undescribed items in room descriptions rule" (do
                (w, (e, _)) <- get
                when (isX NotDescribed _described physicalComponent w e) (setLocalePriority e 0)
                return Nothing),
            makeRule "offer items to writing a paragraph about rule" (do
                (w, (e, _)) <- get
                when (isX False _mentioned physicalComponent w e) (do
                    res <- doActivity writingParagraphAboutName [e]
                    when (res == Just True) (do
                        mentionedLens' e .= True
                        _2 . _2 .= True))
                return Nothing),
            makeRule "use initial appearance in room descriptions rule" (do
                (_, (e, _)) <- get
                phy <- use $ component' physicalComponent e
                unless (_mentioned phy || _handled phy || isNothing (_initialAppearance phy)) (do
                    (w', _) <- get
                    whenJust (fmap (getDescription w' e) (_initialAppearance phy)) (\d' -> unless (d' == "") (do say $ show d'; _2 . _2 .= True))
                    let isSup = getComponent w' supporterComponent e
                        --get the things (maybe) on the supporter
                        enc = fmap (const $ _encloses $ getComponent' w' enclosingComponent e) isSup
                        -- filter to just things that are locale supported
                        locSuppStuff = fmap (DS.filter (\it -> not $ isComponent w' sceneryComponent it || isX True _mentioned physicalComponent w' it || isX NotDescribed _described physicalComponent w' it)) enc
                        isAnyStuff = (\ e' -> if null e' then Nothing else Just e') =<< locSuppStuff
                    --get rid of any mentioned flags if they're still around
                    whenJust enc (mapM_ (\i -> when (isX True _mentioned physicalComponent w' i) 
                            (physicalLens' i . markedForListing .= False)
                        ))
                    whenJust isAnyStuff (\_ -> do
                        say "On "
                        
                        _2 . _2 .= True
                        _ <- printName e (SayOptions Definite Uncapitalised)
                        doActivity listingContentsOfSomethingName [e, markedOnlyFlag, noConcealedFlag]
                        --mentionedLens' e .= True
                        say ".\n\n"
                        )
                    pass
                    )
                return Nothing),
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
                        {-
                        
                        -}
                        )
                    pass
                    )
                return Nothing)
        ])
        (\r1 -> makeRulebook "" r1 [])

data ListingContentsArgs = ListingContentsArgs
    {
        _entity :: Entity,
        _isMarkedOnly :: Bool,
        _isIgnoringConcealed :: Bool
    } deriving (Eq, Show)

instance ActionArgs ListingContentsArgs where
    unboxArguments [] = Nothing
    unboxArguments [x] = Just $ ListingContentsArgs x False False
    unboxArguments (x:xs) = Just $ ListingContentsArgs x (Relude.elem markedOnlyFlag xs) (Relude.elem noConcealedFlag xs)
    defaultActionArguments = ListingContentsArgs (-100) False False

listingContentsOfSomethingName :: Text
listingContentsOfSomethingName = "listing the contents of something activity"

listingContentsOfSomethingImpl :: HasStd w w => UncompiledActivity w () ListingContentsArgs
listingContentsOfSomethingImpl = makeActivity' listingContentsOfSomethingName (do
    (w, ListingContentsArgs e mo igcon) <- get
    l <- use $ component' enclosingComponent e
    let markedPred x = not mo || isX True _markedForListing physicalComponent w x
        ignoreConPred x = not igcon || isX Nothing _concealedBy physicalComponent w x
        stuff = DS.filter (\x -> markedPred x && ignoreConPred x) $ _encloses l
    mapM_ (\(a, v) -> do
        printName a (SayOptions Indefinite Uncapitalised)
        when (v < length stuff - 1) (say ", ")
        when (v == length stuff - 2) (say "and ")
        component' physicalComponent a . mentioned .= True
        ) $ zip (toList stuff) [0..]
    return $ Just True
    )
markedOnlyFlag :: Entity
markedOnlyFlag = -690

noConcealedFlag :: Entity
noConcealedFlag = -691

writingParagraphAboutName :: Text
writingParagraphAboutName = "writing a paragraph about activity"

describingLocaleActivityName :: Text
describingLocaleActivityName = "printing the locale description of something"
newtype LocaleDescription = LocaleDescription { _paragraphCount :: Int }
makeLenses ''LocaleDescription
describingLocaleActivityImpl :: HasStd w w => UncompiledActivity w LocaleDescription Entity
describingLocaleActivityImpl = Activity describingLocaleActivityName (const $ LocaleDescription 0) 
        (\r1 -> makeRulebook "" r1 [
                makeRule "initialise locale description rule" 
                    (do world . gameInfo . localePriorities .= Map.empty; return Nothing),
                makeRule "find notable objects rule" (do
                    doActivity choosingNotableLocaleObjectsActivityName [fst r1]
                    return Nothing)])
        (\r1 -> makeRulebook "" r1 [
                makeRule "interesting locale paragraphs rule" (do
                    lp <- use $ world . gameInfo . localePriorities
                    sayDbgLn $ show lp
                    let sorted = Relude.sortBy (\(_, a) (_, b) -> compare a b) (Map.toAscList lp)
                    sayDbgLn $ "Found a total of " <> fromString (show $ length sorted) <> " potentially interesting things"
                    res <- mapM (doActivity printingLocaleParagraphAboutActivityName . one . fst) sorted
                    _2 . _2 . paragraphCount += length (filter (Just True==) res)
                    return Nothing
                    ),
                makeRule "you can also see rule" (do
                    (w, (p, LocaleDescription e)) <- get
                    -- lp is everything that has a locale priority
                    let lp = Map.keys $ Map.filter (>0) (w ^. gameInfo . localePriorities) 
                    -- we then partition the list into things that have been mentioned and those which have not
                        (ment, notMent) = partition (_mentioned . getComponent' w physicalComponent) lp
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
                    return Nothing
                    )
            ])
        (\r1 -> makeRulebook "" r1 [])

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

choosingNotableLocaleObjectsActivityName :: Text
choosingNotableLocaleObjectsActivityName = "choosing notable locale objects activity"

choosingNotableLocaleObjectsActivityImpl :: HasStd w w => UncompiledActivity w () Entity
choosingNotableLocaleObjectsActivityImpl = makeActivity' choosingNotableLocaleObjectsActivityName (do
    (w, e) <- get    
    let r = getComponent w enclosingComponent e
    whenJust r (mapM_ (\e' -> do
        setLocalePriority e' 5
        component' physicalComponent e' . mentioned .= False) . _encloses)
    return Nothing
    )


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