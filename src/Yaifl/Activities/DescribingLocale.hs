module Yaifl.Activities.DescribingLocale
( describingLocaleImpl
, choosingNotableLocaleObjectsImpl
, printingLocaleParagraphAboutImpl
, HasLocaleProperties
) where
import Yaifl.Common
import Yaifl.Activities.Common
import Yaifl.Prelude
import Yaifl.Messages
import Yaifl.Rulebooks
import qualified Data.Set as DS
import Yaifl.Properties
import Yaifl.ObjectLookup
import qualified Data.EnumMap as DEM
import qualified Data.EnumSet as DES
import Yaifl.Activities.PrintingNameOfSomething
import Data.List (groupBy)

choosingNotableLocaleObjectsImpl
  :: HasProperty s Enclosing
  => Activity s (AnyObject s) (LocalePriorities s)
choosingNotableLocaleObjectsImpl = makeActivity "Choosing notable locale objects"
     $ makeRule "" (\v -> runState $ do
        encl <- getEnclosing v
        maybe
            (do logError "Tried to choose notable locale objects from something that doesn't enclose "; return Nothing)
            (\e -> do
              l <- mapM
                (\x -> do
                  logVerbose $ "Found a " <> show x
                  getObject x)
                (DES.toList (_enclosingContains e))

              return $ Just $ Store $ DEM.fromList $ map (\x -> (getID x, (5,x))) (catMaybes l))
            encl)

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

findNotable :: Rule s (LocaleVariables s) r
findNotable = Rule "Find notable objects" (\v ->
  runState $
    do
      -- by default, pick every object in the domain and assign them '5'
      o <- doActivity choosingNotableLocaleObjects (v ^. localeDomain)
      return (maybe v (\x -> v & localePriorities .~ x) o, Nothing))

interestingLocale :: Rule s (LocaleVariables s) r
interestingLocale = Rule "Interesting locale paragraphs" (\v ->
  runState $
    do
      let tb = v ^. localePriorities
          sorted = sortBy (compare `on` fst) (toList $ unStore tb)
      logVerbose $ "Found a total of " <> fromString (show $ length sorted) <> " potentially interesting things"
      --for each thing, we offer it to write a paragraph
      --then it is either no longer needed to be written about (Just Mentioned)
      --mentioned, but still hanging around (Just Unmentioned)
      --or ignored (Nothing)
      newP <- foldlM (\v' (a, o) -> do
          r <- doActivity printingLocaleParagraphAbout (o, v')
          return $ fromMaybe v' r
          {-}
          case r of
            -- it failed
            Nothing -> return v'
            Just Mentioned -> return (v' & localeParagraphCount %~ (+1)
                                          & removeFromLocale (a, o))
            Just Unmentioned -> return (v' & localeParagraphCount %~ (+1)) -}) v sorted
      return (newP, Nothing))

alsoSee
  :: forall s r.
     HasLocaleProperties s
  => Rule s (LocaleVariables s) r
alsoSee = Rule "You can also see" (\v ->
  runState $
    do
      -- lp is everything that has a locale priority
      -- i.e. things which haven't been removed as they've been mentioned
      -- or otherwise been removed (by setting priority to 0 somehow)
      let lp = DEM.filter (\(x, _) -> x > 0 ) (unStore $ v ^. localePriorities)
          sayDomain :: ObjectLike s o => Text -> o -> State (World s) ()
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

describingLocaleImpl
  :: HasLocaleProperties s
  => Activity s (LocaleVariables s) ()
describingLocaleImpl = Activity "Describing the locale of something" Nothing
    (Rulebook "Before describing locale" Nothing (const . Just) [])
    (Rulebook "Carry out describing locale" Nothing (const . Just) [
        findNotable,
        interestingLocale,
        alsoSee])
    (Rulebook "After describing locale" Nothing (const . Just) [])

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


printingLocaleParagraphAboutImpl :: Activity w (AnyObject s, LocaleVariables s) (LocaleVariables s)
printingLocaleParagraphAboutImpl = undefined
{-Activity "Printing a locale paragraph about something" Nothing
  (Rulebook "Before printing a locale paragraph" Nothing (const . Just) [])
  (Rulebook "Carry out printing a locale paragraph" Nothing (const . Just)
      [ Rule
        "don’t mention player’s supporter in room descriptions rule"
        ( \v -> runState $ do
            playerID <- gets _currentPlayer
            get1st >>= (\e -> whenM (playerID `isEnclosedBy` e) (setLocalePriority e 0))
            return (v, Nothing)
        ),
      Rule
        "don’t mention scenery in room descriptions rule"
        ( do
            get1st >>= (\e -> whenM (e `isType` "scenery") (setLocalePriority e 0))
            return Nothing
        ),
      Rule
        "don’t mention undescribed items in room descriptions rule"
        ( do
            e <- get1st
            b <- isDescribed e
            unless b (setLocalePriority e 0)
            return Nothing
        ),
      Rule
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
        )
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
{-}
      Rule
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
        )-}
    ])
  (Rulebook "After printing a locale paragraph" Nothing (const . Just) [])

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