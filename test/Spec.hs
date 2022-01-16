module Main
    (
        main
    ) where
import Relude

import Yaifl
import Test.HUnit
import qualified Data.Text as T

ex2World :: IO (World ())
ex2World = newWorld $ do
   setTitle "Bic"
   addRoom' "The Staff Break Room" "" pass
   addThing' "Bic pen" "" pass
   addThing' "orange" "It's a small hard pinch-skinned thing from the lunch room, probably with lots of pips and no juice." pass
   addThing' "napkin" "Slightly crumpled." pass
   addWhenPlayBegins $ makeRule' "run property checks at the start of play rule" rulePass
    {-
    addRule whenPlayBeginsRules $ Rule "run property checks at the start of play rule" (do
        foreachObject things (do
            t <- getForeachObject
            whenM (("" ==) <$> evalDescription t) (do
                printName t
                sayLn " has no description."))
        return Nothing)
    -}


tests :: Test
tests = makeTests [YaiflTestCase "Bic - 3.1.2" ex2World [] ex2Test]

testMeWith :: [Text] -> Text -> [Text -> Either Assertion Text] -> Text -> Either Assertion Text
testMeWith _ t c = consumeTitle t >=> foldr (>=>) return c

consumeYouCanSee :: [Text] -> Text -> Either Assertion Text
consumeYouCanSee t1 = consumeLine ("You can see " <> listThings t1 <> " here.\n")

listThings :: [Text] -> Text
listThings t1 = mconcat $ zipWith (\x v -> x <> (if v < length t1 - 1 then ", " else "") <>
                (if v == length t1 - 2 then "and " else "")) t1 [0..]

consumeBlankRoomDescription :: Text -> Text -> Either Assertion Text
consumeBlankRoomDescription t1 = consumeLine (mconcat ["It's ", t1, "."])

consumeLine :: Text -> Text -> Either Assertion Text
consumeLine t1 = consumeText (mconcat [t1, "\n"])
consumeTitle :: Text -> Text -> Either Assertion Text
consumeTitle t = consumeText (introText t)
consumeText :: Text -> Text -> Either Assertion Text
consumeText t1 t2 = case T.stripPrefix t1 t2 of
    Just x -> Right x
    Nothing -> Left $ t2 @?= t1

consumeLooking :: Text -> Text -> Text -> Either Assertion Text
consumeLooking t d = consumeLine t >=> consumeLine d

data YaiflTestCase where
    YaiflTestCase :: { testCaseName :: String
    , testCaseWorld :: IO (World o)
    , testCommands :: [Text]
    , testCaseExpected :: Text -> Either Assertion Text
    } -> YaiflTestCase

ex2Test :: Text -> Either Assertion Text
ex2Test = testMeWith [] "Bic" [
            consumeLooking "The Staff Break Room" "",
            consumeYouCanSee ["a Bic pen", "a orange", "a napkin"],
            consumeLine "Bic pen has no description."]

makeTests :: [YaiflTestCase] -> Test
makeTests lst = TestList $
    map (\YaiflTestCase{..} -> TestLabel testCaseName $
        TestCase (testHarness testCaseWorld testCommands testCaseExpected)) lst

{-
w2 <- runWorld (do
        logInfo "Started world building..."
        addBaseActions
        addBaseActivities
        w
        addExtrasToBeTH
        logInfo "Finished world building. Now validating..."
        logInfo "No validation implemented."
        logInfo "Finished validation, now running game..."
        logInfo "\n-------------------"
        --when I write a proper game loop, this is where it needs to go

        wpbr <- use $ rulebookStore . at whenPlayBeginsName
        maybe (liftIO $ assertFailure "Couldn't find the when play begins rulebook..") (\(BoxedRulebook r) -> runRulebook r) wpbr
        ) (blankGameData blankGameWorld id) (Env (LogAction (liftIO . putTextLn . fmtMessage )))
-}

testHarness :: IO (World o) -> [Text] -> (Text -> Either Assertion Text) -> Assertion
testHarness ioW _ consume = do
    w <- ioW
    w2 <- runGame (do
                --modify $ setSayStyle $ (Just PPTTY.bold)
                --logInfo "Validating...no validation implemented."
                --logInfo "\n---------------"
                w' <- get
                --when I write a proper game loop, this is where it needs to go
                runRulebook (_whenPlayBegins w') ()
                --do the commands...
                get
                ) w
    w3 <- flushBufferToStdOut (Proxy @'LogBuffer) w2
    let (x, _) = flushBufferToText (Proxy @'SayBuffer) w3
    (case Right x >>= consume of
        Left res -> res
        Right "" -> pass
        Right x' -> assertFailure $ "Was left with " <> toString x')

main :: IO ()
main = do
    v <- runTestTT tests
    if errors v + failures v == 0 then
      exitSuccess
    else
      die "you FAIL miette? you fail her tests like weakly typed language? oh! oh! jail for mother! jail for mother for One Thousand Years!!!"
{-


addRuleLast :: Rulebook w v a -> Rule w v a -> Rulebook w v a
addRuleLast (Rulebook n d rs) r = Rulebook n d (rs <> [r])
addRuleLast (RulebookWithVariables n d s rs) r = RulebookWithVariables n d s (rs <> [r])

ex2World :: forall w. HasStandardWorld w => World w ()
ex2World = do
    setTitle "Bic"
    makeRoom "The Staff Break Room" "" pass
    thereIs' @(Thing w) "Bic pen" ""
    thereIs' @(Thing w) "orange" "It's a small hard pinch-skinned thing from the lunch room, probably with lots of pips and no juice."
    thereIs' @(Thing w) "napkin" "Slightly crumpled."
    addRule whenPlayBeginsRules $ Rule "run property checks at the start of play rule" (do
        foreachObject things (do
            t <- getForeachObject
            whenM (("" ==) <$> evalDescription t) (do
                printName t
                sayLn " has no description."))
        return Nothing)
    pass

ex2Test :: Text -> Either Assertion Text
ex2Test = testMeWith [] "Bic" [
            consumeLooking "The Staff Break Room" "",
            consumeYouCanSee ["a Bic pen", "a orange", "a napkin"],
            consumeLine "Bic pen has no description."]

consumeLooking :: Text -> Text -> Text -> Either Assertion Text
consumeLooking t d = consumeLine t >=> consumeLine d

testMeWith :: [Text] -> Text -> [Text -> Either Assertion Text] -> Text -> Either Assertion Text
testMeWith _ t c = consumeTitle t >=> foldr (>=>) return c

ex3World :: HasStandardWorld w => World w ()
ex3World = do
    setTitle "Verbosity"
    -- inform7 uses superbrief, brief, and verbose as the command words
    -- even though the BtS names are abbreviated, sometimes abbreviated, and not abbreviated
    roomDescriptions .= SometimesAbbreviatedRoomDescriptions
    w <- makeRoom "The Wilkie Memorial Research Wing" [r|The research wing was built onto the science building in 1967, when the college's finances were good but its aesthetic standards at a local minimum. A dull brown corridor recedes both north and south; drab olive doors open onto the laboratories of individual faculty members. The twitchy fluorescent lighting makes the whole thing flicker, as though it might wink out of existence at any moment.

The Men's Restroom is immediately west of this point.|] pass

    makeRoom "The Men's Restroom" [r|The Men's Restroom is west of the Research Wing. "Well, yes, you really shouldn't be in here. But the nearest women's room is on the other side of the building, and at this hour you have the labs mostly to yourself. All the same, you try not to read any of the things scrawled over the urinals which might have been intended in confidence.|] (isWestOf w)
    pass

    --testMe ["west", "east", "verbose", "west"]

isWestOf :: RoomObject w -> State (RoomObject w) a0
isWestOf = error "not implemented"

ex4World :: HasStandardWorld w => World w ()
ex4World = do
    setTitle "Slightly Wrong"

    a <- makeRoom "Awning" [r|A tan awning is stretched on tent poles over the dig-site, providing a little shade to the workers here; you are at the bottom of a square twenty feet on a side, marked out with pegs and lines of string. Uncovered in the south face of this square is an awkward opening into the earth.|] pass

    {- makeRoom "Slightly Wrong Chamber" (dynamicDescription (\e -> do
        whenM (isVisited e) (append [r|When you first step into the room, you are bothered by the sense that something is not quite right: perhaps the lighting, perhaps the angle of the walls.|])
        append [r|A mural on the far wall depicts a woman with a staff, tipped with a pine-cone. She appears to be watching you.|])) (isSouthOf a) -}
    pass
    --testMe ["look", "s", "look"]


isVisited :: t0 -> m0 Bool
isVisited = error "not implemented"

append :: t1 -> m0 ()
append = error "not implemented"

isSouthOf :: RoomObject w -> State (RoomObject w) a0
isSouthOf = error "not implemented"

-- Port Royal consists of examples 5,
portRoyalWorld :: HasStandardWorld w => World w ()
portRoyalWorld = do
    setTitle "1691"
    fj <- makeRoom "Fort James" [r|The enclosure of Fort James is a large, roughly hexagonal court walled with heavy stone. The walls face the entrance to Port Royal Harbour, and the battery of guns is prepared to destroy any enemy ship arriving.|] pass

    tse <- makeRoom "Thames Street End" [r|he ill-named Thames Street runs from here -- at the point of the peninsula -- all the way east among houses and shops, through the Fish Market, edging by the round front of Fort Carlisle, to the point where the town stops and there is only sandy spit beyond. Lime Street, wider and healthier but not as rich, runs directly south, and to the north the road opens up into the courtyard of Fort James.|] (isSouthOf fj)

    wl <- makeRoom "Water Lane" [r|Here Thames Street -- never very straight -- goes steeply southeast for a portion before continuing more directly to the east.

    Water Lane runs south toward Queen Street, and facing onto it is the New Prison -- which, in the way of these things, is neither. It did serve in that capacity for a time, and in a measure of the villainy which has been usual in Port Royal from its earliest days, it is nearly the largest building in the town.|] (isEastOf tse)

    tsawb <- makeRoom "Thames Street at the Wherry Bridge" [r|To the southwest is the fishmarket; directly across the street is the entrance to a private alley through a brick archway.|] (isEastOf wl)

    pa <- makeRoom "The Private Alley" [r|You're just outside the tavern the Feathers. To the north, under a pretty little archway, is the active mayhem of Thames Street, but the alley narrows down to a dead end a little distance to the south.|] (isSouthOf tsawb)

    tf <- makeRoom "The Feathers" [r|Newly built with brick, replacing the older Feathers tavern that used to stand here. It sells wines in quantity, as well as serving them directly, and the goods are always of the best quality. There's a room upstairs for those wanting to stay the night.|] (isInsideFrom pa)

    makeRoom "The Feathers Bedroom" "" (isAbove tf)

    makeRoom "Lime Street" "" (isSouthOf tse)

    qsm <- makeRoom "Queen Street Middle" "" pass

    makeRoom "Queen Street East" "" (do
        isSouthOf pa
        isEastOf qsm)

    pass

isEastOf :: RoomObject w -> State (RoomObject w) a1
isEastOf = error "not implemented"

isInsideFrom :: t0 -> State (RoomObject w) a2
isInsideFrom = error "not implemented"

isAbove :: RoomObject w -> State (RoomObject w) a3
isAbove = error "not implemented"



setTitle :: WithGameData w m => Text -> m ()
setTitle t = title .= t

consumeYouCanSee :: [Text] -> Text -> Either Assertion Text
consumeYouCanSee t1 = consumeLine ("You can see " <> listThings t1 <> " here.\n")

listThings :: [Text] -> Text
listThings t1 = mconcat $ zipWith (\x v -> x <> (if v < length t1 - 1 then ", " else "") <>
                (if v == length t1 - 2 then "and " else "")) t1 [0..]

consumeBlankRoomDescription :: Text -> Text -> Either Assertion Text
consumeBlankRoomDescription t1 = consumeLine (mconcat ["It's ", t1, "."])

consumeLine :: Text -> Text -> Either Assertion Text
consumeLine t1 = consumeText (mconcat [t1, "\n"])
consumeTitle :: Text -> Text -> Either Assertion Text
consumeTitle t = consumeText (mconcat $ introText t)
consumeText :: Text -> Text -> Either Assertion Text
consumeText t1 t2 = case T.stripPrefix t1 t2 of
    Just x -> Right x
    Nothing -> Left $ t2 @?= t1

main :: IO ()
main = do
    v <- runTestTT tests
    if errors v + failures v == 0 then
      exitSuccess
    else
      die "uh oh, you made a serious fwcky wucky, now you have to get in the forever box"

tests :: Test
tests = makeTests [("Bic - 3.1.2", ex2World, ex2Test)]

makeTests :: [(String, World GameWorld a, Text -> Either Assertion Text)] -> Test
makeTests lst = TestList $
    map (\(z, x, y) -> TestLabel z  $ TestCase (testExample x [] y)) lst

--testExampleBlank :: (Text -> Either Assertion Text) -> IO ()
--testExampleBlank w1 ts = testExample w1 [] ts

addExtrasToBeTH :: HasStandardWorld w => World w ()
addExtrasToBeTH = do
    updateFirstRoom
    makePlayer defaultPlayerID
    pass
testExample :: World GameWorld a -> [Text] -> (Text -> Either Assertion Text) -> IO ()
testExample w _ ts = do
    w2 <- runWorld (do
        logInfo "Started world building..."
        addBaseActions
        addBaseActivities
        w
        addExtrasToBeTH
        logInfo "Finished world building. Now validating..."
        logInfo "No validation implemented."
        logInfo "Finished validation, now running game..."
        logInfo "\n-------------------"
        --when I write a proper game loop, this is where it needs to go

        wpbr <- use $ rulebookStore . at whenPlayBeginsName
        maybe (liftIO $ assertFailure "Couldn't find the when play begins rulebook..") (\(BoxedRulebook r) -> runRulebook r) wpbr
        ) (blankGameData blankGameWorld id) (Env (LogAction (liftIO . putTextLn . fmtMessage )))
    let x = foldl' (\v p -> v <> show p) ("" :: Text) $ reverse $ w2 ^. messageBuffer . buffer
    --putStrLn "-------------\n"
    case Right x >>= ts of
        Left res -> res
        Right "" -> pass
        Right x' -> assertFailure $ "Was left with " <> toString x'


{-

ex2 :: Assertion
ex2 = testExampleBlank example1World (\z -> consumeTitle "Bic" z >>=
            consumeLine "The Staff Break Room" >>=
            consumeBlankRoomDescription "The Staff Break Room" >>=
            consumeYouCanSee ["a Bic pen", "a orange", "a napkin"] >>=
            consumeLine "Bic pen has no description.")

ex2 :: Assertion
ex2 = testExample example2World [] (\v -> consumeTitle "Verbosity" v >>=
            consumeLine "The Wilkie Memorial Research Wing" >>=
            consumeLine [r|"The research wing was built onto the science building in 1967, when the college's finances were good but its aesthetic standards at a local minimum. A dull brown corridor recedes both north and south; drab olive doors open onto the laboratories of individual faculty members. The twitchy fluorescent lighting makes the whole thing flicker, as though it might wink out of existence at any moment.

The Men's Restroom is immediately west of this point."|])


--parse the input
--run the input
--apply this over every test?
runActions :: [Text] -> World -> World
runActions = error "not implemented"


        -- I have no idea what this is
    {-mapM_ (\(a, v) -> do
        printName a (SayOptions Indefinite Uncapitalised)
        when (v < length stuff - 1) (say ", ")
        when (v == length stuff - 2) (say "and ")
        component' physicalComponent a . mentioned .= True
        ) $ zip (toList stuff) [0..]-}

-}
-}

