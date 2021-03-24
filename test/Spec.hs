module Main
    (
        main
    ) where
import           Yaifl.Prelude
import Yaifl.Components
import           Test.HUnit hiding (State)
import Colog.Message
import Colog
import qualified Data.Text as T
import Yaifl
import Text.RawString.QQ

makeWorld "GameWorld" [''Object, ''RoomData, ''Physical, ''Enclosing, ''Player, ''ContainerData, ''Openable, ''Supporter, ''Enterable]
makeLenses ''GameWorld

instance HasStore GameWorld (Object GameWorld) where
    store = objectStore

instance HasStore GameWorld (Physical GameWorld) where
    store = physicalStore

instance HasStore GameWorld RoomData where
    store = roomDataStore

instance HasStore GameWorld Enclosing where
    store = enclosingStore

instance HasStore GameWorld Player where
    store = playerStore

instance HasStore GameWorld Openable where
    store = openableStore

instance HasStore GameWorld ContainerData where
    store = containerDataStore

instance HasStore GameWorld Enterable where
    store = enterableStore

instance HasStore GameWorld Supporter where
    store = supporterStore

runWorld :: World w a -> GameData w -> Env (World w) -> IO (GameData w)
runWorld w i env = execStateT (runReaderT (unwrapWorld w) env) i

addRule :: Rulebook w () RuleOutcome -> Rule w () RuleOutcome -> World w ()
addRule rb r = do
    let r2 = addRuleLast rb r
    rulebookStore . at (rulebookName r2) ?= BoxedRulebook r2
    pass

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
ex2Test z = consumeTitle "Bic" z >>=
            consumeLine "The Staff Break Room" >>=
            consumeBlankRoomDescription "The Staff Break Room" >>=
            consumeYouCanSee ["a Bic pen", "a orange", "a napkin"] >>=
            consumeLine "Bic pen has no description."

ex3World :: HasStandardWorld w => World w ()
ex3World = do
    setTitle "Verbosity"
    -- inform7 uses superbrief, brief, and verbose as the command words
    -- even though the BtS names are abbreviated, sometimes abbreviated, and not abbreviated
    roomDescriptions .= SometimesAbbreviatedRoomDescriptions 
    makeRoom "The Wilkie Memorial Research Wing" [r|"The research wing was built onto the science building in 1967, when the college's finances were good but its aesthetic standards at a local minimum. A dull brown corridor recedes both north and south; drab olive doors open onto the laboratories of individual faculty members. The twitchy fluorescent lighting makes the whole thing flicker, as though it might wink out of existence at any moment.

The Men's Restroom is immediately west of this point."|] pass

    makeRoom "The Men's Restroom" [r|The Men's Restroom is west of the Research Wing. "Well, yes, you really shouldn't be in here. But the nearest women's room is on the other side of the building, and at this hour you have the labs mostly to yourself. All the same, you try not to read any of the things scrawled over the urinals which might have been intended in confidence."|] pass
    pass
    --isWestOf w


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