module Main
    (
        main
    ) where
import           Yaifl.Prelude
import Yaifl
import Control.Effect.Lens
import Control.Carrier.State.Lazy
import Control.Carrier.Writer.Strict
import Control.Carrier.Lift
import           Test.HUnit hiding (State)
import qualified Data.Text.Prettyprint.Doc.Render.Terminal
                                               as PPTTY
import qualified Data.Text.Prettyprint.Doc     as PP
import qualified Data.IntMap.Strict            as IM
import qualified Data.Map.Strict            as Map

main :: IO ()
main = do
    putStrLn $ "Example " <> show 1
    let (a, b) = runApplication $ do
            addContext "blah" Nothing 
            logMsg Error "hi"
            sayLn "test"
            logMsg Debug "hi again"
    PPTTY.putDoc b
    PPTTY.putDoc "\n------------\n"
    PPTTY.putDoc $ coerce a
    PPTTY.putDoc "\n------------\n"

    --_ <- runTestTT tests
    pass
{-
tests :: Test
tests = makeTests []

makeTests :: _ -> _
makeTests lst = TestList $
    zipWith (\ i x -> TestLabel (mkName i)  $ TestCase (runWorldTest i x)) [1 ..] lst
        where mkName i = "example " <> show i
-}
type WorldOutput = (SayOutput, PP.Doc PPTTY.AnsiStyle)
newtype ConcreteGameStack m a = CGS { getCarrier :: 
                    WriterC (PP.Doc PPTTY.AnsiStyle)
                    (WriterC SayOutput
                    (StateC LoggingContext
                    (StateC (GameSettings (ConcreteGameStack m)) Identity
                   ))) a } deriving (Functor, Applicative, Monad)

newtype GS m a = GS { getGS :: StateC (GameSettings (GS m)) m a}
  deriving (Functor, Applicative, Monad)

runApplication :: forall m. m () -> WorldOutput
runApplication v = second fst x where x = run . evalState (GameSettings "Untitled" Nothing Map.empty) . evalState (LoggingContext [] mempty) . runWriter @SayOutput . runWriter @(PP.Doc PPTTY.AnsiStyle) $ v


runWorldTest :: (WithLogging sig m, Has (Writer SayOutput) sig m) => Int -> m () -> (m () -> _ ()) -> Assertion
runWorldTest i w h = do
        putStrLn $ "Example " <> show i
        let (a, b) = runApplication (h w) {-$ do
            addContext "blah" Nothing 
            logMsg Error "hi"
            sayLn "test"
            logMsg Debug "hi again"-}
        PPTTY.putDoc b
        PPTTY.putDoc "\n------------\n"
        PPTTY.putDoc $ coerce a
        PPTTY.putDoc "\n------------\n"
        pass

example1World :: HasGameSettings sig m => m ()
example1World = do
    title .= "Bic"
    {-
    addRoom' "The Staff Break Room"
    addThing' "Bic pen" ""
    addThing' "orange" "It's a small hard pinch-skinned thing from the lunch room, probably with lots of pips and no juice."
    sayLn "aaaa"
    addThing' "napkin" "Slightly crumpled."
    sayLn "moo"
    addWhenPlayBeginsRule' "run property checks at the start of play rule" (do
        mapObjects2 physicalComponent objectComponent (\v o -> do
            when (descriptionOf o == "") (do
                --printName' o
                sayLn " has no description.")
            return (v, o)
            )
        return Nothing)
    g <- get
    sayLn (show $ Map.keys $ g ^. rulebooks)
    -}


{-
where

import           Yaifl.Common

import           Yaifl.Components
import           Yaifl.WorldBuilder
import           Data.Text.Prettyprint.Doc.Render.Terminal\
import           Test.HUnit hiding (State)


import Polysemy.State
import Polysemy.Error
import Polysemy.Output
import Polysemy.IO
import Control.Lens
import Polysemy.Trace
import qualified Prettyprinter.Render.Terminal as PPTTY
import qualified Data.Text.Prettyprint.Doc     as PP


data TestWorld = TestWorld
    {
        _objects :: Store Object,
        _enclosing :: Store Enclosing,
        _roomData :: Store RoomData,
        _physical :: Store Physical
    , _ec      :: Int
    }

makeLenses ''TestWorld

instance HasStore TestWorld Object where
    store _ = castOptic objects

instance HasStore TestWorld Enclosing where
    store _ = castOptic enclosing

instance HasStore TestWorld RoomData where
    store _ = castOptic roomData

instance HasStore TestWorld Physical where
    store _ = physical

instance EntityProducer TestWorld where
    entityCounter = ec

--type TestingMonadStack a = Sem TypeList a

type WorldOutput = Either Text ([PP.Doc PPTTY.AnsiStyle], ())

runWorldTest :: Int -> Sem r () -> (Sem r () -> IO WorldOutput) -> Assertion
runWorldTest i w h' = do
        putStrLn $ "Example " <> show i
        Right (v, _) <- h' w
        PPTTY.putDoc "\n------------\n"
        PPTTY.putDoc $ PP.fillCat v
        pass

h :: Sem '[World TestWorld, Log, State LoggingContext, Say,
                             State (GameSettings TestWorld), Error Text, State WorldBuildInfo,
                             Embed IO] () -> IO WorldOutput
h w = w 
            & worldToMapStore
            & evalState (TestWorld IM.empty IM.empty IM.empty IM.empty 0)
            & logToOutput
            & prettyprintOutputToIO
            & evalState (LoggingContext [] mempty)
            & sayToOutput
            & runOutputList
            & evalState (GameSettings "Untitled" Nothing Map.empty)
            & runError
            & evalState (WorldBuildInfo (-5))
            & runM
            
            

main :: IO ()
main = do
    _ <- runTestTT tests
    pass

tests :: Test
tests = makeTests [example1World]

makeTests lst = TestList $
    zipWith (\ i x -> TestLabel (mkName i)  $ TestCase (runWorldTest i x h)) [1 ..] lst
        where mkName i = "example " <> show i

testExampleBlank :: (Text -> Either Assertion Text) -> IO ()
testExampleBlank w1 ts = testExample w1 [] ts

testExample :: WorldBuilder World -> [Text] -> (Text -> Either Assertion Text) -> IO ()
testExample worldbuilder actions ts = do
    let w = buildWorld worldbuilder blankWorld
        w2 = w ^? gameInfo . rulebooks . ix whenPlayBeginsName
    let w4 = (\(CompiledRulebook j) -> runState j w) <$> w2
        w4' = snd $ fromMaybe (Nothing, w) w4
        --v = runActions actions $ snd w4'
        x = Relude.foldl' (\v p -> v <> show p) ("" :: Text) $ reverse $ w4' ^. messageBuffer . stdBuffer
    _ <- runStateT printMessageBuffer w4'
    putStrLn "-------------\n"
    case (Right x >>= ts) of 
        Left res -> res
        Right "" -> pass
        Right x' -> assertFailure $ "Was left with " <> toString x'




ex1 :: Assertion
ex1 = makeTestWorld

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = makeTests [ex1]

makeTests :: [Assertion] -> TestTree
makeTests lst = testGroup "Tests" $ map
    (\(i, x) -> after AllFinish (mkName (i - 1)) $ testCase (mkName i) x)
    (zip [1 ..] lst)
    where mkName i = "example " <> show i

        )


makeWorld "World" defaultWorld
makeLenses ''World

instance HasWorld World World where
    world = id

instance HasGameInfo World World where
    gameInfo = worldGameInfo

instance Has World Object where
    store _ = objectStore

instance Has World RoomData where
    store _ = roomDataStore

instance Has World Physical where
    store _ = physicalStore

instance Has World Player where
    store _ = playerStore

instance HasMessageBuffer World where
    messageBuffer = gameInfo . msgBuffer

instance Has World Enclosing where
    store _ = enclosingStore

instance Has World Container where
    store _ = containerStore

instance Has World Openable where
    store _ = openableStore

instance Has World Supporter where
    store _ = supporterStore

instance Has World Direction where
    store _ = directionStore
    


example2World :: WorldBuilder World
example2World = do
    setTitle "Verbosity"
    _1 . gameInfo . roomDescriptions .= SometimesAbbreviatedRoomDescriptions
    w <- addRoom "The Wilkie Memorial Research Wing" [r|"The research wing was built onto the science building in 1967, when the college's finances were good but its aesthetic standards at a local minimum. A dull brown corridor recedes both north and south; drab olive doors open onto the laboratories of individual faculty members. The twitchy fluorescent lighting makes the whole thing flicker, as though it might wink out of existence at any moment.

The Men's Restroom is immediately west of this point."|]
    addRoom "The Men's Restroom" [r|The Men's Restroom is west of the Research Wing. "Well, yes, you really shouldn't be in here. But the nearest women's room is on the other side of the building, and at this hour you have the labs mostly to yourself. All the same, you try not to read any of the things scrawled over the urinals which might have been intended in confidence."|]
    isWestOf w
    pass
    
ex1 :: Assertion
ex1 = testExampleBlank example1World (\z -> consumeTitle "Bic" z >>=
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
consumeText t1 t2 = case Text.stripPrefix t1 t2 of
    Just x -> Right x
    Nothing -> Left $ t1 @?= t2

assertText :: (Text, [Text]) -> World -> Assertion
assertText (ti, xs) w = do
    let x = reverse $ w ^. messageBuffer . stdBuffer
    --_ <- runStateT printMessageBuffer w
    Relude.foldl' (\v p -> v <> show p) ("" :: Text) x @?= buildExpected ti xs
    pass
    where buildExpected t x = mconcat $ introText t <> [unlines x]
-}
