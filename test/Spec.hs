module Main
    (
        main
    ) where
import           Yaifl.Prelude
import Yaifl
import Yaifl.Components
import           Test.HUnit hiding (State)
import qualified Data.Text.Prettyprint.Doc.Render.Terminal
                                               as PPTTY
import qualified Data.Text.Prettyprint.Doc     as PP
import qualified Data.IntMap.Strict            as IM
import qualified Data.Map.Strict            as Map
import qualified Data.Set as DS
import qualified Control.Monad.State.Lazy as MonState


import Yaifl.TH
import Colog.Monad
import Colog (LogAction(..),Severity(..), logStringStdout, logPrint, HasLog)
import Colog.Message
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.IO as TIO
import Yaifl.Activities
import Yaifl.Rulebooks
import Yaifl.Actions
{-
Game { unwrapGame :: ReaderT (RulebookStore (Game w))
(LoggerT Text (World w)) a } deriving (Functor, Applicative, Monad)
                                            
newtype World w a = World 
{ unwrapWorld :: State (GameData w) a } deriving (Functor, Applicative, Monad)
-}

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
runWorld w i env = execStateT (runReaderT (unwrapWorld w) env) i

addRule :: Rulebook w () RuleOutcome -> Rule w () RuleOutcome -> World w ()
addRule rb r = do
    logDebug "added a rule.."
    let r2 = addRuleLast rb r
    rulebookStore . at (rulebookName r2) ?= BoxedRulebook r2
    pass

addRuleLast :: Rulebook w v a -> Rule w v a -> Rulebook w v a
addRuleLast (Rulebook n d rs) r = Rulebook n d (rs <> [r])
addRuleLast (RulebookWithVariables n d s rs) r = RulebookWithVariables n d s ([r] <> rs)

modifyingM :: MonadState s m => LensLike m s s a b -> (a -> m b) -> m ()
modifyingM t f = do
  s <- get
  s' <- t f s
  put s'

ex1World :: forall w. HasStandardWorld w => World w ()
ex1World = do
    setTitle "Bic"
    thereIs @(RoomObject w) $ do
        name .= "The Staff Break Room"
    thereIs @(Thing w) $ do
        name .= "Bic pen"
    thereIs @(Thing w) $ do
        name .= "orange"
        description .= "It's a small hard pinch-skinned thing from the lunch room, probably with lots of pips and no juice."
    thereIs @(Thing w) $ do
        name .= "napkin"
        description .= "Slightly crumpled."
    thereIs @(RoomObject w) $ do
        name .= "The Staff Break Room2"
    addRule whenPlayBeginsRules $ Rule "run property checks at the start of play rule" (do
        modifyingM (gameWorld . things . traverse) (\t -> do
            whenM (do
                desc <- evalDescription t
                return $ "" == desc) (do
                sayLn $ (t ^. name) <> " has no description.")
            return t)
        return Nothing)
    pass

ex1Test :: Text -> Either Assertion Text
ex1Test z = consumeTitle "Bic" z >>=
            consumeLine "The Staff Break Room" >>=
            consumeBlankRoomDescription "The Staff Break Room" >>=
            consumeYouCanSee ["a Bic pen", "a orange", "a napkin"] >>=
            consumeLine "Bic pen has no description."

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
tests = makeTests [("Bic - 3.1.2", ex1World, ex1Test)]

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
tests :: Test
tests = makeTests []

makeTests :: _ -> _
makeTests lst = TestList $
    zipWith (\ i x -> TestLabel (mkName i)  $ TestCase (runWorldTest i x)) [1 ..] lst
        where mkName i = "example " <> show i

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

-}