module Main (main) where

import Yaifl
import Relude
import Control.Lens
import qualified Data.Text as Text
import Data.Text.Prettyprint.Doc.Render.Terminal
import Test.Tasty
import Test.Tasty.HUnit


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
    
example1World :: WorldBuilder World
example1World = do
    setTitle "Bic"
    addRoom "The Staff Break Room"
    makeThingWithoutDescription "Bic pen"
    makeThing "orange" 
        "It's a small hard pinch-skinned thing from the lunch room, probably with lots of pips and no juice."
    makeThing "napkin" "Slightly crumpled."
    addWhenPlayBeginsRule "run property checks at the start of play rule" (do
        mapObjects objectComponent (\o -> do
            when (o ^. description == "") (do
                printName o
                sayLn " has no description." ) 
            return o
            )
        return Nothing
        )

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Chapter 1" [ex1]

ex1 :: TestTree
ex1 = testCase "example 1" $ do
    let w = buildWorld example1World blankWorld
    let w2 = w ^? gameInfo . rulebooks . ix whenPlayBeginsName
    print w2
    let w3 = (\(CompiledRulebook j) -> runState j w) <$> w2
    let v = fromMaybe (Nothing, w) w3
    assertText ("Bic", ["Bic pen has no description."]) =<< runStateT printMessageBuffer (snd v)
    pass

assertText :: (Text, [Text]) -> (Text, a) -> Assertion
assertText (ti, xs) (x, _) = buildExpected ti xs @?= x
                where buildExpected t x = mconcat $ introText t <> [unlines x]
