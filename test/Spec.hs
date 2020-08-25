module Main (main) where

import Yaifl
import Relude
import Control.Lens
import qualified Data.Text as Text
import Data.Text.Prettyprint.Doc.Render.Terminal
import Test.Tasty
import Test.Tasty.HUnit
import Data.Map as DM


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
            w <- get
            when (getDescription w o == "") (do
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
    assertText ("Bic", ["Bic pen has no description."]) (snd v)
    pass

assertText :: (Text, [Text]) -> World -> Assertion
assertText (ti, xs) w = do
    let x = reverse $ w ^. messageBuffer . stdBuffer
    _ <- runStateT printMessageBuffer w
    Relude.foldl' (\v p -> v <> show p) ("" :: Text) x @?= buildExpected ti xs
    pass
                where buildExpected t x = mconcat $ introText t <> [unlines x]

data Holder = Holder
    {
        aStore :: DM.Map Int Bool,
        bStore :: DM.Map Int Text,
        cStore :: DM.Map Int Int,
        dStore :: DM.Map Int (Bool, Bool),
        currentID :: Int
    }


newtype GuaranteedKey (a :: [k]) = GuaranteedKey Int -- ???

data BoxedKey where
    Box :: GuaranteedKey a -> BoxedKey

--unboxKey :: BoxedKey -> Proxy a -> Maybe (GuaranteedKey a)
--unboxKey (Box (a :: GuaranteedKey a)) _ = Just a

type family Contains' (list :: [Type]) (single :: Type) where
  Contains' (x ': xs) (x) = 'True
  Contains' '[]       (x) = 'False
  Contains' (x ': xs) (y) = Contains' xs (y)

makeAC :: Holder -> Bool -> Int -> (GuaranteedKey ('[Bool, Int]), Holder)
makeAC h b i = runState (do
    h1 <- get
    let cid = currentID h1
    put $ h1 {currentID = cid+1, aStore = insert cid b (aStore h), cStore = insert cid i (cStore h)}
    return $ GuaranteedKey cid) h

makeAB :: Holder -> Bool -> Text -> (GuaranteedKey ('[Bool, Text]), Holder)
makeAB h b i = runState (do
    h1 <- get
    let cid = currentID h1
    put $ h1 {currentID = cid+1, aStore = insert cid b (aStore h), bStore = insert cid i (bStore h)}
    return $ GuaranteedKey cid) h

knownType :: (Contains' k Text ~ 'True) => GuaranteedKey k -> Holder -> Text
knownType (GuaranteedKey a) h = case DM.lookup a (bStore h) of
    Just x -> x
    Nothing -> error "congrats, you broke it all"
testFunc :: IO ()
testFunc = do
    let h = Holder DM.empty DM.empty DM.empty DM.empty 0
    let (k1, h') = makeAC h False 5
    --print $ knownType k1 --doesn't compile!
    let (k2, h'') = makeAB h' True "test"
    let someList = [Box k1, Box k2]
    --print $ knownType (head someList) h''
    pass
    --print $ knownType k2 h
    --print $ knownType k1 