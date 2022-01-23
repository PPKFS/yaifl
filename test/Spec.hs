module Main ( main ) where

import Yaifl
import Test.Hspec
import qualified Data.Text as T
import Yaifl.Prelude
import Yaifl.Activities
import qualified Data.EnumMap as DEM
import Yaifl.ObjectLookup

ex2World :: Game () (World ())
ex2World = newWorld $ do
  setTitle "Bic"
  addRoom' "The Staff Break Room" "" pass
  addThing' "Bic pen" "" pass
  addThing' "orange" "It's a small hard pinch-skinned thing from the lunch room, probably with lots of pips and no juice." pass
  addThing' "napkin" "Slightly crumpled." pass
  addWhenPlayBegins $ makeRule' "run property checks at the start of play" $
    do
      foreachObject things (\t -> do
        when (isBlankDescription (_objDescription t)) (do
          printName t
          sayLn " has no description.")
        return Nothing)
      return Nothing

foreachObject :: 
  MonadWorld s m
  => StoreLens' s d
  -> (Object s d -> m (Maybe (Object s d)))
  -> m ()
foreachObject sl f = do
  store <- use sl
  DEM.traverseWithKey (\_ o -> do
    robj <- reifyObject sl o
    updObj <- f robj
    whenJust updObj (setObjectFrom sl)
    ) (unStore store)
  pass

ex2Test :: [Text]
ex2Test = 
  [ expectLooking "The Staff Break Room" ""
  , expectYouCanSee ["a Bic pen", "a orange", "a napkin"]
  , expectLine "Bic pen has no description."]

tests :: Spec
tests = describe "Chapter 3: " $ do
  it "runs chapter 3.1.2" $ 
    testHarness "Bic - 3.1.2" ex2World [] ex2Test

testHarness :: 
  HasStandardProperties o 
  => Text 
  -> Game o (World o)
  -> [Text] 
  -> [Text]
  -> Expectation
testHarness fullTitle initWorld actionsToDo expected = do
  let (t, shortName) = first (T.dropEnd 3) $ T.breakOnEnd " - " fullTitle
  w2 <- runGame shortName (do
    info $ bformat ("Building world " %! stext %! "...") shortName
    w' <- initWorld
    info $ bformat "World construction finished, beginning game..."
    --when I write a proper game loop, this is where it needs to go
    withoutMissingObjects 
      (runRulebook (_whenPlayBegins w') ())
      (handleMissingObject "Failed when beginning" (Just False))
    --do the commands...
    get) blankWorld
  let (x, _) = flushBufferToText (Proxy @'SayBuffer) w2
      buildExpected = mconcat (expectTitle t : expected )
  x `shouldBe` buildExpected

expectLine :: Text -> Text
expectLine t1 = t1 <> "\n"

expectTitle :: Text -> Text
expectTitle = introText

expectYouCanSee :: [Text] -> Text
expectYouCanSee t1 = expectLine ("You can see " <> listThings t1 <> " here.\n")

listThings :: [Text] -> Text
listThings t1 = mconcat $ zipWith (\x v -> x <> (if v < length t1 - 1 then ", " else "") <>
                (if v == length t1 - 2 then "and " else "")) t1 [0..]

expectLooking :: Text -> Text -> Text
expectLooking t d = expectLine t <> expectLine d

{-
consumeBlankRoomDescription :: Text -> Text -> Either Assertion Text
consumeBlankRoomDescription t1 = consumeLine (mconcat ["It's ", t1, "."])
-}

main :: IO ()
main = hspec tests
      --die "you FAIL miette? you fail her tests like weakly typed language? oh! oh! jail for mother! jail for mother for One Thousand Years!!!"
{-

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
-}

