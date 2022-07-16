{-# LANGUAGE Strict #-}

module Main ( main ) where
{-
import Yaifl
import Test.Hspec
import qualified Data.Text as T
import Yaifl.Core.Prelude
import Yaifl.Core.Activities
import qualified Data.EnumMap as DEM
import Yaifl.Core.ObjectLookup
-}
{-
import Yaifl
import Test.Hspec
import qualified Data.Text as T
import Yaifl.Core.Prelude
import Yaifl.Core.Activities
import qualified Data.EnumMap as DEM
import Yaifl.Core.ObjectLookup
-}
import qualified Yaifl.Test.Chapter3.Common as Chapter3
import Test.Sandwich
    ( runSandwich,
      defaultOptions,
      describe,
      Options(optionsFormatters, optionsProjectRoot,
              optionsTestArtifactsDirectory),
      SomeFormatter(SomeFormatter),
      TestArtifactsDirectory(TestArtifactsGeneratedDirectory) )
import Yaifl.Test.Common (DiffException (..))
import Test.Sandwich.Formatters.TerminalUI
    ( defaultTerminalUIFormatter,
      CustomTUIException(CustomTUIExceptionBrick),
      terminalUICustomExceptionFormatters )
import Graphics.Vty
    ( Attr(attrForeColor), MaybeDefault(SetTo), green, red )
import Brick ( modifyDefAttr, txtWrap, vBox, Widget )
import Data.Algorithm.Diff
    ( getGroupedDiff, Diff, PolyDiff(First, Both, Second) )
import Data.Time ( getCurrentTime )


formatDiff :: SomeException -> Maybe CustomTUIException
formatDiff e = case fromException e of
  Just (DiffException amendedOutput ex) -> Just $ CustomTUIExceptionBrick
    $ vBox $ map diffBlockToWidget $ getGroupedDiff (lines amendedOutput) (lines ex)
  Nothing -> Nothing

diffBlockToWidget :: Diff [Text] -> Widget n
diffBlockToWidget (Both a _) = txtWrap $ unlines a
diffBlockToWidget (Data.Algorithm.Diff.Second a) = modifyDefAttr (\x -> x { attrForeColor = SetTo red }) $ txtWrap $ "-" <> unlines a
diffBlockToWidget (Data.Algorithm.Diff.First a) = modifyDefAttr (\x -> x { attrForeColor = SetTo green }) $ txtWrap $ "+" <> unlines a

testOptions :: Options
testOptions = defaultOptions {
  optionsFormatters = [SomeFormatter $ defaultTerminalUIFormatter {
      terminalUICustomExceptionFormatters = [formatDiff]
      }]
  , optionsProjectRoot = Just "testfol"
  , optionsTestArtifactsDirectory = TestArtifactsGeneratedDirectory "test_runs" (show <$> getCurrentTime)
}

main :: IO ()
main = runSandwich testOptions $ do
  describe "Examples" $ do
    Chapter3.spec
{-
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

