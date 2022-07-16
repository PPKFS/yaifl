module Yaifl.Test.Common where



import Yaifl
import qualified Data.Text as T
import Language.Haskell.TH.Quote hiding (quoteExp)
import Language.Haskell.TH
import Data.Char (isSpace)
import Yaifl.Core.World
import Yaifl.Core.Say
import Yaifl.Core.Logger
import Cleff.State (runState, get)
import Yaifl.Core.Objects.Query
import Yaifl.Core.Actions.Action
import Yaifl.Core.Rulebooks.Run
import Yaifl.Core.Common
import Yaifl.Core.Rulebooks.WhenPlayBegins (introText)
import Text.Interpolation.Nyan
import Test.Sandwich ( timeAction, it, HasBaseContext, SpecFree )
import Control.Monad.Catch ( MonadMask )
import Control.Exception (throwIO)
import Prelude hiding (force)

expQQ :: (String -> Q Exp) -> QuasiQuoter
expQQ quoteExp = QuasiQuoter quoteExp notSupported notSupported notSupported where
  notSupported _ = fail "Quotation in this context is not supported"

wrappedText :: QuasiQuoter
wrappedText = expQQ (return . LitE . StringL . toString . T.strip . newlinesToWrap . unindent . tabsToSpaces . toText)

lineIndent :: Text -> Int
lineIndent = T.length . T.takeWhile (== ' ')

minimumIndent :: Text -> Maybe Int
minimumIndent =
  listToMaybe . sort . map lineIndent
    . filter (\t -> T.empty /= T.dropWhile isSpace t) . lines

unindent :: Text -> Text
unindent s =
  case lines s of
    h : t ->
      let
        unindentedHead = T.dropWhile (== ' ') h
        minimumTailIndent = minimumIndent . unlines $ t
        unindentedTail = case minimumTailIndent of
          Just indent -> map (T.drop indent) t
          Nothing -> t
      in unlines $ unindentedHead : unindentedTail
    [] -> ""

tabsToSpaces :: Text -> Text
tabsToSpaces = T.replace "\t" " "

newlinesToWrap :: Text -> Text
newlinesToWrap = foldl' (\acc -> \case
  "" -> acc <> "\n" <> (if fmap snd (unsnoc acc) == Just '\n' then "" else "\n")
  x -> acc <> x) "" . lines

data DiffException = DiffException Text Text
  deriving stock Show
instance Exception DiffException

force :: NFData a => a -> a
force a = deepseq a a
testHarness ::
  forall wm a context m.
  MonadIO m
  => MonadMask m
  => HasBaseContext context
  => HasStandardProperties wm
  => HasCallStack
  => Text
  -> Game wm a
  -> [Text]
  -> [Text]
  -> SpecFree context m ()
testHarness fullTitle initWorld actionsToDo expected = do
  it (toString $ "Runs " <> fullTitle) $ do
    (!w :: World wm) <- timeAction "Worldbuilding" $ liftIO $ runGame blankWorld fullTitle (do
        newWorld
        info [int|t|Building world #{fullTitle}...|]
        initWorld
        info "World construction finished, beginning game..."
        )
    (!w2 :: World wm) <- timeAction "Running" $ liftIO $ runGame w fullTitle $ do
      wa <- get @(WorldActions wm)
      --when I write a proper game loop, this is where it needs to go
      failHorriblyIfMissing (runRulebook (wa ^. whenPlayBegins) ())
      mapM_ (parseAction (ActionOptions False)) actionsToDo
      pass
    --  print rs
    let flushBufferToText w' = runPure $ runState w' $ do
          -- take it down and flip it around
          msgList <- use (messageBuffer % msgBufBuffer % reversed)
          return $ (mconcat . map show) msgList
    let (x, _) = flushBufferToText w2
        amendedOutput = case w2 ^. worldMetadata % errorLog of
          [] -> x
          xs -> x <> "\nEncountered the following errors:  \n" <> unlines xs
    let ex = mconcat (expectTitle fullTitle : expected )
    unless (amendedOutput == ex) (liftIO $ throwIO $ DiffException amendedOutput ex)

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
expectLooking t "" = expectLine t <> expectLine ""
expectLooking t d = expectLine t <> expectLine "" <> expectLine d 

expectAction :: Text -> Text
expectAction a = expectLine $ "> " <> a <> "\n"