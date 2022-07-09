module Yaifl.Test.Common where

import Solitude

import Yaifl
import qualified Data.Text as T
import Language.Haskell.TH.Quote hiding (quoteExp)
import Language.Haskell.TH
import Data.Char (isSpace)
import Yaifl.Core.World
import Yaifl.Core.Say
--import Yaifl.Core.Objects.Missing
--import Yaifl.Core.Rulebooks.Rulebook
import Yaifl.Core.Logger
import Cleff.State (runState, get)
import Test.Syd
import Yaifl.Core.Objects.Query
import Yaifl.Core.Actions.Action
import Yaifl.Core.Rulebooks.Run
--import Yaifl.Core.Rulebooks.WhenPlayBegins
--import Yaifl.Core.Actions.Action

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

testHarness ::
  forall wm a. 
  HasStandardProperties wm
  => HasCallStack
  => Text
  -> Game wm a
  -> [Text]
  -> [Text]
  -> IO ()
testHarness fullTitle initWorld actionsToDo expected = do
  let (t, shortName) = first (T.dropEnd 3) $ T.breakOnEnd " - " fullTitle
  (w2 :: World wm) <- liftIO $ runGame shortName (do
    newWorld
    info $ bformat ("Building world " %! stext %! "...") shortName
    initWorld
    info $ bformat "World construction finished, beginning game..."
    wa <- get @(WorldActions wm)
    --when I write a proper game loop, this is where it needs to go
    withoutMissingObjects (runRulebook (wa ^. whenPlayBegins) ()) (handleMissingObject "Failed when beginning" (return $ Just False)))
    --do the commands...
   -- rs <- mapM parseAction actionsToDo
  --  print rs
  let flushBufferToText w = runPure $ runState w $ do
        -- take it down and flip it around
        msgList <- use (messageBuffer % msgBufBuffer % reversed)
        return $ (mconcat . map show) msgList
  let (x, _) = flushBufferToText w2
      buildExpected = mconcat (expectTitle t : expected )
  putStrLn . T.unpack $ x
  x `shouldBe` buildExpected
{-
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
-}
expectLine :: Text -> Text
expectLine t1 = t1 <> "\n"

expectTitle :: Text -> Text
expectTitle = id . id -- introText

expectYouCanSee :: [Text] -> Text
expectYouCanSee t1 = expectLine ("You can see " <> listThings t1 <> " here.\n")

listThings :: [Text] -> Text
listThings t1 = mconcat $ zipWith (\x v -> x <> (if v < length t1 - 1 then ", " else "") <>
                (if v == length t1 - 2 then "and " else "")) t1 [0..]

expectLooking :: Text -> Text -> Text
expectLooking t d = expectLine t <> expectLine d