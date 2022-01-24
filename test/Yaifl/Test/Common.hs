module Yaifl.Test.Common where

import Yaifl.Prelude
import Yaifl
import qualified Data.EnumMap as DEM
import qualified Data.Text as T
import Test.Hspec
import Yaifl.ObjectLookup (setObjectFrom)
import Language.Haskell.TH.Quote hiding (quoteExp)
import Language.Haskell.TH
import Data.Char (isSpace)

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