module Yaifl.Test.Common where

import Solitude

import Breadcrumbs
import Data.Char (isSpace)
import Language.Haskell.TH
import Language.Haskell.TH.Quote hiding (quoteExp)
import Yaifl
import Yaifl.Core.Actions.Action
import Yaifl.Core.Metadata
import Yaifl.Core.Objects.Query
import Yaifl.Core.Rulebooks.Rule
import Yaifl.Core.Rulebooks.Run
import Yaifl.Core.Rulebooks.WhenPlayBegins (introText)
import Yaifl.Core.Say
import Yaifl.Core.World
import qualified Data.Text as T

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

force :: NFData a => a -> a
force a = deepseq a a

testHarness ::
  forall wm a.
  HasStandardProperties wm
  => HasCallStack
  => Text
  -> [Text]
  -> Game wm a
  -> IO Text
testHarness fullTitle actionsToDo initWorld = do
  tId <- readTraceId
  (_, !w2 :: World wm) <- runGame tId blankWorld $ do
      withSpan' "test run" fullTitle $ do
        withSpan' "worldbuilding" fullTitle $ do
          newWorld
          initWorld
        --withSpan "world verification" fullTitle $ do
        withSpan' "run" fullTitle $ do
          wa <- get @(WorldActions wm)
          --when I write a proper game loop, this is where it needs to go
          failHorriblyIfMissing (runRulebook Nothing (wa ^. #whenPlayBegins) ())
          mapM_ (parseAction (ActionOptions False Nothing)) actionsToDo
      flush
    --  print rs
  let flushBufferToText w' = runPureEff $ runStateShared w' $ do
          -- take it down and flip it around
          msgList <- gets (view $ #messageBuffer % #buffer % reversed)
          return $ (mconcat . map show) msgList
  let (x, _) = flushBufferToText w2
      amendedOutput = case w2 ^. #metadata % #errorLog of
        [] -> x
        xs -> x <> "\nEncountered the following errors:  \n" <> unlines xs
      {-(LB logs) = w2 ^. worldLogs
      finalisedLogs = reverse $ map (\(txts, ts, _, ms, txt) ->
        let cxt' = (let f = T.intercalate "|" (filter (/= T.empty) txts) in if T.empty == f then "" else "<"<>f<>">")
            logTy = case ms of
              Info -> "[Info]"
              Debug -> "[Debug]"
              Warning -> "[Warn]"
              Error -> "[Error]"
        in
        show ts <> logTy <> cxt' <> txt
        ) logs
  writeFile (toString $ "test/logs/" <> fullTitle) (toString $ unlines finalisedLogs)-}

  pure amendedOutput

readTraceId :: IO TraceID
readTraceId = TraceID <$> readFileBS "traceid.temp"

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