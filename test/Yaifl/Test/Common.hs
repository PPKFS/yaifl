module Yaifl.Test.Common where

import Solitude

import Breadcrumbs
import Data.Char (isSpace)
import Language.Haskell.TH
import Language.Haskell.TH.Quote hiding (quoteExp)
import Yaifl
import Yaifl.Model.Action
import Yaifl.Model.Query
import Yaifl.Model.Rules.Run
import Yaifl.Text.ResponseCollection
import Yaifl.Model.Rules.RuleEffects

import Yaifl.Game.World
import qualified Data.Text as T
import Yaifl.Text.AdaptiveNarrative
import Yaifl.Text.Print
import Yaifl.Text.Verb
import Yaifl.Model.Actions.Args

expQQ :: (String -> Q Exp) -> QuasiQuoter
expQQ quoteExp = QuasiQuoter quoteExp notSupported notSupported notSupported where
  notSupported _ = fail "Quotation in this context is not supported"

wrappedText :: QuasiQuoter
wrappedText = expQQ
  (return . LitE . StringL . toString . T.strip . newlinesToWrap . unindent . tabsToSpaces . toText)

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
  x -> if T.empty == acc || T.last acc == '\n' then acc <> x else acc <> " " <> x) "" . lines

data ConstructionOptions wm = ConstructionOptions
  { activityCollectionBuilder :: ActivityCollection wm -> ActivityCollector wm
  , responseCollectionBuilder :: ResponseCollection wm -> ResponseCollector wm
  }

defaultOptions :: ConstructionOptions PlainWorldModel
defaultOptions = ConstructionOptions ActivityCollector ResponseCollector

testHarness ::
  forall wm a.
  HasStandardProperties wm
  => HasCallStack
  => Bool
  -> Text
  -> [Text]
  -> ConstructionOptions wm
  -> Game wm a
  -> IO Text
testHarness allTenses fullTitle actionsToDo conOptions initWorld = do
  fst <$$> runGame (blankWorld (activityCollectionBuilder conOptions) (responseCollectionBuilder conOptions)) blankActionCollection $ do
      output <- withSpan' "test run" fullTitle $ do
        withSpan' "worldbuilding" fullTitle $ do
          newWorld
          initWorld
          -- this just moves the actions from the indexed, static, standard library collection
          -- into the dynamic collection
          -- we do it here because we need to copy over changes to actions and we can't modify WrappedActions directly
          addStandardActions
        --withSpan "world verification" fullTitle $ do
        let runWorld suffix = do
              withSpan' ("run " <> suffix) fullTitle $ do
                wa <- get @(WorldActions wm)
                unless (suffix == "") $ printLn suffix
                --when I write a proper game loop, this is where it needs to go
                failHorriblyIfMissing (runRulebook Nothing False (wa ^. #whenPlayBegins) ())
                mapM_ (parseAction (ActionOptions False False) [NoParameter]) actionsToDo
                (w2 :: World wm) <- get
                let (x, _) = runPureEff $ runStateShared w2 $ do
                      -- take it down and flip it around
                      msgList <- gets (view $ #messageBuffer % #buffer % reversed)
                      return $ (mconcat . map show) msgList
                pure $ case w2 ^. #metadata % #errorLog of
                  [] -> x <> "\n"
                  xs -> x <> "\nEncountered the following errors:  \n" <> unlines (reverse xs)
        w <- get
        if allTenses
          then do
            mconcat <$> sequence [ (do
              put (updateNarrative x y w)
              runWorld ("Tense: " <> show y <> " | Viewpoint: " <> show x)) | x <- universe, y <- universe ]
          else runWorld ""
      flush
      pure output

updateNarrative ::
  VerbPersonage
  -> Tense
  -> World wm
  -> World wm
updateNarrative p t w = w & #adaptiveNarrative % #narrativeViewpoint .~ p & #adaptiveNarrative % #tense .~ t

readTraceId :: IO TraceID
readTraceId = TraceID <$> readFileBS "traceid.temp"
