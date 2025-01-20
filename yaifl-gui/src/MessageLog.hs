{-# LANGUAGE RecordWildCards #-}
module MessageLog where

import Yaifl.Prelude hiding (Reader)
import Yaifl.Text.Print
import Prettyprinter
import Rogue.Rendering.Viewport
import qualified Data.Text.Lazy.Builder as TLB
import qualified Data.Text as T
import qualified Rogue.Geometry.Rectangle as R
import BearLibTerminal
import BearLibTerminal.Raw (Dimensions(..))
import Rogue.Colour (toHex)
import Effectful.Reader.Static (Reader)
import Rogue.Geometry.V2
import Rogue.Colour (Colour(..))

data Message = Message
  { doc :: [StyledDoc MessageAnnotation]
  , laidOut :: SimpleDocStream MessageAnnotation
  , height :: Int
  }  deriving stock (Show, Generic)

data MessageLog layers = MessageLog
  { viewport :: Viewport layers
  , log :: [Message]
  --, scrollbar :: Maybe Scrollbar
  } deriving stock (Show, Generic)

data MessageLogAnchor = AnchorTop | AnchorBottom
  deriving stock (Eq, Ord, Bounded, Enum, Show, Generic)

renderMessageLog ::
  forall (l :: Type) es.
  AsLayer l
  => Enum l
  => Bounded l
  => IOE :> es
  => MessageLogAnchor
  -> MessageLog l
  -> Eff es ()
renderMessageLog anchor ml = renderViewport (ml ^. #viewport) $ go (ml ^. #viewport % #viewport % to R.height) (log ml)
  where
    go :: Int -> [Message] -> Eff (Reader (Viewport l) ': es) ()
    go _ [] = pass
    go remainingHeight (x:xs) = do
      let borderAnchor = if anchor == AnchorTop then (\a -> a + 1) else (\a -> a - 1)
          withoutBorder = if hasBorder (ml ^. #viewport) then \y -> V2 1 (borderAnchor y) else \y -> V2 0 y
          printLocation = withoutBorder (if anchor == AnchorTop then (ml ^. #viewport % #viewport % to R.height) - remainingHeight else remainingHeight - view #height x)
      --putStrLn $ "message height is" <> show (view #height x) <> " " <> show (length xs)
      viewportPrint @l printLocation Nothing (Colour 0xFF000000) $ (renderAnnotated . laidOut $ x)
      -- only continue if we've got at least part of one more message space
      when (remainingHeight > 0) $ go (remainingHeight - view #height x) xs


addMessage :: IOE :> es => [StyledDoc MessageAnnotation] -> MessageLog layers -> Eff es (MessageLog layers)
addMessage doc ml = if null doc then return ml else do
  msg <- makeMessage (view #viewport ml) doc
  if (view #height msg) > 100000000 then putStrLn (show msg <> "string measured wrongly?") >> return ml else return $ ml & #log %~ (msg:)

makeMessage :: IOE :> es => Viewport l -> [StyledDoc MessageAnnotation] -> Eff es Message
makeMessage vp doc = let
  docStream = layoutSmart (defaultLayoutOptions { layoutPageWidth = AvailablePerLine (vp ^. #viewport % to R.width) 1.0}) (mconcat doc)
  renderedText = renderAnnotated docStream
  in do
    (Dimensions _ height) <- terminalMeasureExtText (vp ^. #viewport % to R.width) (vp ^. #viewport % to R.height) renderedText
    return $ Message { doc, laidOut = docStream, height }

renderAnnotated :: SimpleDocStream MessageAnnotation -> Text
renderAnnotated =
    let push x = (x :)
        unsafePeek [] = error "failed to render text"
        unsafePeek (x:_) = x
        unsafePop [] = error "failed to render text"
        unsafePop (x:xs) = (x, xs)
        go :: [MessageAnnotation] -> SimpleDocStream MessageAnnotation -> TLB.Builder
        go s sds = case sds of
            SFail -> error "failed to render text"
            SEmpty -> mempty
            SChar c rest -> TLB.singleton c <> go s rest
            SText _ t rest -> TLB.fromText t <> go s rest
            SLine i rest -> TLB.singleton '\n' <> TLB.fromText (T.replicate i " ") <> go s rest
            SAnnPush style rest ->
                let currentStyle = unsafePeek s
                    newStyle = style <> currentStyle
                in  TLB.fromText (styleToRawText newStyle) <> go (push style s) rest
            SAnnPop rest ->
                let (currentStyle, s') = unsafePop s
                    newStyle = unsafePeek s'
                in  TLB.fromText (getTagsToEnd currentStyle newStyle <> styleToRawText newStyle) <> go s' rest

    in toStrict . TLB.toLazyText . go [mempty]

getTagsToEnd :: MessageAnnotation -> MessageAnnotation -> Text
getTagsToEnd m1 m2 = let MessageAnnotation{..} = getDifferences m1 m2 in mconcat
  [ maybe "" (const "[/color]") foregroundAnnotation
  , maybe "" (const "[/bgcolor]") backgroundAnnotation
  , maybe "" (const "[/font]") (case (boldAnnotation, italics, underlined) of
      (Nothing, Nothing, Nothing) -> Nothing
      _ -> Just ())
  ]

getDifferences :: MessageAnnotation -> MessageAnnotation -> MessageAnnotation
getDifferences m1 m2 = MessageAnnotation
  { foregroundAnnotation = preferSecond (foregroundAnnotation m1) (foregroundAnnotation m2)
  , backgroundAnnotation = preferSecond (backgroundAnnotation m1) (backgroundAnnotation m2)
  , boldAnnotation = if boldAnnotation m1 == boldAnnotation m2 then Nothing else Just Bold
  , italics = preferSecond (italics m1) (italics m2)
  , underlined = preferSecond (underlined m1) (underlined m2)
  }
  where
    preferSecond a b = case (a, b) of
      (Just x, Nothing) -> Just x
      (_, Just y) -> Just y
      (Nothing, Nothing) -> Nothing

styleToRawText :: MessageAnnotation -> Text
styleToRawText MessageAnnotation{..} = mconcat
  [ maybe "" (\c -> "[color=#"<>toHex c<>"]") foregroundAnnotation
  , maybe "" (\c -> "[bgcolor=#"<>toHex c<>"]") backgroundAnnotation
  , maybe "" (\x -> "[font="<>x<>"]") (case (boldAnnotation, italics, underlined) of
      (Just _, _, _) -> Just "bold"
      _ -> Nothing)
  ]
