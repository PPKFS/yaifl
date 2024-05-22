{-# LANGUAGE RecordWildCards #-}
module BearMonadTerminal where

import BearLibTerminal.Raw
import Data.Text ( Text )
import Control.Monad.IO.Class (MonadIO (..))
import GHC.Generics
import Control.Concurrent (runInBoundThread)
import Foreign.C
import qualified Data.Text.Lazy.Builder as LT
import qualified Data.List as L
import Data.Maybe
import qualified Data.Text as T
import Data.Functor (void)
import Foreign.Marshal.Alloc
import Foreign.Storable
import qualified Data.Text.Lazy.Encoding as LT
import qualified Data.ByteString as BS
import qualified Data.Text.Lazy as TL

class BearLibConfigString s where
  toConfigString :: s -> LT.Builder

data Cellsize = Auto | Size (Int, Int)
  deriving stock (Eq, Ord, Show, Generic)

instance BearLibConfigString (Int, Int) where
  toConfigString (x, y) = LT.fromString (show x) <> LT.singleton 'x' <> LT.fromString (show y)

instance BearLibConfigString Cellsize where
  toConfigString Auto = LT.fromText "auto"
  toConfigString (Size s) = toConfigString s

instance BearLibConfigString Text where
  toConfigString s = LT.singleton '"' <> LT.fromText (T.replace "\"" "\"\"" s) <> LT.singleton '"'

instance BearLibConfigString String where
  toConfigString s = LT.singleton '"' <> LT.fromText (T.replace "\"" "\"\"" $ T.pack s) <> LT.singleton '"'

instance BearLibConfigString Bool where
  toConfigString True = LT.fromText "true"
  toConfigString False = LT.fromText "false"

newtype ConfigOption = ConfigOption { unConfig :: (Text, LT.Builder) }

instance BearLibConfigString ConfigOption where
  toConfigString (ConfigOption (t, v)) = LT.fromText t <> LT.singleton '=' <> v

toByteString :: BearLibConfigString c => c -> BS.ByteString
toByteString = BS.toStrict . LT.encodeUtf8 . LT.toLazyText . toConfigString

terminalSet :: MonadIO m => BearLibConfigString c => c -> m Bool
terminalSet = terminalSetText . TL.toStrict . LT.toLazyText . toConfigString

data WindowOptions = WindowOptions
  { size :: Maybe (Int, Int)
  , cellsize :: Maybe Cellsize
  , title :: Maybe Text
  , icon :: Maybe FilePath
  , resizeable :: Maybe Bool
  , fullscreen :: Maybe Bool
  } deriving stock (Show, Eq, Ord)

defaultWindowOptions :: WindowOptions
defaultWindowOptions = WindowOptions
  { size = Just (80, 125)
  , cellsize = Just Auto
  , title = Just "BearMonadTerminal"
  , icon = Nothing
  , resizeable = Just False
  , fullscreen = Just False
  }
instance BearLibConfigString WindowOptions where
  toConfigString WindowOptions{..} =
    let f :: Functor f => BearLibConfigString g => Text -> f g -> f ConfigOption
        f t = fmap (ConfigOption . (t,) . toConfigString)
        mkOptions = map toConfigString $ catMaybes
          [ f "size" size
          , f "cellsize" cellsize
          , f "title" title
          -- todo: work out how filepaths should work
          -- todo: this should probably be done with generics
          , f "icon" icon
          , f "resizeable" resizeable
          , f "fullscreen" fullscreen
          ]
    in
      case mkOptions of
        [] -> mempty
        opts -> LT.fromText "window: " <> mconcat (L.intersperse (LT.singleton ',') $ opts) <> LT.singleton ';'

makeWindow :: IO ()
makeWindow = void $ runInBoundThread $ do
  void c_terminal_open
  c_terminal_refresh
  c_terminal_refresh
  c_terminal_delay 1000
  c2 <- newCString "hello, world!🐒"
  v <- alloca (\p -> c_terminal_print_ptr 14 14 c2 p >> peek @Dimensions p)
  print v
  c_terminal_refresh
  c_terminal_delay 5000
  c_terminal_close
  return ()

initWindow :: MonadIO m => WindowOptions -> m ()
initWindow opts = do
  terminalOpen
  terminalSet opts
  return ()

 {-}
  liftIO $ c_terminal_refresh
  liftIO $ c_terminal_refresh
  liftIO $ c_terminal_delay 5000
  -}

omniMain :: MonadIO m => m ()
omniMain = do
  -- todo: font:default, input filter to keyboard
  initWindow defaultWindowOptions { title = Just "Omni: menu" }
  terminalColorNameText "white"
