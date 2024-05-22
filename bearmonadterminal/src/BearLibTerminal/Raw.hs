{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}

module BearLibTerminal.Raw where

import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr
import Foreign.Storable
import Control.Monad.IO.Class
import Data.ByteString ( ByteString )
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text.Foreign as TF
import Foreign.Marshal.Array (withArray)
import Foreign.Marshal.Alloc
import qualified Data.Text.Foreign as T

data Dimensions = Dimensions
  { width :: Int
  , height :: Int
  } deriving stock (Show)

instance Storable Dimensions where
  sizeOf _ = 8
  alignment _ = 4
  poke p Dimensions{..} = do
    pokeByteOff p 0 width
    pokeByteOff p 4 height
  peek p = do
    width <- peekByteOff p 0
    height <- peekByteOff p 4
    return $ Dimensions width height

asBool :: CInt -> Bool
asBool = (== 1)

foreign import capi safe "BearLibTerminal.h terminal_open" c_terminal_open :: IO CInt

terminalOpen :: MonadIO m => m Bool
terminalOpen = asBool <$> liftIO c_terminal_open

foreign import capi safe "BearLibTerminal.h terminal_close" c_terminal_close :: IO ()

terminalClose :: MonadIO m => m ()
terminalClose = liftIO c_terminal_close

foreign import capi safe "BearLibTerminal.h terminal_set" c_terminal_set :: CString -> IO CInt

bsToCString :: MonadIO m => (CString -> IO a) -> ByteString -> m a
bsToCString f = liftIO . flip BS.useAsCString f

textToCString :: MonadIO m => (CString -> IO a) -> Text -> m a
textToCString f = liftIO . flip TF.withCString f

terminalSetCString :: MonadIO m => CString -> m Bool
terminalSetCString = liftIO . (fmap asBool . c_terminal_set)

terminalSetBS :: MonadIO m => ByteString -> m Bool
terminalSetBS = bsToCString terminalSetCString

terminalSetText :: MonadIO m => Text -> m Bool
terminalSetText = textToCString terminalSetCString

foreign import capi safe "BearLibTerminal.h terminal_color" c_terminal_color_uint :: CUInt -> IO ()
foreign import capi safe "BearLibTerminal.h terminal_color" c_terminal_color_from_name :: CString -> IO ()

terminalColorUInt :: MonadIO m => CUInt -> m ()
terminalColorUInt = liftIO . c_terminal_color_uint

terminalColorNameCString :: MonadIO m => CString -> m ()
terminalColorNameCString = liftIO . c_terminal_color_from_name

terminalColorNameText :: MonadIO m => Text -> m ()
terminalColorNameText = textToCString terminalColorNameCString

terminalColorNameBS :: MonadIO m => ByteString -> m ()
terminalColorNameBS = bsToCString terminalColorNameCString

foreign import capi safe "BearLibTerminal.h terminal_bkcolor" c_terminal_bkcolor_uint :: CUInt -> IO ()
foreign import capi safe "BearMonadTerminal.h terminal_bkcolor_from_name" c_terminal_bkcolor_from_name :: CString -> IO ()

terminalBkColorUInt :: MonadIO m => CUInt -> m ()
terminalBkColorUInt = liftIO . c_terminal_bkcolor_uint

terminalBkColorNameCString :: MonadIO m => CString -> m ()
terminalBkColorNameCString = liftIO . c_terminal_bkcolor_from_name

terminalBkColorNameText :: MonadIO m => Text -> m ()
terminalBkColorNameText = textToCString terminalBkColorNameCString

terminalBkColorNameBS :: MonadIO m => ByteString -> m ()
terminalBkColorNameBS = bsToCString terminalBkColorNameCString

foreign import capi safe "BearLibTerminal.h terminal_composition" c_terminal_composition :: CInt -> IO ()

data TerminalCompositionMode = CompositionOn | CompositionOff
class ToCode tk where
  toCode :: tk -> CInt

instance ToCode TerminalCompositionMode where
  toCode CompositionOn = 1
  toCode CompositionOff = 0

terminalComposition :: MonadIO m => TerminalCompositionMode -> m ()
terminalComposition = liftIO . c_terminal_composition . toCode

foreign import capi safe "BearLibTerminal.h terminal_layer" c_terminal_layer :: CInt -> IO ()

terminalLayer :: MonadIO m => Int -> m ()
terminalLayer = liftIO . c_terminal_layer . fromIntegral

foreign import capi safe "BearLibTerminal.h terminal_clear" c_terminal_clear :: IO ()

terminalClear :: MonadIO m => m ()
terminalClear = liftIO c_terminal_clear

foreign import capi safe "BearLibTerminal.h terminal_clear_area" c_terminal_clear_area :: CInt -> CInt -> CInt -> CInt -> IO ()

terminalClearArea :: MonadIO m => Int -> Int -> Int -> Int -> m ()
terminalClearArea x y w h = liftIO (c_terminal_clear_area (fromIntegral x) (fromIntegral y) (fromIntegral w) (fromIntegral h))

data Rectangle a = Rectangle
  { x :: a
  , y :: a
  , width :: a
  , height :: a
  }

terminalClearRect :: MonadIO m => Rectangle Int -> m ()
terminalClearRect Rectangle{..} = terminalClearArea x y width height

foreign import capi safe "BearLibTerminal.h terminal_crop" c_terminal_crop :: CInt -> CInt -> CInt -> CInt -> IO ()

terminalCrop :: MonadIO m => Int -> Int -> Int -> Int -> m ()
terminalCrop x y w h = liftIO (c_terminal_crop (fromIntegral x) (fromIntegral y) (fromIntegral w) (fromIntegral h))

terminalCropRect :: MonadIO m => Rectangle Int -> m ()
terminalCropRect Rectangle{..} = terminalCrop x y width height

foreign import capi safe "BearLibTerminal.h terminal_refresh" c_terminal_refresh :: IO ()

terminalRefresh :: MonadIO m => m ()
terminalRefresh = liftIO c_terminal_refresh

foreign import capi safe "BearLibTerminal.h terminal_put" c_terminal_put :: CInt -> CInt -> CInt -> IO ()

terminalPut :: MonadIO m => Int -> Int -> Int -> m ()
terminalPut x y c = liftIO $ c_terminal_put (fromIntegral x) (fromIntegral y) (fromIntegral c)

foreign import capi unsafe "BearLibTerminal.h terminal_pick" c_terminal_pick :: CInt -> CInt -> CInt -> IO CInt

terminalPick :: MonadIO m => Int -> Int -> Int -> m Int
terminalPick x y i = liftIO $ fromIntegral <$> c_terminal_pick (fromIntegral x) (fromIntegral y) (fromIntegral i)

foreign import capi unsafe "BearLibTerminal.h terminal_pick_color" c_terminal_pick_color :: CInt -> CInt -> CInt -> IO CUInt

terminalPickColor :: MonadIO m => Int -> Int -> Int -> m Integer
terminalPickColor x y i = liftIO $ fromIntegral <$> c_terminal_pick_color (fromIntegral x) (fromIntegral y) (fromIntegral i)

foreign import capi unsafe "BearLibTerminal.h terminal_pick_bkcolor" c_terminal_pick_bkcolor :: CInt -> CInt -> IO CUInt

terminalPickBkColor :: MonadIO m => Int -> Int -> m Integer
terminalPickBkColor x y = liftIO $ fromIntegral <$> c_terminal_pick_bkcolor (fromIntegral x) (fromIntegral y)

foreign import capi safe "BearLibTerminal.h terminal_put_ext" c_terminal_put_ext :: CInt -> CInt -> CInt -> CInt -> CInt -> Ptr CUInt -> IO ()

terminalPutExt :: MonadIO m => Int -> Int -> Int -> Int -> Int -> Maybe (Integer, Integer, Integer, Integer) -> m ()
terminalPutExt x y dx dy code mbColors = liftIO $ colorsToArr $ c_terminal_put_ext (fromIntegral x) (fromIntegral y) (fromIntegral dx) (fromIntegral dy) (fromIntegral code)
  where
    colorsToArr :: (Ptr CUInt -> IO a) -> IO a
    colorsToArr f = case mbColors of
      Nothing -> f nullPtr
      Just (tl, bl, br, tr) -> withArray (map fromIntegral [tl, bl, br, tr]) f

foreign import capi safe "BearMonadTerminal.h terminal_print_ptr" c_terminal_print_ptr :: CInt -> CInt -> CString -> Ptr Dimensions -> IO ()

terminalPrintCString :: MonadIO m => Int -> Int -> CString -> m Dimensions
terminalPrintCString x y c = liftIO $ alloca (\dim -> c_terminal_print_ptr (fromIntegral x) (fromIntegral y) c dim >> peek dim)

terminalPrintBS :: MonadIO m => Int -> Int -> ByteString -> m Dimensions
terminalPrintBS x y = bsToCString (terminalPrintCString x y)

terminalPrintText :: MonadIO m => Int -> Int -> Text -> m Dimensions
terminalPrintText x y = textToCString (terminalPrintCString x y)

foreign import capi safe "BearMonadTerminal.h terminal_print_ext_ptr" c_terminal_print_ext_ptr :: CInt -> CInt -> CInt -> CInt -> CInt -> CString -> Ptr Dimensions -> IO ()

terminalPrintExtCString :: MonadIO m => Int -> Int -> Int -> Int -> Int -> CString -> m Dimensions
terminalPrintExtCString x y w h align c = liftIO $ alloca (\dim -> c_terminal_print_ext_ptr (fromIntegral x) (fromIntegral y) (fromIntegral w) (fromIntegral h) (fromIntegral align) c dim >> peek dim)

terminalPrintExtRectCString :: MonadIO m => Rectangle Int -> Int -> CString -> m Dimensions
terminalPrintExtRectCString Rectangle{..} = terminalPrintExtCString x y width height

terminalPrintExtBS :: MonadIO m => Int -> Int -> Int -> Int -> Int -> ByteString -> m Dimensions
terminalPrintExtBS x y w h align = bsToCString (terminalPrintExtCString x y w h align)

terminalPrintExtRectBS :: MonadIO m => Rectangle Int -> Int -> ByteString -> m Dimensions
terminalPrintExtRectBS Rectangle{..} = terminalPrintExtBS x y width height

terminalPrintExtText :: MonadIO m => Int -> Int -> Int -> Int -> Int -> Text -> m Dimensions
terminalPrintExtText x y w h align = textToCString (terminalPrintExtCString x y w h align)

terminalPrintExtRectText :: MonadIO m => Rectangle Int -> Int -> Text -> m Dimensions
terminalPrintExtRectText Rectangle{..} = terminalPrintExtText x y width height

-- I don't know if wchar is actually useful here.
-- I don't care enough to try and wrap va_list around the printf variants.
-- so that's printf, printf_ext, wprint, wprintf, wprint_ext, wprintf_ext, measuref, wmeasure, measuref_ext, wmeasuref_ext
-- check is unnecessary
-- foreign import capi unsafe "BearLibTerminal.h terminal_check" c_terminal_check :: CInt -> IO CInt
-- also read_wstr
-- not bothering with: color_from_name, color_from_argb

foreign import capi unsafe "BearMonadTerminal.h terminal_measure_ptr" c_terminal_measure_ptr :: CString -> Ptr Dimensions -> IO ()

terminalMeasureCString :: MonadIO m => CString -> m Dimensions
terminalMeasureCString c = liftIO $ alloca (\dim -> c_terminal_measure_ptr c dim >> peek dim)

terminalMeasureBS :: MonadIO m => ByteString -> m Dimensions
terminalMeasureBS = bsToCString terminalMeasureCString

terminalMeasureText :: MonadIO m => Text -> m Dimensions
terminalMeasureText = textToCString terminalMeasureCString

foreign import capi unsafe "BearMonadTerminal.h terminal_measure_ext_ptr" c_terminal_measure_ext_ptr :: CInt -> CInt -> CString -> Ptr Dimensions -> IO ()

terminalMeasureExtCString :: MonadIO m => Int -> Int -> CString -> m Dimensions
terminalMeasureExtCString w h c = liftIO $ alloca (\dim -> c_terminal_measure_ext_ptr (fromIntegral w) (fromIntegral h) c dim >> peek dim)

terminalMeasureExtBS :: MonadIO m => Int -> Int -> ByteString -> m Dimensions
terminalMeasureExtBS w h = bsToCString (terminalMeasureExtCString w h)

terminalMeasureExtText :: MonadIO m => Int -> Int -> Text -> m Dimensions
terminalMeasureExtText w h = textToCString (terminalMeasureExtCString w h)

foreign import capi safe "BearLibTerminal.h terminal_state" c_terminal_state :: CInt -> IO CInt

terminalStateCode :: MonadIO m => ToCode c => c -> m Int
terminalStateCode = liftIO . fmap fromIntegral . c_terminal_state . toCode

foreign import capi safe "BearLibTerminal.h terminal_has_input" c_terminal_has_input :: IO CInt

terminalHasInput :: MonadIO m => m Bool
terminalHasInput = liftIO $ asBool <$> c_terminal_has_input

foreign import capi safe "BearLibTerminal.h terminal_read" c_terminal_read :: IO CInt

terminalReadCode :: MonadIO m => m Int
terminalReadCode = liftIO $ fromIntegral <$> c_terminal_read

foreign import capi safe "BearLibTerminal.h terminal_peek" c_terminal_peek :: IO CInt

terminalPeekCode :: MonadIO m => m Int
terminalPeekCode = liftIO $ fromIntegral <$> c_terminal_peek

foreign import capi safe "BearLibTerminal.h terminal_read_str" c_read_str :: CInt -> CInt -> Ptr CChar -> CInt -> IO CUInt

terminalReadStr :: MonadIO m => Int -> Int -> Int -> m (Maybe Text)
terminalReadStr x y m = liftIO $ alloca (\c -> c_read_str (fromIntegral x) (fromIntegral y) c (fromIntegral m) >>=
  \res -> if res == -1 then return Nothing else Just <$> T.peekCStringLen (c, fromIntegral res))

foreign import capi safe "BearLibTerminal.h terminal_delay" c_terminal_delay :: CInt -> IO ()

terminalDelay :: MonadIO m => Int -> m ()
terminalDelay = liftIO . c_terminal_delay . fromIntegral


