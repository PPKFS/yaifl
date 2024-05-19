{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
module BearMonadTerminal where

import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr
import Foreign.Storable

data Dimensions = Dimensions
  { width :: CInt
  , height :: CInt
  }

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

foreign import capi unsafe "BearLibTerminal.h terminal_open" c_terminal_open :: IO CInt
foreign import capi unsafe "BearLibTerminal.h terminal_close" c_terminal_close :: IO ()
foreign import capi unsafe "BearLibTerminal.h terminal_set" c_terminal_set :: CString -> IO CInt

foreign import capi unsafe "BearLibTerminal.h terminal_color" c_terminal_color :: CUInt -> IO ()
foreign import capi unsafe "BearLibTerminal.h terminal_bkcolor" c_terminal_bkcolor :: CUInt -> IO ()
foreign import capi unsafe "BearLibTerminal.h terminal_composition" c_terminal_composition :: CInt -> IO ()
foreign import capi unsafe "BearLibTerminal.h terminal_layer" c_terminal_layer :: CInt -> IO ()

foreign import capi unsafe "BearLibTerminal.h terminal_clear" c_terminal_clear :: IO ()
foreign import capi unsafe "BearLibTerminal.h terminal_clear_area" c_terminal_clear_area :: CInt -> CInt -> CInt -> CInt -> IO ()
foreign import capi unsafe "BearLibTerminal.h terminal_crop" c_terminal_crop :: CInt -> CInt -> CInt -> CInt -> IO ()
foreign import capi safe "BearLibTerminal.h terminal_refresh" c_terminal_refresh :: IO ()
foreign import capi unsafe "BearLibTerminal.h terminal_put" c_terminal_put :: CInt -> CInt -> CInt -> IO ()
foreign import capi unsafe "BearLibTerminal.h terminal_pick" c_terminal_pick :: CInt -> CInt -> CInt -> IO CInt
foreign import capi unsafe "BearLibTerminal.h terminal_pick_color" c_terminal_pick_color :: CInt -> CInt -> CInt -> IO CUInt
foreign import capi unsafe "BearLibTerminal.h terminal_pick_bkcolor" c_terminal_pick_bkcolor :: CInt -> CInt -> IO CUInt
foreign import capi unsafe "BearLibTerminal.h terminal_put_ext" c_terminal_put_ext :: CInt -> CInt -> CInt -> CInt -> CInt -> Ptr CUInt -> IO ()
foreign import capi unsafe "BearMonadTerminal.h terminal_print_ptr" c_terminal_print_ptr :: CInt -> CInt -> CString -> Ptr Dimensions -> IO ()
--foreign import capi unsafe "BearLibTerminal.h terminal_printf" c_terminal_printf :: CInt -> CInt -> CString -> IO CUInt
--foreign import capi unsafe "BearLibTerminal.h terminal_wprint" c_terminal_wprint :: CInt -> CInt -> CString -> IO ()
--foreign import capi unsafe "BearLibTerminal.h terminal_wprintf" c_terminal_wprint :: CInt -> CInt -> CString -> IO ()
--foreign import capi unsafe "BearLibTerminal.h terminal_print_ext" c_terminal_print_ext :: CInt -> CInt -> CInt -> CInt -> CInt -> CString -> IO CUInt
-- also missing f/w options here, and also for measure
--foreign import capi unsafe "BearLibTerminal.h terminal_measure" c_terminal_measure :: CString -> IO CUInt
--foreign import capi unsafe "BearLibTerminal.h terminal_measure_ext" c_terminal_measure_ext :: CInt -> CInt -> CString -> IO CUInt

foreign import capi unsafe "BearLibTerminal.h terminal_state" c_terminal_state :: CInt -> IO CInt
--foreign import capi unsafe "BearLibTerminal.h terminal_check" c_terminal_check :: CInt -> IO CInt
foreign import capi unsafe "BearLibTerminal.h terminal_has_input" c_terminal_has_input :: IO CInt
foreign import capi unsafe "BearLibTerminal.h terminal_read" c_terminal_read :: IO CInt
foreign import capi unsafe "BearLibTerminal.h terminal_peek" c_terminal_peek :: IO CInt
-- also read_wstr
foreign import capi unsafe "BearLibTerminal.h terminal_read_str" c_read_str :: CInt -> CInt -> Ptr CUChar -> CInt -> IO CUInt

-- not bothering with: terminal_delay, color_from_name, color_from_argb
