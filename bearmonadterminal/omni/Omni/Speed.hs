module Omni.Speed (testSpeed) where
import BearLibTerminal.Raw
import Control.Monad
import System.Random
import Data.Word8
import Foreign.C.Types
import qualified Data.ByteString as B
import Data.Bits
import qualified Data.Vector as V
import Data.Time.Clock
import Data.Fixed
import Data.Time.Clock.POSIX

testSpeed :: IO ()
testSpeed = do
  terminalSetText "window.title='Omni: synchronous rendering"
  terminalComposition CompositionOn
  terminalSetText "output.vsync=false"
  (s, r) <- setupSpeedDemo
  print s
  t <- nominalDiffTimeToSeconds <$> getPOSIXTime
  runLoop (0, t) 0 0 (V.fromList s)

data Color a = Color !a !a !a
shiftColor :: Int -> Color Word8
shiftColor s =
  let (f :: Float) = fromIntegral (s `mod` 80) / 80.0
      (Color r g b) = if
            | f < 0.33
                -> Color (255 * ((0.33-f)/0.33)) (255 * ((f-0.0)/0.33)) 0
            | f < 0.66
                -> Color 0 (255 * ((0.66-f)/0.33)) (255 * ((f-0.33)/0.33))
            | otherwise
                -> Color (255 * ((f-0.66)/0.33)) 0 (255 * ((1.0-f)/0.33))
  in Color (min 255 (round r)) (min 255 (round r)) (min 255 (round r))

highlightedColor :: Color Word8 -> CUInt
highlightedColor (Color r g b) = fromByteString $ B.pack [255, r, g, b]

dimmedColor :: Color Word8 -> CUInt
dimmedColor (Color r g b) = fromByteString $ B.pack [255, r, g, b]

fromByteString :: (Num a, Bits a) => B.ByteString -> a
fromByteString = B.foldl go 0
    where go acc i = (acc  `shiftL` 8) .|. (fromIntegral i)

setupSpeedDemo :: IO ([(CUInt{-, CUInt-})], [Int])
setupSpeedDemo = do
  let shifted = map (\i -> let c = shiftColor i in (highlightedColor c, dimmedColor c)) [0..80]
  r0 <- mapM (const $ randomRIO (0, 255)) [(0 :: Int)..2000]
  pure (map fst shifted, r0)
  {-

  unsigned int shift_f = 0, shift_b = 0, shift_f2 = 0;
  float shift_f2f = 0;

  color_t shifted_f[80], shifted_b[80];
  for (int i = 0; i < 80; i++)
  {
    color3_t c = GetShiftedColor(i);
    shifted_b[i] = GetHighlightedColor(c);
    shifted_f[i] = GetDimmedColor(c);
  }

  uint64_t fps_update_time = GetTime();
  int fps_counter = 0;
  int fps_value = 0;
  bool vsync = true;

  std::srand(std::time(nullptr));
  int r0[2000];
  for (int i=0; i<2000; i++) r0[i] = rand()%256;
-}
runLoop :: (Int, Pico) -> Int -> Int -> V.Vector CUInt -> IO ()
runLoop (i, c) s s2 v = do
  terminalClear
  forM_ [0..25] $ \y ->
    forM_ [0..80] $ \x -> (do
      terminalColorUInt (v V.! ((s+x+y) `mod` 80))
      -- terminal_color(shifted_b[(shift_b+x+y)%80]);
      terminalPut x y 0x2588
      terminalColorUInt $ ((v V.! ((s2+y-x) `mod` 80)) .&. 0x00FFFFFF) .|. 0x64000000
      -- terminal_color(color_from_another(100, shifted_b[(shift_f2+y-x)%80]));
      terminalPut x y 0x2588
      let (d :: Float) = abs $ 40.0 - fromIntegral (s `mod` 80)
      terminalColorUInt (fromByteString $ B.pack [ min 255 (fromIntegral $ round ((128.0*d)/40.0)), 255, 255, 255 ])
      -- int d = (int)std::fabs(40-(int)((shift_f)%80));
                        -- terminal_color(color_from_argb((int)(d/40.0f*128.0f), 255, 255, 255));
      terminalPut x y (fromEnum '0'))
      --terminalPut x y '0' + (r1+r0[y*80+x])%10);
  terminalRefresh
  t2 <- nominalDiffTimeToSeconds <$> getPOSIXTime
  if (t2 - c) > 1 then
    (do
      print i
      runLoop (0, t2) (s+1) (s2-2) v
    )
  else do
    --print (t2-c)
    --print (t2 - c)
    runLoop (i+1, c) (s+1) (s2-2) v
--408_130_000
{-
  for (bool proceed=true; proceed;)
  {
    int r1 = rand()%256;

    terminal_clear();
    for (int y = 0; y < 25; y++)
    {
      for (int x = 0; x < 80; x++)
      {
        terminal_color(shifted_b[(shift_b+x+y)%80]);
        terminal_put(x, y, 0x2588);
        terminal_color(color_from_another(100, shifted_b[(shift_f2+y-x)%80]));
        terminal_put(x, y, 0x2588);
        int d = (int)std::fabs(40-(int)((shift_f)%80));
        terminal_color(color_from_argb((int)(d/40.0f*128.0f), 255, 255, 255));
        terminal_put(x, y, '0' + (r1+r0[y*80+x])%10);
      }
    }
    terminal_printf(2, 1, "[color=black]vsync: %s\nFPS: %d", vsync? "yes": "no", fps_value);
    terminal_printf(2, 4, "[color=black]Press TAB to switch vsync on an off");
    terminal_refresh();


    fps_counter += 1;
    uint64_t time = GetTime();
    if (time > fps_update_time + 1000)
    {
      fps_value = fps_counter;
      fps_counter = 0;
      fps_update_time = time;
    }

    while (proceed && terminal_has_input())
    {
      int code = terminal_read();
      if (code == TK_ESCAPE || code == TK_CLOSE)
      {
        proceed = false;
      }
      else if (code == TK_TAB)
      {
        vsync = !vsync;
        terminal_setf("output.vsync=%s", vsync? "true": "false");
      }
    }

    shift_f -= 1;
    shift_f2 -= 2;
    shift_b += 1;

    shift_f2f -= 1.25f;
    shift_f2 = (int)shift_f2f;
  }

  terminal_set("output.vsync=true");
  terminal_composition(TK_OFF);
  -}