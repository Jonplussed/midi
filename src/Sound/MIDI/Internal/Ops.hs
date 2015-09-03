module Sound.Midi.Internal.Ops where

import Data.Word (Word8)

percentWord8 :: Int -> Word8
percentWord8 per
    | per < 0   = percentWord8 0
    | per > 100 = percentWord8 100
    | otherwise = fromIntegral . round $ fromIntegral per / 100 * maxValue
  where
    maxValue = 127
