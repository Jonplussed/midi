module Sound.MIDI.BitOps (toHex, toBin) where

import Data.Bits
import Data.Word

toHex :: (Integral a, Bits a) => a -> String
toHex num = "0x" ++ map showNybble indices
  where
    pick i = 0xf .&. shiftR num i -- select the nybble at index i.
    showNybble i = "0123456789abcdef" !! (fromIntegral $ pick i) -- -1 < (pick i) < 16
    indices = reverse [0, 4..(bitSize num - 1)]

toBin :: (Integral a, Bits a) => a -> String
toBin n = reverse $ map toBit [0..bitSize n - 1]
  where
    toBit i = if n .&. bit i == 0
              then '0'
              else '1'
