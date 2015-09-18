module Sound.Midi.Values.PPQN where

import Sound.Midi.Internal.Encoding.Value (PPQN (..))

ppqn :: Int -> PPQN
ppqn = PPQN . fromIntegral
