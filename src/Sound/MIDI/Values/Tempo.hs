module Sound.Midi.Values.Tempo where

import Data.Word.Word24
import Sound.Midi.Internal.Types (PPQN (..))

ppqn :: Word24 -> PPQN
ppqn = PPQN

bpm :: Int -> PPQN
bpm = undefined

smpte :: Int -> PPQN
smpte = undefined
