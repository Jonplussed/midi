module Sound.Midi.Values.FileFormat where

import Sound.Midi.Internal.Types (FileFormat (..))

singleTrack     = FileFormat 0
syncMultiTrack  = FileFormat 1
asyncMultiTrack = FileFormat 2
