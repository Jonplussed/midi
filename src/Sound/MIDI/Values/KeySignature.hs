module Sound.Midi.Values.KeySignature where

import Data.Bits ((.|.), shiftL)
import Sound.Midi.Internal.Types
  (KeyChord (..), KeyNote (..), KeySignature (..))

major   = KeyChord 0
minor   = KeyChord 1

c       = KeyNote 0
cSharp  = KeyNote 7
dFlat   = KeyNote (-5)
d       = KeyNote 2
eFlat   = KeyNote (-3)
e       = KeyNote 4
f       = KeyNote (-1)
fSharp  = KeyNote 6
gFlat   = KeyNote (-6)
g       = KeyNote 1
aFlat   = KeyNote (-4)
a       = KeyNote 3
bFlat   = KeyNote (-2)
b       = KeyNote 5
cFlat   = KeyNote (-7)

keyOf :: KeyNote -> KeyChord -> KeySignature
keyOf (KeyNote note) (KeyChord chord) =
    KeySignature $ shiftL (fromIntegral note) 8 .|. fromIntegral chord
