module Sound.MIDI.Internal.Types where

import Data.Word (Word8)

class ToWord8 a where
  toWord8 :: a -> Word8

newtype Note = Note Word8

instance ToWord8 Note where
  toWord8 (Note n) = n

newtype Channel = Channel Word8

instance ToWord8 Channel where
  toWord8 (Channel n) = n
