module Sound.Midi.Internal.Types where

import Data.Word (Word8)

newtype Channel     = Channel Word8
newtype VoiceEvent  = VoiceEvent (Channel -> Word8)
newtype FileFormat  = FileFormat Word8
newtype MetaEvent   = MetaEvent Word8
newtype Note        = Note Word8
newtype Pressure    = Pressure Word8
newtype Velocity    = Velocity Word8
