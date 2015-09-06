module Sound.Midi.Internal.Types where

import Data.Word (Word8, Word16)

newtype Channel         = Channel Word8
newtype ControllerIdent = ControllerIdent Word8
newtype ControllerValue = ControllerValue Word8
newtype DeltaTime       = DeltaTime [Word8]
newtype FileFormat      = FileFormat Word16
newtype MetaEvent       = MetaEvent Word8
newtype Note            = Note Word8
newtype Patch           = Patch Word8
newtype PitchWheel      = PitchWheel Word16
newtype Pressure        = Pressure Word8
newtype Ticks           = Ticks Word16
newtype Velocity        = Velocity Word8
newtype VoiceEvent      = VoiceEvent (Channel -> Word8)
