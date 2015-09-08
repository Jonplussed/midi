module Sound.Midi.Internal.Types where

import Data.Word (Word8, Word16)
import Data.Word.Word24 (Word24)

newtype Channel         = Channel Word8
newtype ControllerIdent = ControllerIdent Word8
newtype ControllerValue = ControllerValue Word8
newtype DeltaTime       = DeltaTime [Word8]
newtype FileFormat      = FileFormat Word16
newtype MetaEvent       = MetaEvent Word16
newtype Note            = Note Word8
newtype Patch           = Patch Word8
newtype PitchWheel      = PitchWheel Word16
newtype PPQN            = PPQN Word24
newtype Pressure        = Pressure Word8
newtype Sequence        = Sequence Word16
newtype Ticks           = Ticks Word16
newtype Velocity        = Velocity Word8
newtype VoiceEvent      = VoiceEvent (Channel -> Word8)
