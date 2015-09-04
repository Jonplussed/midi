module Sound.Midi.Values.VoiceEvent
( noteOff
, noteOn
, afterTouch
, controlChange
, patchChange
, channelPressure
, pitchWheelChange
) where

import Data.Bits ((.|.))
import Data.Word (Word8)
import Sound.Midi.Internal.Types (Channel (..), VoiceEvent (..))

noteOff           = eventWithChannel 0x80
noteOn            = eventWithChannel 0x90
afterTouch        = eventWithChannel 0xA0
controlChange     = eventWithChannel 0xB0
patchChange       = eventWithChannel 0xC0
channelPressure   = eventWithChannel 0xD0
pitchWheelChange  = eventWithChannel 0xE0

-- private functions

eventWithChannel :: Word8 -> VoiceEvent
eventWithChannel byte = VoiceEvent $ \(Channel n) -> byte .|. n
