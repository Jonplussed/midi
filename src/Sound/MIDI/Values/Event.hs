module Sound.Midi.Values.VoiceEvent
( noteOff
, noteOn
, keyAfterTouch
, controlChange
, patchChange
, channelAfterTouch
, pitchWheelChange
) where

import Data.Bits ((.|.))
import Sound.Midi.Internal.Types (Channel (..), VoiceEvent (..))

noteOff           = VoiceEvent $ withChannel 0x80
noteOn            = VoiceEvent $ withChannel 0x90
keyAfterTouch     = VoiceEvent $ withChannel 0xA0
controlChange     = VoiceEvent $ withChannel 0xB0
patchChange       = VoiceEvent $ withChannel 0xC0
channelAfterTouch = VoiceEvent $ withChannel 0xD0
pitchWheelChange  = VoiceEvent $ withChannel 0xE0

-- private function

withChannel :: Word8 -> Channel -> Word8
withChannel byte (Channel n) -> byte .|. n
