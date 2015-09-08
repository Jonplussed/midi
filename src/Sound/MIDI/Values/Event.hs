module Sound.Midi.Values.Event
( noteOff
, noteOn
, afterTouch
, controlChange
, patchChange
, channelPressure
, pitchWheelChange
, sequenceNumber
, textArbitrary
, textCopyright
, textTrackName
, textInstruName
, textLyric
, textMarker
, textCuePoint
, endOfTrack
, setTempo
, setTimeSig
, setKeySig
) where

import Data.Bits ((.|.))
import Data.Word (Word8, Word16)
import Sound.Midi.Internal.Types
  (Channel (..), VoiceEvent (..), MetaEvent (..))

noteOff           = voiceEvent 0x80
noteOn            = voiceEvent 0x90
afterTouch        = voiceEvent 0xA0
controlChange     = voiceEvent 0xB0
patchChange       = voiceEvent 0xC0
channelPressure   = voiceEvent 0xD0
pitchWheelChange  = voiceEvent 0xE0

sequenceNumber    = metaEvent 0x00
textArbitrary     = metaEvent 0x01
textCopyright     = metaEvent 0x02
textTrackName     = metaEvent 0x03
textInstruName    = metaEvent 0x04
textLyric         = metaEvent 0x05
textMarker        = metaEvent 0x06
textCuePoint      = metaEvent 0x07
endOfTrack        = metaEvent 0x2F
setTempo          = metaEvent 0x51
setTimeSig        = metaEvent 0x58
setKeySig         = metaEvent 0x59

-- private functions

voiceEvent :: Word8 -> VoiceEvent
voiceEvent ident = VoiceEvent $ \(Channel n) -> ident .|. n

-- all meta-events are preceded with the prefix 0xFF00
metaEvent :: Word16 -> MetaEvent
metaEvent ident = MetaEvent $ 0xFF00 .|. ident
