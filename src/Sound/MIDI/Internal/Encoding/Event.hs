module Sound.Midi.Internal.Encoding.Event
( encodeMetaChunk
, encodeVoiceChunk
) where

import Data.Bits ((.|.))
import Data.Monoid ((<>))
import Data.Word (Word8)

import qualified Data.ByteString as StrictBS
import qualified Data.ByteString.Lazy.Builder as Bld

import Sound.Midi.Internal.Encoding.Value
import Sound.Midi.Internal.Types

encodeMetaChunk :: MetaChunk -> Bld.Builder
encodeMetaChunk chunk =
    Bld.word8 0xFF <>
    Bld.word8 (metaChunkIdent chunk) <>
    Bld.word8 (metaArgSize chunk) <>
    metaChunkValue chunk

encodeVoiceChunk :: Channel -> VoiceChunk -> Bld.Builder
encodeVoiceChunk chan chunk =
    Bld.word8 (voiceChunkIdent chan chunk) <>
    voiceChunkValue chunk

-- private functions

metaArgSize :: MetaChunk -> Word8
metaArgSize chunk = case chunk of
    SequenceNumber _    -> 0x02
    TextArbitrary text  -> len text
    TextCopyright text  -> len text
    TextTrackName text  -> len text
    TextInstruName text -> len text
    TextLyric text      -> len text
    TextMarker text     -> len text
    TextCuePoint text   -> len text
    SetTempo _          -> 0x03
    SetTimeSig          -> 0x04
    SetKeySig _         -> 0x02
  where
    len = fromIntegral . StrictBS.length

metaChunkIdent :: MetaChunk -> Word8
metaChunkIdent chunk = case chunk of
    SequenceNumber _    -> 0x00
    TextArbitrary _     -> 0x01
    TextCopyright _     -> 0x02
    TextTrackName _     -> 0x03
    TextInstruName _    -> 0x04
    TextLyric _         -> 0x05
    TextMarker _        -> 0x06
    TextCuePoint _      -> 0x07
    SetTempo _          -> 0x51
    SetTimeSig          -> 0x58
    SetKeySig _         -> 0x59

metaChunkValue :: MetaChunk -> Bld.Builder
metaChunkValue chunk = case chunk of
    SequenceNumber seq  -> encode seq
    TextArbitrary text  -> Bld.byteString text
    TextCopyright text  -> Bld.byteString text
    TextTrackName text  -> Bld.byteString text
    TextInstruName text -> Bld.byteString text
    TextLyric text      -> Bld.byteString text
    TextMarker text     -> Bld.byteString text
    TextCuePoint text   -> Bld.byteString text
    SetTempo tempo      -> undefined
    SetTimeSig          -> undefined
    SetKeySig sig       -> encode sig

voiceChunkIdent :: Channel -> VoiceChunk -> Word8
voiceChunkIdent (Channel ch) chunk = case chunk of
    NoteOff _ _         -> withChan 0x80
    NoteOn _ _          -> withChan 0x90
    AfterTouch _ _      -> withChan 0xA0
    ControlChange _ _   -> withChan 0xB0
    PatchChange _       -> withChan 0xC0
    ChannelPressure _   -> withChan 0xD0
    PitchWheelChange _  -> withChan 0xE0
  where
    withChan = (.|. ch)

voiceChunkValue :: VoiceChunk -> Bld.Builder
voiceChunkValue chunk = case chunk of
    NoteOff note vel        -> encode note <> encode vel
    NoteOn note vel         -> encode note <> encode vel
    AfterTouch note pres    -> encode note <> encode pres
    ControlChange ident val -> encode ident <> encode val
    PatchChange patch       -> encode patch
    ChannelPressure pres    -> encode pres
    PitchWheelChange pitch  -> encode pitch
