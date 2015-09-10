{-# LANGUAGE OverloadedStrings #-}

module Sound.Midi.Internal.Encoding.Event where

import Data.Binary.Put (Put, putByteString, putWord8, putWord32be, runPut)
import Data.Bits ((.|.))
import Control.Monad.Free (Free (..))
import Data.Word (Word8)
import Sound.Midi.Internal.Encoding.Value
import Sound.Midi.Internal.Types

import qualified Data.ByteString as StrictBS
import qualified Data.ByteString.Lazy as LazyBS

putTrack :: Channel -> TrackM a -> Put
putTrack chan track = do
    putTrackBegin $ interp track (DeltaTime 0)
    putTrackEnd
  where
    interp (Free (VoiceChunk nextDeltaTime chunk next)) deltaTime = do
      encode deltaTime
      putWord8 $ voiceChunkIdent chan chunk
      encode chunk
      interp next nextDeltaTime
    interp (Free (MetaChunk nextDeltaTime chunk next)) deltaTime = do
      encode deltaTime
      putWord8 0xFF
      putWord8 $ metaChunkIdent chunk
      putWord8 $ metaArgSize chunk
      encode chunk
      interp next nextDeltaTime
    interp (Free (Rest nextDeltaTime next)) deltaTime = do
      encode deltaTime
      interp next nextDeltaTime

putTrackBegin :: Put -> Put
putTrackBegin put = do
    putByteString "MTrk"
    putWord32be . fromIntegral $ StrictBS.length str
    putByteString str
  where
    str = LazyBS.toStrict $ runPut put

-- this is technically a MIDI meta-event, but encoding it this way
-- rather than as a useful event ensures its proper usage
putTrackEnd :: Put
putTrackEnd = do
    putWord8 0xFF
    putWord8 0x2F
    putWord8 0x00

instance Encodable VoiceChunk where
  encode (NoteOff note vel) = encode note >> encode vel
  encode (NoteOn note vel) = encode note >> encode vel
  encode (AfterTouch note pres) = encode note >> encode pres
  encode (ControlChange ident val) = encode ident >> encode val
  encode (PatchChange patch) = encode patch
  encode (ChannelPressure pres) = encode pres
  encode (PitchWheelChange pitch) = encode pitch

instance Encodable MetaChunk where
  encode (SequenceNumber seq) = encode seq
  encode (TextArbitrary text) = putByteString text
  encode (TextCopyright text) = putByteString text
  encode (TextTrackName text) = putByteString text
  encode (TextInstruName text) = putByteString text
  encode (TextLyric text) = putByteString text
  encode (TextMarker text) = putByteString text
  encode (TextCuePoint text) = putByteString text
  encode (SetTempo tempo) = undefined
  encode (SetTimeSig) = undefined
  encode (SetKeySig sig) = encode sig

-- private functions

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
