{-# LANGUAGE OverloadedStrings #-}

module Sound.Midi.Internal.Encoding.Event
( buildTrack
, buildFile
) where

import Control.Monad.Free (Free (..))
import Control.Monad.Reader (runReaderT)
import Control.Monad.State (runState)
import Control.Monad.Trans.Class (lift)
import Data.Bits ((.|.))
import Data.Monoid ((<>))
import Data.Word (Word8)
import Sound.Midi.Internal.Ops (fromBeats)

import qualified Data.ByteString as StrictBS
import qualified Data.ByteString.Lazy as LazyBS
import qualified Data.ByteString.Lazy.Builder as Bld
import qualified Data.Binary.Put as Put

import Sound.Midi.Internal.Encoding.Value
import Sound.Midi.Internal.Types

-- NOTE: musicians write music as "[event] for [time]", whereas MIDI is
-- written as "wait for [time], then perform [event]". We merge the two by
-- kicking each action's deltaTime up to the next event

buildTrack :: PPQN -> Channel -> TrackM a -> Bld.Builder
buildTrack ppqn chan track =
    trackBegin (Put.runPut . interp track $ Beats 0) <>
    trackEnd
  where
    interp (Free (VoiceChunk nextBeats chunk next)) beats = do
      encode $ fromBeats ppqn beats
      Put.putWord8 $ voiceChunkIdent chan chunk
      encode chunk
      interp next nextBeats
    interp (Free (MetaChunk nextBeats chunk next)) beats = do
      encode $ fromBeats ppqn beats
      Put.putWord8 0xFF
      Put.putWord8 $ metaChunkIdent chunk
      Put.putWord8 $ metaArgSize chunk
      encode chunk
      interp next nextBeats
    interp (Free (Rest nextBeats next)) beats = do
      encode $ fromBeats ppqn beats
      interp next nextBeats

buildFile :: FileFormat -> PPQN -> Midi () -> Bld.Builder
buildFile format ppqn tracks =
    fileBegin format ppqn trackCount <>
    trackStr
  where
    initState = (TrackCount 0, mempty)
    (_, (trackCount, trackStr)) = runState (runReaderT tracks ppqn) initState

-- private functions

fileBegin :: FileFormat -> PPQN -> TrackCount -> Bld.Builder
fileBegin (FileFormat format) (PPQN ppqn) (TrackCount trackCount) =
    Bld.byteString "MThd" <>
    Bld.word32BE 0x06 <>
    Bld.word16BE format <>
    Bld.word16BE trackCount <>
    Bld.word16BE ppqn

trackBegin :: LazyBS.ByteString -> Bld.Builder
trackBegin lazyBS =
    Bld.byteString "MTrk" <>
    Bld.word32BE (fromIntegral $ LazyBS.length lazyBS) <>
    Bld.lazyByteString lazyBS

-- this is technically a MIDI meta-event, but encoding it this way
-- rather than as a useful event ensures its proper usage
trackEnd :: Bld.Builder
trackEnd =
  Bld.word8 0xFF <>
  Bld.word8 0x2F <>
  Bld.word8 0x00

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
  encode (TextArbitrary text) = Put.putByteString text
  encode (TextCopyright text) = Put.putByteString text
  encode (TextTrackName text) = Put.putByteString text
  encode (TextInstruName text) = Put.putByteString text
  encode (TextLyric text) = Put.putByteString text
  encode (TextMarker text) = Put.putByteString text
  encode (TextCuePoint text) = Put.putByteString text
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
