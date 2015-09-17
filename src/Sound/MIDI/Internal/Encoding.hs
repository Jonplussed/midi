{-# LANGUAGE DeriveFunctor, OverloadedStrings #-}

module Sound.Midi.Internal.Encoding
( ChunkM (..)
, Midi
, Track
, buildTrack
, buildFile
) where

import Control.Monad.Free (Free (..))
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.State (State, runState)
import Data.Monoid ((<>))

import qualified Data.ByteString.Lazy as LazyBS
import qualified Data.ByteString.Lazy.Builder as Bld

import Sound.Midi.Internal.Encoding.Event
import Sound.Midi.Internal.Encoding.Value

data ChunkM next
  = VoiceChunk Float VoiceChunk next
  | MetaChunk Float MetaChunk next
  deriving (Show, Functor)

type MidiM = ReaderT PPQN (State (TrackCount, Bld.Builder))
type Midi = MidiM ()

type TrackM = Free ChunkM
type Track = TrackM ()

-- NOTE: musicians write music as "[event] for [time]", whereas MIDI is
-- written as "wait for [time], then perform [event]". We merge the two by
-- kicking each action's deltaTime up to the next event

buildTrack :: PPQN -> Channel -> Track -> Bld.Builder
buildTrack ppqn chan track =
    trackBegin (interp track) <>
    trackEnd
  where
    interp :: Track -> Bld.Builder
    interp (Free (VoiceChunk beats chunk next)) =
      encode (fromBeats ppqn $ Beats beats) <>
      encodeVoiceChunk chan chunk <>
      interp next
    interp (Free (MetaChunk beats chunk next)) =
      encode (fromBeats ppqn $ Beats beats) <>
      encodeMetaChunk chunk <>
      interp next
    interp (Pure _) = mempty

buildFile :: FileFormat -> PPQN -> Midi -> Bld.Builder
buildFile format ppqn tracks =
    fileBegin format ppqn trackCount <>
    trackStr
  where
    (_, (trackCount, trackStr)) = runState (runReaderT tracks ppqn) initState

-- private functions

fromBeats :: PPQN -> Beats -> DeltaTime
fromBeats (PPQN ppqn) (Beats beats) =
    DeltaTime . round $ fromIntegral ppqn * beats

fileBegin :: FileFormat -> PPQN -> TrackCount -> Bld.Builder
fileBegin format ppqn trackCount =
    Bld.byteString "MThd" <>
    Bld.word32BE 0x06 <>
    encode format <>
    encode trackCount <>
    encode ppqn

trackBegin :: Bld.Builder -> Bld.Builder
trackBegin track =
    Bld.byteString "MTrk" <>
    Bld.word32BE trackLength <>
    track
  where
    trackLength = fromIntegral . LazyBS.length $ Bld.toLazyByteString track

-- this is technically a MIDI meta-event, but encoding it this way
-- rather than as a useful event ensures its proper usage
trackEnd :: Bld.Builder
trackEnd =
  Bld.word8 0xFF <>
  Bld.word8 0x2F <>
  Bld.word8 0x00

initState :: (TrackCount, Bld.Builder)
initState = (TrackCount 0, mempty)
