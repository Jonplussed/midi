{-# LANGUAGE OverloadedStrings #-}

module Sound.Midi.Header where

import qualified Data.ByteString.Lazy as LazyBS
import qualified Data.ByteString as BS
import qualified Sound.Midi.Values.VoiceEvent as VE

import Data.Binary.Put
import Sound.Midi.Internal.Types

-- fileHeader :: FileFormat -> [Track] -> Put
-- fileHeader (FileFormat fileFormat) tracks = do
--     putByteString "MThd"
--     putWord32be   6
--     putWord16be   fileFormat
--     putWord16be   (length tracks)
--     putWord16be   pulsesPerQuarterNote
--   where

