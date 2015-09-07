
module Sound.Midi.Header where

import Data.Binary.Put
import Data.Bits ((.&.), setBit, shiftR)
import Data.Word

import Sound.Midi.Internal.Types

-- fileHeader :: FileFormat -> [Track] -> Put
-- fileHeader (FileFormat fileFormat) tracks = do
--     putByteString "MThd"
--     putWord32be   6
--     putWord16be   fileFormat
--     putWord16be   (length tracks)
--     putWord16be   pulsesPerQuarterNote
--   where
--     chunkSize = 6
--     pulsesPerQuarterNote = 480
