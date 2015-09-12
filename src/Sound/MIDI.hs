module Sound.Midi where

import Data.Binary.Put (Put)
import Data.Monoid ((<>))

import qualified Data.ByteString as StrictBS
import qualified Data.ByteString.Lazy as LazyBS
import qualified Data.ByteString.Lazy.Builder as Bld

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Class

import Sound.Midi.Internal.Encoding.Event
import Sound.Midi.Internal.Encoding.Value
import Sound.Midi.Internal.Types

track :: Channel -> TrackM a -> Midi ()
track channel events = do
    ppqn <- ask
    let nextTrack = buildTrack ppqn channel events
    lift . modify $ \(TrackCount count, tracks) ->
      (TrackCount (succ count), tracks <> nextTrack)

midi :: FileFormat -> PPQN -> Midi () -> LazyBS.ByteString
midi format ppqn tracks = Bld.toLazyByteString $ buildFile format ppqn tracks
