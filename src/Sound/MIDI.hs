module Sound.Midi where

import Control.Monad.Reader (ask)
import Control.Monad.State (modify)
import Control.Monad.Trans.Class (lift)
import Data.Monoid ((<>))
import Sound.Midi.Internal.Encoding.Event (buildFile, buildTrack)

import qualified Data.ByteString as StrictBS
import qualified Data.ByteString.Lazy as LazyBS
import qualified Data.ByteString.Lazy.Builder as Bld

import Sound.Midi.Events
import Sound.Midi.Internal.Types
import Sound.Midi.Values

midi :: FileFormat -> PPQN -> Midi () -> LazyBS.ByteString
midi format ppqn tracks = Bld.toLazyByteString $ buildFile format ppqn tracks

track :: Channel -> TrackM a -> Midi ()
track channel events = do
    ppqn <- ask
    let nextTrack = buildTrack ppqn channel events
    lift . modify $ \(TrackCount count, tracks) ->
      (TrackCount (succ count), tracks <> nextTrack)

note :: Note -> Float -> TrackM ()
note n beats = do
    noteOn n moderate beats
    noteOff n moderate 0
