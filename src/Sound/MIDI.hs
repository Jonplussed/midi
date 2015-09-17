module Sound.Midi
( Enc.Midi
, midi
, Enc.Track
, track
) where

import Control.Monad.Reader (ask)
import Control.Monad.State (modify)
import Control.Monad.Trans.Class (lift)
import Data.Monoid ((<>))

import qualified Data.ByteString.Lazy as LazyBS
import qualified Data.ByteString.Lazy.Builder as Bldr
import qualified Sound.Midi.Internal.Encoding as Enc
import qualified Sound.Midi.Internal.Encoding.Value as Val

midi :: Val.FileFormat -> Val.PPQN -> Enc.Midi -> LazyBS.ByteString
midi format ppqn tracks = Bldr.toLazyByteString $ Enc.buildFile format ppqn tracks

track :: Val.Channel -> Enc.Track -> Enc.Midi
track channel events = do
    ppqn <- ask
    let nextTrack = Enc.buildTrack ppqn channel events
    lift . modify $ \(Val.TrackCount count, tracks) ->
      (Val.TrackCount (succ count), tracks <> nextTrack)
