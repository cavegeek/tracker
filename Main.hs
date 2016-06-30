import Track
import MidiTrack

import Data.Binary.Put
import qualified Data.ByteString.Lazy as B (putStr)

jacques0 = [(2,[[60],[62],[64],[60]])]
jacques1 = [(2,[[64],[65]]),(4,[[67]])]
jacques2 = [(1,[[67],[69],[67],[65]]),(2,[[64],[60]])]
jacques3 = [(2,[[60],[55]]),(4,[[60]])]

jacques
  = t0
  ++ (t0 `combine` t1)
  ++ (t0 `combine` t1 `combine` t2)
  ++ (t0 `combine` t1 `combine` t2 `combine` t3)
  ++ (t1 `combine` t2 `combine` t3)
  ++ (t2 `combine` t3)
  ++ t3
  where
    t0 = trackToMidi 0 0 8 (jacques0 ++ jacques0)
    t1 = trackToMidi 0 0 8 (jacques1 ++ jacques1)
    t2 = trackToMidi 0 0 8 (jacques2 ++ jacques2)
    t3 = trackToMidi 0 0 8 (jacques3 ++ jacques3)

drums0 = (1, [[36],[38],[36,38],[38]])

drums = trackToMidi 0 9 16 (take 14 $ repeat drums0)

main = B.putStr (runPut $ output (jacques `combine` drums))
