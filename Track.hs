module Main (
  main
  ) where

  import qualified Data.ByteString.Lazy as B (putStr)
  import Data.Binary.Put
  import Data.Binary (Word8)
  import Control.Monad (mapM_)
  import qualified Data.ByteString.Char8 as C (pack)


  type Note = Word8
  type Tick = Int

  type Part = [[Note]]
  type Track = [(Tick, Part)]

  type Channel = Word8
  type Velocity = Word8
  type Program = Word8
  type Event = [Word8]
  type Signature = (Word8,Word8)
  type Qpm = Int

  word7 :: Word8 -> Word8
  word7 b = b `mod` 0x80

  nybble :: Word8 -> Word8
  nybble b = b `mod` 0x10

  message :: Word8 -> Channel -> Word8
  message m c = nybble m * 0x10 + nybble c

  startNote :: Channel -> Note -> Velocity -> Event
  startNote c n v = [message 0x9 c, word7 n, word7 v]
  stopNote :: Channel -> Note -> Velocity -> Event
  stopNote c n v = [message 0x8 c, word7 n, word7 v]
  program :: Channel -> Program -> Event
  program c p = [message 0xC c, word7 p]
  signature :: Signature -> Event
  signature (n,dl) = [0xFF, 0x58, 4, n, dl, 0x24, 0x04]
  tempo :: Qpm -> Event
  tempo t = [0xFF, 0x51, 3] ++ map fromIntegral [b0, b1, b2]
    where
      usecPerMin = 60000000
      usecPerQNote = usecPerMin `div` t
      (b0,rest) = usecPerQNote `divMod` 0x10000
      (b1,b2) = rest `divMod` 0x100
  end :: Event
  end = [0xFF, 0x2F, 0]

  type Delay = Int

  delay :: Delay -> [Word8]
  delay n
    | n < 0 = [0]
    | otherwise = f ((abs n) `mod` (2^28)) 0
    where
      f n k
        | n < 0x40 = [fromIntegral (n + k)]
        | otherwise =
            f (n `div` 0x40) 0x40 ++ [fromIntegral (n `mod` 0x40)]

  type MidiTrack = [(Delay, Event)]

  delayBy :: Delay -> MidiTrack -> MidiTrack
  delayBy _ [] = []
  delayBy db ((d,e):rest) = (d+db,e):rest

  partToMidi :: Channel -> Delay -> Part -> MidiTrack
  partToMidi c d p
    = foldr (step d (\n -> stopNote c n 0)) [] p
      `combine`
      delayBy (negate d) (foldr (step d (\n -> startNote c n 64)) [] p)
    where
      step d _ [] rest = delayBy d rest
      step d f ns rest = zip (d:[0..]) (map f ns) ++ rest

  trackToMidi :: Program -> Channel -> Delay -> Track -> MidiTrack
  trackToMidi p c d t
    = (0,program c p) : concatMap (\(t,p) -> partToMidi c (t*d) p) t

  combine :: MidiTrack -> MidiTrack -> MidiTrack
  combine [] m = m
  combine m [] = m
  combine m0@((d0, e0):r0) m1@((d1, e1):r1)
    | d0 <= d1 = (d0, e0) : combine r0 (delayBy (negate d0) m1)
    | otherwise = (d1, e1) : combine (delayBy (negate d1) m0) r1

  output :: MidiTrack -> Put
  output m
    = do
      putMidiHeader
      putMidiTrack m
    where
      putMidiHeader
        = do
          putByteString (C.pack "MThd")
          putWord32be 6 -- length
          putWord16be 0 -- format
          putWord16be 1 -- track count
          putWord16be 16 -- ticks per quarter note (0 to (2^16) - 1)
      putMidiTrack m
        = do
          putByteString (C.pack "MTrk")
          putWord32be (fromIntegral $ length encoded)
          mapM_ putWord8 encoded
        where
          encoded = concatMap (\(d,e) -> delay d ++ e) fullTrack
          fullTrack = trackStart ++ m ++ trackEnd
          trackStart = [(0, signature (4,2)), (0, tempo 120)]
          trackEnd = [(0, end)]

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

  main = B.putStr (runPut $ output jacques)
