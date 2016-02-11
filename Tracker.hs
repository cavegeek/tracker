module Main (
  main
  ) where

  import qualified Data.ByteString.Lazy as B (putStr, length)
  import Data.Binary.Put
  import Data.Binary (Word8, Word32)
  import Control.Monad (mapM_)
  import qualified Data.ByteString.Char8 as C (pack)

  data MidiMessage
    = NoteStart Key Velocity
    | NoteEnd Key Velocity
    | Program Patch
    | Pitch Word8
    | Pan Word8
    | Volume Word8

  type MidiTrack = [(Delta, Channel, MidiMessage)]

  type MidiFile = [MidiTrack]

  type Key = Word8 -- 0 to 127 (60 = middle c)
  type Velocity = Word8 -- 0 to 127
  type Delta = Integer -- ticks
  type Channel = Word8 -- 0 to 15
  type Patch = Word8 -- 0 to 127

  type Nat = Integer -- 0 ..

  variableLength :: Nat -> Put
  variableLength n = f ((abs n) `mod` (2^28)) 0
    where
      f n k
        | n < 128 = putWord8 (fromInteger (n + k))
        | otherwise =
            f (n `div` 128) 128 >> putWord8 (fromInteger (n `mod` 128))

  channelMessage :: Word8 -> Channel -> [Word8] -> Put
  channelMessage message c args = do
    putWord8 (message*0x10 + (c `mod` 0x10))
    mapM_ (putWord8 . (`mod` 128)) args

  encodeMessage :: (Delta, Channel, MidiMessage) -> Put
  encodeMessage (d,c,m) = variableLength d >> case m of
    (NoteStart k v) -> channelMessage 0x9 c [k, v] 
    (NoteEnd k v) -> channelMessage 0x8 c [k, v]
    (Program p) -> channelMessage 0xC c [p]
    (Pitch n) -> channelMessage 0xE c [n]
    (Pan n) -> channelMessage 0xB c [0xA, n]
    (Volume n) -> channelMessage 0xB c [0x7, n]

  encodeTrack :: MidiTrack -> Put
  encodeTrack = mapM_ encodeMessage

  --

  type Note = Key

  type Track = [(Nat,Velocity,[Note])]

  toMidiTrack :: Nat -> Channel -> Track -> MidiTrack
  toMidiTrack ticks channel = concatMap toMidiNotes
    where
      toMidiNotes :: (Nat,Velocity,[Note]) -> MidiTrack
      toMidiNotes (mul, _, [])
        = [(0, channel, NoteStart 0 0), (ticks*mul, channel, NoteEnd 0 0)]
      toMidiNotes (mul, vel, notes@(n:ns))
        = map (\n -> (0, channel, NoteStart n vel)) notes
          ++ (mul*ticks, channel, NoteEnd n vel)
            : map (\n -> (0, channel, NoteEnd n vel)) ns 

  putBPM :: Word32 -> Put
  putBPM bpm = do
      putWord8 (fromIntegral (usecPerQNote `div` 0x10000))
      putWord16be (fromIntegral (usecPerQNote `mod` 0x10000))
    where usecPerQNote = 60000000 `div` bpm

  simpleMidiFile :: Patch -> Track -> Put
  simpleMidiFile patch track
    = do
      putByteString (C.pack "MThd")
      putWord32be 6 -- length
      putWord16be 0 -- format
      putWord16be 1 -- track count
      putWord16be 16 -- ticks per quarter note (0 to (2^16) - 1)
      putByteString (C.pack "MTrk")
      putWord32be (fromIntegral (B.length midiTrack) + 15 + 4)
      putWord8 0 -- delta
      putWord16be 0xFF58 -- system time signature
      putWord8 4 -- byte count
      putWord32be 0x04022404
      putWord8 0 -- delta
      putWord16be 0xFF51 -- system tempo
      putWord8 3 -- byte count
      putBPM 120
      putLazyByteString midiTrack
      putWord8 0 -- delta
      putWord16be 0xFF2F --system end track
      putWord8 0 -- byte count
    where
      midiTrack
        = runPut (encodeTrack ((0,0,Program patch) : toMidiTrack 16 0 track))

  jacques :: Track
  jacques = map ((,,) 1 64) $ map (:[]) [60,62,64,60,60,62,64,60]

  chord :: Track
  chord
    = [(2,64,[60,64,67]),(2,64,[64,67,71]),(1,127,[67,71,74]),(1,64,[67,71,74])]

  main = B.putStr (runPut $ simpleMidiFile 49 chord)
