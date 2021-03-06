module Track (
  Note,
  Tick,
  Part,
  Track
  ) where

  import Data.Binary (Word8)

  type Note = Word8
  type Tick = Int

  type Part = [[Note]]
  type Track = [(Tick, Part)]
