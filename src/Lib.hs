module Lib
  ( someFunc
  , newEmulator
  , execRawOpcode
  ) where

import Data.Word
import Data.Bits

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data Instruction = CLR
                 | Jump
                 | None

data Emulator = Emulator { memory :: [Word8]
                         , registers :: [Word16]
                         , pc :: Word16
                         , i :: Word16
                         , stack :: [Word16]
                         , videoMemory :: [Word16]
                         }

instance Show Emulator where
  show emulator = show $ videoMemory emulator

newEmulator :: Emulator
newEmulator =
  Emulator { memory = [0x000 | _ <- [0..4096]]
           , registers = [0x000 | _ <- [1..16]]
           , pc = 0x000
           , i = 0x000
           , stack = [0x000 | _ <- [1..16]]
           , videoMemory = [1 | _ <- [1..10]]
           }

execRawOpcode :: Emulator -> Word16 -> Emulator
execRawOpcode emulator opcode =
  execIntruction emulator $ decodeOpcode opcode

execIntruction :: Emulator -> Instruction -> Emulator
execIntruction emulator instruction =
  case instruction of
    CLR -> clearScreen emulator
    Jump -> emulator
    None -> emulator

decodeOpcode :: Word16 -> Instruction
decodeOpcode opcode
  | opcode == 0x00E0 = CLR
  | opcode .&. 0xF000 == 0x1000 = Jump
  | otherwise  = None

clearScreen :: Emulator -> Emulator
clearScreen emulator =
  Emulator { memory = memory emulator
           , registers = registers emulator
           , pc = pc emulator
           , i = i emulator
           , stack = stack emulator
           , videoMemory = [0x000 | _ <- [1..2048]]
           }
