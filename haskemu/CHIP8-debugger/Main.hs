{-|
Module      : Main
Description : The CHIP-8 debugging utility
Copyright   : (c) Chad Reynolds, 2023
License     : MIT
-}
module Main
  ( main
  ) where

import           System.Environment (getArgs)
import           Data.ByteString    as BS (readFile, unpack)
import           Data.Word          (Word8)

import           Opcode             (getOpcode)

main :: IO ()
main = do
  args <- getArgs
  (commandStr, filePath) <- verifyArgs args
  let command = parseCommand commandStr
  programData <- BS.readFile filePath
  let opcodeData = pairOff (BS.unpack programData) 0x0
  print $ uncurry getOpcode $ head opcodeData
  runCommand command opcodeData

data Command = Disassemble
  deriving (Show)

verifyArgs :: [ String ] -> IO (String, String)
verifyArgs (x : y : _) = pure (x, y)
verifyArgs _           = error "invalid number of arguments, 2 expected"

parseCommand :: String -> Command
parseCommand "disassemble" = Disassemble
parseCommand x = error $ "invalid command " ++ x

-- | Pair every two elements together.  Uneven sequences pad with the given default value.
pairOff :: [ a ] -> a -> [ (a, a) ]
pairOff (x : y : xs) pad = (x, y) : pairOff xs pad
pairOff (x : xs) pad = (x, pad) : pairOff xs pad
pairOff [] _ = []

-- | Determines opcodes for instructions.
--  Its use is limited because it has no semantic understanding of programs.
--  Limited to usages on known program data.
runCommand :: Command -> [ (Word8, Word8) ] -> IO ()
runCommand Disassemble d = print $ map (uncurry getOpcode) d
