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

import           Opcode             (Opcode, getOpcode)

main :: IO ()
main = do
  args <- getArgs
  (commandStr, filePath) <- verifyArgs args
  let command = parseCommand commandStr
  programData <- BS.readFile filePath
  runCommand command $ BS.unpack programData

data Command = Disassemble
  deriving (Show)

verifyArgs :: [ String ] -> IO (String, String)
verifyArgs (x : y : _) = pure (x, y)
verifyArgs _           = error "invalid number of arguments, 2 expected"

parseCommand :: String -> Command
parseCommand "disassemble" = Disassemble
parseCommand x = error $ "invalid command " ++ x

runCommand :: Command -> [ Word8 ] -> IO ()
runCommand Disassemble d = print $ disassemble d

disassemble :: [ Word8 ] -> [ Opcode ]
disassemble (x : y : xs) = getOpcode x y : disassemble xs
disassemble [] = []
disassemble _ = error "Uneven number of bytes in program"
