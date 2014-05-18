{-# LANGUAGE OverloadedStrings #-}

module Test where

import Prelude hiding (putStrLn, readFile)
import Data.ByteString.Lazy.Char8 (readFile, pack, putStrLn, ByteString)
import Data.ByteString.Base64.Lazy (decode)
import Data.Either.Combinators (fromRight)
import System.IO (hPutStrLn, stderr)

import GCode

main :: IO ()
main = readFile "test.gcode" >>= either (hPutStrLn stderr) (either print putStrLn . decode) . makeXYZ
