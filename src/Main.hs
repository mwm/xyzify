module Main where

import Prelude hiding (getContents, putStrLn)
import Data.ByteString.Lazy.Char8 (getContents, putStrLn)
import System.IO (hPutStrLn, stderr)

import GCode

main :: IO ()
main = getContents >>= either (hPutStrLn stderr) putStrLn . makeXYZ