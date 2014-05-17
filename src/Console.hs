-- | Main entry point to the application.
module Console where

import System.IO

import GCode

-- | The main entry point.
main :: IO ()
main = putStr =<< fmap makeXYZ . hGetContents =<< openFile "test.gcode" ReadMode