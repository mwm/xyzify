{-# LANGUAGE OverloadedStrings #-}

module GCode (makeXYZ) where

import Prelude hiding (lines, unlines, length, head)
import Data.ByteString.Lazy.Char8 (ByteString, lines, unlines, length, head)
import Data.ByteString.Base64.Lazy (encode)
    
makeXYZ :: ByteString -> ByteString
makeXYZ = encode . unlines . fixHalves . break (== "; --- END SECTION ---") . lines

fixHalves :: ([ByteString], [ByteString]) -> [ByteString]
fixHalves (prefix, codes) = fixPrefix prefix ++ tail codes

fixPrefix :: [ByteString] -> [ByteString]
fixPrefix = swapParts . break (== "; --- MOVE THIS SECTION TO THE TOP AND DELETE THIS LINE ---")

swapParts :: ([ByteString], [ByteString]) -> [ByteString]
swapParts (header, xyzStuff) = tail xyzStuff ++ filter isCommand header

isCommand :: ByteString -> Bool
isCommand line = length line /= 0 && head line /= ';'