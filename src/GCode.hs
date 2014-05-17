module GCode (makeXYZ) where
    
makeXYZ :: String -> String
makeXYZ = unlines . fixHalves . break (== "; --- END SECTION ---") . lines

fixHalves :: ([String], [String]) -> [String]
fixHalves (prefix, codes) = fixPrefix prefix ++ tail codes

fixPrefix :: [String] -> [String]
fixPrefix = swapParts . break (== "; --- MOVE THIS SECTION TO THE TOP AND DELETE THIS LINE ---")

swapParts :: ([String], [String]) -> [String]
swapParts (header, xyzStuff) = tail xyzStuff ++ filter isCommand header

isCommand :: String -> Bool
isCommand line = length line /= 0 && head line /= ';'