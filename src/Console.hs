-- | Main entry point to the application.
module Console where

import Prelude hiding (getContents, putStr)
import Data.ByteString.Lazy.Char8 (getContents, putStr)
import GCode

-- | The main entry point.
main :: IO ()
main = fmap makeXYZ getContents >>= putStr