module Util where

import System.FilePath (FilePath, takeExtension, addExtension, replaceExtension)

fixName :: FilePath -> FilePath
fixName nIn =
  (if takeExtension nIn == ".gcode" then replaceExtension else addExtension)
  nIn ".3w"
