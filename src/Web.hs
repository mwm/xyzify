#!/usr/bin/env runhaskell
{-# LANGUAGE OverloadedStrings #-}

module Web where

import Prelude hiding (span)

import Control.Error (runEitherT, scriptIO, tryRight)
import Data.ByteString.Lazy.Char8 (hGetContents)
import MFlow.Wai.Blaze.Html.All hiding (id)
import System.IO (IOMode(ReadMode), openFile)

import GCode (makeXYZ)
import Main (fixName)


main :: IO ()
main = runNavigation "" . step . page $ do
  setFilesPath "./"
  file <- h1 "XYZifier"
          ++> p << a "Need help?" ! href "help.html"
          ++> fileUpload <** br ++> submitButton "Convert"
  process file

process :: (FilePath, String, FilePath) -> View Html IO ()
process (name, _, input) = do
  res <- runEitherT $ do
    dIn <- scriptIO $ openFile input ReadMode >>= hGetContents
    res <- tryRight $ makeXYZ dIn
    let out = fixName name
    -- Code to download ByteString res to file name goes here!
    return $ out
  either (format toHtml $ b "Conversion failed: ") (format code "Downloading ") res

format :: (Html -> Html) -> Html -> String -> View Html IO ()
format tag pre msg = p (toHtml pre >> tag << toHtml msg) ++> noWidget
