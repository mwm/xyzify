#!/usr/bin/env runhaskell
{-# LANGUAGE OverloadedStrings #-}

module Web where

import Prelude hiding (span)

import Control.Error (runEitherT, scriptIO, tryRight)
import Data.ByteString.Lazy.Char8 (hGetContents, append)
import Data.ByteString.Char8 (pack)
import MFlow.Forms.Internals (getToken)
import MFlow.Wai.Blaze.Html.All hiding (id)
import System.IO (IOMode(ReadMode), openFile)

import GCode (makeXYZ)
import Main (fixName)


main :: IO ()
main = runNavigation "" . step . page $ do
  setFilesPath "./"
  file <- h1 "XYZifier"
          ++> script "" ! src "dropzone.js"
          ++> p << a "Need help?" ! href "help.html"
          ++> fileUpload <** br ++> submitButton "Convert"
  process file

process :: (FilePath, String, FilePath) -> View Html IO ()
process (name, _, input) = do
  res <- runEitherT $ do
    dIn <- scriptIO $ openFile input ReadMode >>= hGetContents
    res <- tryRight $ makeXYZ dIn
    let out = fixName name
    tok <- getToken
    scriptIO . sendFlush tok . HttpData [("Content-Type", "text/plain"),
                                         ("Cache-Control", "max-age=360000"),
                                         ("Content-Disposition",
                                          pack $ "attachment; filename=" ++ out)]
                                        [] $ res `append` "\n"
    return $ out
  either (format toHtml $ b "Conversion failed: ") (format code "Downloading ") res

format :: (Html -> Html) -> Html -> String -> View Html IO ()
format tag pre msg = p (toHtml pre >> tag << toHtml msg) ++> noWidget

rawSend :: (FormInput v,MonadIO m) => HttpData -> View v m ()
rawSend dat =  do
    tok <- getToken
    liftIO $ sendFlush tok dat
