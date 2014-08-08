#!/usr/bin/env runhaskell
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative ((<*>), (<$>))
import Control.Error (runScript, scriptIO, throwT, tryRight)
import Data.ByteString.Lazy.Char8 (hGetContents, hPutStr)
import System.Environment (getArgs, getProgName)
import System.FilePath (FilePath, takeExtension, addExtension, replaceExtension)
import System.IO (IOMode(ReadMode, WriteMode), openFile, stdin, stdout)

import GCode (makeXYZ)

main :: IO ()
main = runScript $ do
  (myName, args) <- scriptTupleIO getProgName getArgs
  (hIn, hOut) <-
        case args of []  -> return (stdin, stdout)
                     [f] -> scriptTupleIO (openFile f ReadMode)
                                          (openFile (fixName f) WriteMode)
                     _   -> throwT $ "Usage: " ++ myName ++ " [inputfile]"
  dIn <- scriptIO $ hGetContents hIn
  res <- tryRight $ makeXYZ dIn
  scriptIO $ hPutStr hOut res

scriptTupleIO x y = scriptIO $ (,) <$> x <*> y

fixName :: FilePath -> FilePath
fixName nIn =
  (if takeExtension nIn == ".gcode" then replaceExtension else addExtension)
  nIn ".3w"
