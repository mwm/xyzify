#!/usr/bin/env runhaskell
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Prelude hiding (getContents, putStrLn)

import Control.Error (runScript, scriptIO, tryIO, throwT, tryRight)
import Control.Monad (liftM2)
import Data.ByteString.Lazy.Char8 (hGetContents, hPutStrLn)
import System.Environment (getArgs, getProgName)
import System.Exit (exitWith, ExitCode(ExitSuccess))
import System.FilePath (FilePath, takeExtension, addExtension, replaceExtension)
import System.IO (IOMode(ReadMode, WriteMode), openFile, hClose,
                  stdin, stdout, stderr)

import GCode (makeXYZ)

main :: IO ()
main = runScript $ do
  myName <- scriptIO getProgName
  as <- scriptIO getArgs
  (hIn, hOut) <-
        case as of []  -> return (stdin, stdout)
                   [f] -> do hI <- scriptIO $ openFile f ReadMode
                             hO <- scriptIO $ openFile (fixName f) WriteMode
                             return (hI, hO)
                   _   -> throwT $ "Usage: " ++ myName ++ " [inputfile]"
  dIn <- scriptIO $ hGetContents hIn
  res <- tryRight $ makeXYZ dIn
  scriptIO $ hPutStrLn hOut res

fixName :: FilePath -> FilePath
fixName nIn =
  (if takeExtension nIn == ".gcode" then replaceExtension else addExtension)
  nIn ".3w"
