#!/usr/bin/env runhaskell
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative ((<*>), (<$>))
import Control.Error (runScript, scriptIO, throwT, tryRight)
import Data.ByteString.Lazy.Char8 (hGetContents, hPutStr)
import System.Environment (getArgs, getProgName)
import System.IO (IOMode(ReadMode, WriteMode), openFile, stdin, stdout, hClose)

import GCode (makeXYZ)
import Util (fixName)

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
  scriptIO $ hClose hOut

scriptTupleIO x y = scriptIO $ (,) <$> x <*> y
