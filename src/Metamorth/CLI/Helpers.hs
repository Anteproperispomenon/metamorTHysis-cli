{-# LANGUAGE ApplicativeDo #-}

module Metamorth.CLI.Helpers
  ( inputOrthReader
  , outputOrthReader
  , dataReader
  ) where

import Data.Char (toLower)

import Data.Map.Strict qualified as M

import Data.Text qualified as T

import Metamorth.CLI.Types

import Options.Applicative

dataReader :: (Show inOrth, Show outOrth) => M.Map String inOrth -> M.Map String (outOrth, String) -> Parser (DataFromCLI inOrth outOrth)
dataReader inMap outMap = do
  inFile <- option str
     ( short 'i'
     <> long "input"
     <> long "infile"
     <> long "in"
     <> metavar "FILE"
     <> helpDoc Nothing -- TODO
     )
  outFile <- optional $ option str
     ( short 'o'
     <> long "output"
     <> long "outfile"
     <> long "out"
     <> metavar "FILE"
     <> helpDoc Nothing -- TODO
     )
  inOrth <- option (inputOrthReader inMap)
     ( short 'f'
     <> long "from"
     <> metavar "ORTHOGRAPHY"
     <> helpDoc Nothing -- TODO
     )
  (outOrth, extn) <- option (outputOrthReader outMap)
     ( short 't'
     <> long "to"
     <> metavar "ORTHOGRAPHY"
     <> helpDoc Nothing -- TODO
     )
  return $ DataFromCLI inFile outFile inOrth outOrth extn

inputOrthReader :: M.Map String inOrth -> ReadM inOrth
inputOrthReader mp = maybeReader $ \thisStr -> M.lookup (map toLower thisStr) mp

outputOrthReader :: M.Map String (outOrth, String) -> ReadM (outOrth, String)
outputOrthReader = inputOrthReader -- lol this works.

