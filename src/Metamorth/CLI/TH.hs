{-# LANGUAGE TemplateHaskell #-}

module Metamorth.CLI.TH (createMain) where

import Data.ByteString      qualified as BS
import Data.ByteString.Lazy qualified as BL

import Data.Char (toLower)

import Data.Map.Strict qualified as M

import Data.Text          qualified as T
import Data.Text.Encoding qualified as TE

import Data.Text.Lazy          qualified as TL
import Data.Text.Lazy.Encoding qualified as TLE

import Metamorth.CLI.Helpers
import Metamorth.CLI.Types

import Options.Applicative

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import System.FilePath


createMain :: Q [Dec]
createMain = do
  -- Okay...
  inMapName    <- maybeLookupValue  "inputOrthNameMap"  inputErr
  outMapName   <- maybeLookupValue "outputOrthNameMap" outputErr
  inOrthType   <- maybeLookupType  "InOrth"  inOrthErr
  outOrthType  <- maybeLookupType "OutOrth" outOrthErr

  mainFuncName <- maybeLookupValue "convertOrthographyBS" mainFuncErr

  -- hmm...
  parserExp <- [| dataReader $(pure $ VarE inMapName) $(pure $ VarE outMapName) |]

  parserStuff <- [| info ( $(pure parserExp) <**> helper) mempty |]
  
  -- mainRunner <- newName "mainRunner"

  runParserDefn <- 
    [d| mainRunner :: DataFromCLI -> IO ()
        mainRunner (DataFromCLI inFP outFP' inOrth outOrth fpExt) = do
          outFP <- case outFP' of
            (Just x) -> do
              return x
            Nothing -> do
              return $ addSubExtension inFP fpExt
          inData <- TE.decodeUtf8 <$> BS.readFile inFP
          let outDataE = $(pure $ VarE mainFuncName) inOrth outOrth inData
          case outDataE of
            (Left err) -> putStrLn $ "Error: " ++ err
            (Right ot) -> BL.writeFile outFP ot
    |]

  -- mainType <- [t| IO () |]
  mainExpr <- [d| main :: IO ()
                  main = (execParser $(pure parserStuff)) >>= mainRunner |]

  return (runParserDefn ++ mainExpr)

maybeLookupValue :: String -> String -> Q Name
maybeLookupValue valName err = do
  mNom <- lookupValueName valName
  case mNom of
    Nothing -> do
      reportError err
      return $ mkName valName -- standin.
    (Just x) -> return x

maybeLookupType :: String -> String -> Q Name
maybeLookupType typName err = do
  mNom <- lookupTypeName typName
  case mNom of
    Nothing -> do
      reportError err
      return $ mkName typName -- standin.
    (Just x) -> return x


inputErr :: String
inputErr = unlines
  [ "Can't find the input orthography map; make sure"
  , "you add \"inputOrthNameMap\" to the export list"
  , "of the orthography file."
  ]

outputErr :: String
outputErr = unlines
  [ "Can't find the output orthography map; make sure"
  , "you add \"outputOrthNameMap\" to the export list"
  , "of the orthography file."
  ]

inOrthErr :: String
inOrthErr = unlines
  [ "Can't find the input Orthography type; make sure"
  , "you add \"InOrth(..)\" to the export list of the"
  , "orthography file."
  ]

outOrthErr :: String
outOrthErr = unlines
  [ "Can't find the output Orthography type; make sure"
  , "you add \"OutOrth(..)\" to the export list of the"
  , "orthography file."
  ]

mainFuncErr :: String
mainFuncErr = unlines
  [ "Can't find the main function; make sure you add"
  , "\"convertOrthographyBS\" to the export list of the"
  , "orthography file."
  ]

addSubExtension :: FilePath -> String -> FilePath
addSubExtension fp newExt
  | (fp', exts) <- splitExtensions fp
  = fp' <.> newExt <.> exts
