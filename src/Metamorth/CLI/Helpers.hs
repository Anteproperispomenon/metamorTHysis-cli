{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Metamorth.CLI.Helpers
  ( inputOrthReader
  , outputOrthReader
  , dataReader
  , makeOrthOptions -- Debug
  ) where

import Data.Set qualified as S

import Data.Char (toLower)

import Data.Map.Strict qualified as M

import Data.Text qualified as T

import Metamorth.CLI.Types

import Options.Applicative hiding (Doc)

import Prettyprinter
import Prettyprinter.Render.Terminal

dataReader :: (Show inOrth, Show outOrth, Ord inOrth, Ord outOrth) => M.Map String inOrth -> M.Map String (outOrth, String) -> Parser (DataFromCLI inOrth outOrth)
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
     <> helpDoc (Just (makeOrthOptions "Input" inFlip))
     )
  ovr <- switch
     ( short 'w'
     <> long "ovr"
     <> long "overwrite"
     <> helpDoc (Just "Overwrite the output file if it already exists.")
     )
  (outOrth, extn) <- option (outputOrthReader outMap)
     ( short 't'
     <> long "to"
     <> metavar "ORTHOGRAPHY"
     <> helpDoc (Just (makeOrthOptions "Output" outFlip))
     )
  return $ DataFromCLI inFile outFile inOrth outOrth extn ovr
  where
   inFlip  = invertOrthMap inMap
   outFlip = invertOrthMap (fmap fst outMap)

inputOrthReader :: M.Map String inOrth -> ReadM inOrth
inputOrthReader mp = maybeReader $ \thisStr -> M.lookup (map toLower thisStr) mp

outputOrthReader :: M.Map String (outOrth, String) -> ReadM (outOrth, String)
outputOrthReader = inputOrthReader -- lol this works.

-- | Invert a map.
invertOrthMap :: (Ord inOrth) => M.Map String inOrth -> M.Map inOrth (S.Set String)
invertOrthMap = M.foldlWithKey (\mp' k val -> insertWithElse S.insert S.singleton val k mp') M.empty
-- invertOrthMap inMap = M.foldlWithKey (\mp' k val -> insertWithElse S.insert S.singleton val k mp') M.empty inMap

-- | Like `insertWith`, but the type of the value to be
--   inserted doesn't have to be the same as value already
--   inserted. 
insertWithElse :: (Ord k) => (w -> v -> v) -> (w -> v) -> k -> w -> M.Map k v -> M.Map k v
insertWithElse op f k val
  = M.alter (\case {Nothing -> Just $ f val ; (Just y) -> Just $ op val y}) k


------------------------------------------------
-- Pretty-Printing some Documentation

makeOrthOptions :: forall orth. (Ord orth, Show orth) => String -> M.Map orth (S.Set String) -> Doc AnsiStyle
makeOrthOptions str mp
  = (pretty str) <+> "Orthography Options:" <> line <> (indent 2 $ vsep $ map (uncurry makeOption) $ M.assocs mp)
      
  where
    annSeln :: Color -> Doc AnsiStyle -> Doc AnsiStyle
    annSeln col = annotate (color col) 
    makeOption :: (Ord orth, Show orth) => orth -> S.Set String -> Doc AnsiStyle
    makeOption ort st
      = (annotate ((color Green) <> bold) (viaShowX ort)) <+> (annotate (color Yellow) ":") <+> 
          (hsep $ punctuate "," $ map (annSeln Red . pretty) $ S.toList st)

-- Removing "In/Out" before the orthography name.

viaShowX :: (Show a) => a -> Doc ann
viaShowX = pretty . T.pack . showX

showX :: (Show a) => a -> String
showX x = case (show x) of
   ('I':'n':rst)     -> rst
   ('O':'u':'t':rst) -> rst
   rst               -> rst






