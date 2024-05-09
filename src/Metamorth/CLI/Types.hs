module Metamorth.CLI.Types 
  ( DataFromCLI(..)

  ) where

data DataFromCLI inO outO = DataFromCLI
  { cliInputFile  :: FilePath
  , cliOutputFile :: Maybe FilePath
  , cliInputOrth  :: inO
  , cliOutputOrth :: outO
  , cliDefaultExt :: String
  } deriving (Show, Eq)

