module Metamorth.CLI.Types 
  ( DataFromCLI(..)

  ) where

-- | Basic type for information parsed by the CLI.
data DataFromCLI inO outO = DataFromCLI
  { cliInputFile  :: FilePath
  , cliOutputFile :: Maybe FilePath
  , cliInputOrth  :: inO
  , cliOutputOrth :: outO
  , cliDefaultExt :: String
  , cliOverwrite  :: Bool
  } deriving (Show, Eq)




