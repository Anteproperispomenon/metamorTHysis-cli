{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE DerivingVia #-}

module Metamorth.CLI.Colour
  ( onBackgroundColour
  , queryColours
  , getBkdColour
  , colour
  , ctxColour
  , RGB16(..)
  -- * Patterns

  -- ** Vivid Colours
  , pattern BlackH   
  , pattern RedH     
  , pattern GreenH   
  , pattern BlueH    
  , pattern YellowH  
  , pattern MagentaH 
  , pattern CyanH    
  , pattern WhiteH   

  -- ** Dull Colours
  , pattern BlackL   
  , pattern RedL     
  , pattern GreenL   
  , pattern BlueL    
  , pattern YellowL  
  , pattern MagentaL 
  , pattern CyanL    
  , pattern WhiteL   

  -- ** Either Shade of a Colour
  , pattern BlackX   
  , pattern RedX     
  , pattern GreenX   
  , pattern BlueX    
  , pattern YellowX  
  , pattern MagentaX 
  , pattern CyanX    
  , pattern WhiteX   

  ) where

import Control.Monad

import Data.Colour.SRGB (Colour(..), sRGBBounded)
import Data.Colour.RGBSpace (RGB(..), uncurryRGB)

import Data.Maybe
import Data.Word

import Data.Map.Strict qualified as M

import Prettyprinter.Render.Terminal qualified as PP

import System.IO

import System.Console.ANSI

getBkdColour :: IO (Maybe (RGB Word16))
getBkdColour = getLayerColor Background

-- | Combined `onBackgroundColour` with `colour`.
ctxColour :: Maybe (M.Map (PP.Color, Bool) (RGB Word16), M.Map RGB16 (PP.Color, Bool)) -> Maybe (RGB Word16) -> (PP.Color, Bool) -> PP.AnsiStyle
ctxColour mps bkdClr clr = colour $ onBackgroundColour mps bkdClr clr

onBackgroundColour :: Maybe (M.Map (PP.Color, Bool) (RGB Word16), M.Map RGB16 (PP.Color, Bool)) -> Maybe (RGB Word16) -> (PP.Color, Bool) -> (PP.Color, Bool)
onBackgroundColour _ Nothing  c = c
onBackgroundColour Nothing (Just _) c = c
onBackgroundColour (Just (mp1, mp2)) (Just bkdClr) clr
  | (Just ppBkdClr) <- M.lookup (RGB16 bkdClr) mp2
  = fixColour ppBkdClr clr
  | otherwise = clr -- idk

convertColour :: (PP.Color, Bool) -> (Color, ColorIntensity)
convertColour (PP.Black  , x) = (Black  , vvdty x)
convertColour (PP.Red    , x) = (Red    , vvdty x)
convertColour (PP.Green  , x) = (Green  , vvdty x)
convertColour (PP.Yellow , x) = (Yellow , vvdty x)
convertColour (PP.Blue   , x) = (Blue   , vvdty x)
convertColour (PP.Magenta, x) = (Magenta, vvdty x)
convertColour (PP.Cyan   , x) = (Cyan   , vvdty x)
convertColour (PP.White  , x) = (White  , vvdty x)

vvdty :: Bool -> ColorIntensity
vvdty True  = Vivid
vvdty False = Dull   

defaultColour :: (PP.Color, Bool) -> RGB Word16
defaultColour (PP.Black  , False) = RGB 0x0000 0x0000 0x0000
defaultColour (PP.Red    , False) = RGB 0xAAAA 0x0000 0x0000
defaultColour (PP.Green  , False) = RGB 0xAAAA 0x0000 0x0000
defaultColour (PP.Blue   , False) = RGB 0x0000 0x0000 0xAAAA
defaultColour (PP.Yellow , False) = RGB 0xAAAA 0x5555 0x0000
defaultColour (PP.Magenta, False) = RGB 0xAAAA 0x0000 0xAAAA
defaultColour (PP.Cyan   , False) = RGB 0x0000 0xAAAA 0xAAAA
defaultColour (PP.White  , False) = RGB 0xAAAA 0xAAAA 0xAAAA

defaultColour (PP.Black  , True ) = RGB 0x5555 0x5555 0x5555
defaultColour (PP.Red    , True ) = RGB 0xFFFF 0x5555 0x5555
defaultColour (PP.Green  , True ) = RGB 0x5555 0xFFFF 0x5555
defaultColour (PP.Blue   , True ) = RGB 0x5555 0x5555 0xFFFF
defaultColour (PP.Yellow , True ) = RGB 0xFFFF 0xFFFF 0x5555
defaultColour (PP.Magenta, True ) = RGB 0xFFFF 0x5555 0xFFFF
defaultColour (PP.Cyan   , True ) = RGB 0x5555 0xFFFF 0xFFFF
defaultColour (PP.White  , True ) = RGB 0xFFFF 0xFFFF 0xFFFF

ppColours :: [(PP.Color, Bool)]
ppColours =  [(clr,vvd) | clr <- ppCols, vvd <- [False, True]]

ppCols :: [PP.Color]
ppCols = [PP.Black, PP.Red, PP.Green, PP.Blue, PP.Yellow, PP.Magenta, PP.Cyan, PP.White]

-- | Query the RGB Values of the 16
--   Default Colours.
queryColours :: IO (Maybe (M.Map (PP.Color, Bool) (RGB Word16), M.Map RGB16 (PP.Color, Bool)))
queryColours = do
  origFore <- getLayerColor Foreground
  case origFore of
    -- Can't do anything...
    Nothing -> return Nothing
    (Just origColour) -> do
      -- OKAY
      rslt <- forM ppColours $ \clr' -> do
        -- Convert the colour type...
        let (clr,vvd) = convertColour clr'
        setSGR [SetColor Foreground vvd clr]
        rgbClr <- getLayerColor Foreground
        return (clr', fromMaybe (defaultColour clr') rgbClr)
      -- construct the Map
      let rsltMap = M.fromList rslt
      -- return the original colour
      setSGR [SetRGBColor Foreground (uncurryRGB sRGBBounded origColour)]
      return (Just (rsltMap, reverseMap $ fmap RGB16 rsltMap))


reverseMap :: (Ord a, Ord b) => M.Map a b -> M.Map b a
reverseMap = M.foldlWithKey (\acc k val -> M.insert val k acc) M.empty

-- I don't know how else to do this.
newtype RGB16 = RGB16 {getRGB16 :: RGB Word16}
  deriving newtype (Eq, Read, Show)

instance Ord RGB16 where
  compare (RGB16 (RGB r1 g1 b1)) (RGB16 (RGB r2 g2 b2))
    = case compare r1 r2 of
        EQ -> case compare g1 g2 of
            EQ -> compare b1 b2
            x  -> x
        x -> x
  (RGB16 (RGB r1 g1 b1)) < (RGB16 (RGB r2 g2 b2))
    | r1 < r2   = True
    | r1 > r2   = False
    | g1 < g2   = True
    | g1 > g2   = False
    | otherwise = b1 < b2
  (RGB16 (RGB r1 g1 b1)) <= (RGB16 (RGB r2 g2 b2))
    | r1 <= r2   = True
    | g1 <= g2   = True
    | otherwise = b1 <= b2
    

-- deriving instance Ord (RGB Word16)

-- | Convert a basic Colour on a background colour
--   to a more visible one.
fixColour :: (PP.Color, Bool) -> (PP.Color, Bool) -> (PP.Color, Bool)
fixColour bkdClr fgdClr = case (bkdClr, fgdClr) of
  (BlackH, BlackH) -> WhiteH
  (BlackL, BlackH) -> WhiteL
  (BlackH, BlackL) -> WhiteH
  (BlackL, BlackL) -> WhiteL
  (BlueH , CyanX ) -> WhiteH
  (BlueL , CyanX ) -> WhiteL
  (BlueX , GreenL) -> YellowL
  (BlueX , GreenH) -> YellowH
  (WhiteX, WhiteX) -> BlackL
  -- idk
  (_, clr) -> clr

colour :: (PP.Color, Bool) -> PP.AnsiStyle
colour (clr, True ) = PP.color clr
colour (clr, False) = PP.colorDull clr

pattern BlackH, RedH, GreenH, BlueH, YellowH, MagentaH, CyanH, WhiteH :: (PP.Color, Bool)
pattern BlackH   = (PP.Black  , True)
pattern RedH     = (PP.Red    , True)
pattern GreenH   = (PP.Green  , True)
pattern BlueH    = (PP.Blue   , True)
pattern YellowH  = (PP.Yellow , True)
pattern MagentaH = (PP.Magenta, True)
pattern CyanH    = (PP.Cyan   , True)
pattern WhiteH   = (PP.White  , True)

pattern BlackL, RedL, GreenL, BlueL, YellowL, MagentaL, CyanL, WhiteL :: (PP.Color, Bool)
pattern BlackL   = (PP.Black  , False)
pattern RedL     = (PP.Red    , False)
pattern GreenL   = (PP.Green  , False)
pattern BlueL    = (PP.Blue   , False)
pattern YellowL  = (PP.Yellow , False)
pattern MagentaL = (PP.Magenta, False)
pattern CyanL    = (PP.Cyan   , False)
pattern WhiteL   = (PP.White  , False)

pattern BlackX, RedX, GreenX, BlueX, YellowX, MagentaX, CyanX, WhiteX :: (PP.Color, Bool)
pattern BlackX   <- (PP.Black  , _)
pattern RedX     <- (PP.Red    , _)
pattern GreenX   <- (PP.Green  , _)
pattern BlueX    <- (PP.Blue   , _)
pattern YellowX  <- (PP.Yellow , _)
pattern MagentaX <- (PP.Magenta, _)
pattern CyanX    <- (PP.Cyan   , _)
pattern WhiteX   <- (PP.White  , _)


