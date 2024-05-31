{-# LANGUAGE TemplateHaskell #-}

module HaskBoy.Ppu
    ( Ppu(..)
    , PpuMode(..)
    , Pixel(..)
    , Display
    , toPixel
    , display, clock
    , newPpu
    ) where

import Control.Lens
import Data.Sequence (Seq)
import Data.Sequence qualified as S

type Display = Seq (Seq Pixel)

data Ppu = Ppu
    { _display :: !Display -- ^ Gameboy's 160x144 physical display
    , _clock   :: !Integer
    }

data Pixel
    = I0
    | I1
    | I2
    | I3
    deriving (Bounded, Enum, Eq, Show)

data PpuMode
    = HBlank
    | VBlank
    | OamRead
    | VramRead deriving (Enum, Eq)

makeLenses ''Ppu

newPpu :: Ppu
newPpu = Ppu
    { _display = S.replicate 144 . S.replicate 160 $ toPixel False False
    , _clock   = 0
    }

-- | 'toPixel' converts a pair of booleans into a 'Pixel'
toPixel
    :: Bool -- ^ Lower bit
    -> Bool -- ^ Upper bit
    -> Pixel

toPixel lb ub = toEnum (fromEnum ub * 2 + fromEnum lb)
