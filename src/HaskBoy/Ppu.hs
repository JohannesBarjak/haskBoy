{-# LANGUAGE TemplateHaskell #-}

module HaskBoy.Ppu
    ( Ppu(..)
    , PpuMode(..)
    , Pixel(..)
    , Display
    , toPixel
    , display, clock
    ) where

import Control.Lens
import Data.Sequence (Seq)

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
    | OAMRead
    | VRAMRead deriving Enum

makeLenses ''Ppu

-- | 'toPixel' converts a pair of booleans into a 'Pixel'
toPixel
    :: Bool -- ^ Lower bit
    -> Bool -- ^ Upper bit
    -> Pixel

toPixel lb ub = toEnum (fromEnum ub * 2 + fromEnum lb)
