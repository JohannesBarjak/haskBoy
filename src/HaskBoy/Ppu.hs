{-# LANGUAGE TemplateHaskell #-}

module HaskBoy.Ppu
    ( Ppu(..)
    , PpuMode(..)
    , Pixel(..)
    , Color(..)
    , Display
    , toPixel, toColor
    , display, clock
    ) where

import HaskBoy.Mmu (Mmu, raw)

import Test.QuickCheck.Arbitrary

import Control.Lens

import Control.Monad.State.Strict (State)
import Data.Vector (Vector)
import Data.Bits (Bits((.&.), shiftR))

type Display = Vector Pixel

data Ppu = Ppu
    { _display :: Display -- ^ Gameboy's 256x256 logical display
    , _clock   :: Integer
    }

data Color
    = White
    | LightGray
    | DarkGray
    | Black
    deriving (Enum, Eq)

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

instance Arbitrary Pixel where
    arbitrary = arbitraryBoundedEnum

-- | 'toPixel' converts a pair of booleans into a 'Pixel'
toPixel
    :: Bool -- ^ Lower bit
    -> Bool -- ^ Upper bit
    -> Pixel

toPixel lb ub = toEnum (fromEnum ub * 2 + fromEnum lb)

-- |  Use the Gameboy's color palette
-- at 0xFF47 to convert a 'Pixel' into a 'Color'
toColor :: Pixel -> State Mmu Color
toColor pixel = do
    palette <- use (raw 0xFF47)
    let color = fromIntegral (palette `shiftR` (fromEnum pixel * 2)) .&. 3

    pure (toEnum color)
