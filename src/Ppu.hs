{-# LANGUAGE TemplateHaskell #-}

module Ppu
    ( Ppu(..)
    , Pixel
    , Color(..)
    , toPixel
    , display, clock
    ) where

import Mmu (Mmu, raw)

import Control.Lens (makeLenses, use)
import Control.Monad.State.Strict (State)
import Data.Vector (Vector)
import Data.Bits (Bits((.&.), shiftR))

data Ppu = Ppu
    { _display   :: Vector Pixel
    , _clock     :: Integer
    }

data Color
    = White
    | LightGray
    | DarkGray
    | Black
    deriving Enum

data Pixel
    = I0
    | I1
    | I2
    | I3
    deriving Enum

data PPUMode
    = HBlank
    | VBlank
    | OAMRead
    | VRAMRead deriving Enum

makeLenses ''Ppu

toPixel :: Bool -> Bool -> Pixel
toPixel ub lb = toEnum (fromEnum ub * 2 + fromEnum lb)

toColor :: Pixel -> State Mmu Color
toColor pixel = do
    palette <- use (raw 0xFF47)
    let color = fromIntegral (palette `shiftR` (fromEnum pixel * 2)) .&. 3

    pure (toEnum color)
