{-# LANGUAGE TemplateHaskell     #-}

module Ppu
    ( display
    , clock
    , Ppu(..)
    , PPUMode(..)
    , Color(..)
    , ColorIndex(..)
    ) where

import Control.Lens (makeLenses)
import Data.Vector (Vector)

data Ppu = Ppu
    { _display   :: Vector ColorIndex
    , _clock     :: Integer
    }

-- TODO: Make a fancier datatype which can only be constructed with
-- two bytes but can be read as a color

data Color
    = White
    | LightGray
    | DarkGray
    | Black
    deriving Enum

data ColorIndex
    = C0
    | C1
    | C2
    | C3
    deriving Enum

data PPUMode
    = HBlank
    | VBlank
    | OAMRead
    | VRAMRead deriving Enum

makeLenses ''Ppu
