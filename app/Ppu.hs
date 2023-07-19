{-# LANGUAGE TemplateHaskell     #-}

module Ppu
    ( display
    , clock
    , Ppu(..)
    , PPUMode(..)
    , Color(..)
    ) where

import Control.Lens (makeLenses)
import Data.Vector (Vector)

data Ppu = Ppu
    { _display   :: Vector (Vector Color)
    , _clock     :: Integer
    }

data Color
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
