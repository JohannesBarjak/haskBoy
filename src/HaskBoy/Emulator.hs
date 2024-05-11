{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module HaskBoy.Emulator where

import HaskBoy.Mmu (Mmu)

import HaskBoy.Cpu
    ( Cpu(..)
    , Registers(..)
    )

import HaskBoy.Ppu
    ( Ppu(..)
    , toPixel
    )

import Control.Lens
import Data.Sequence qualified as Seq

data Emulator = Emulator
    { _mmu :: !Mmu
    , _cpu :: !Cpu
    , _ppu :: !Ppu
    }

makeLenses ''Emulator

initialEmulator :: Mmu -> Emulator
initialEmulator _mmu = Emulator
    { _mmu
    , _cpu = initialCpu
    , _ppu = initialPpu
    }

initialCpu :: Cpu
initialCpu = Cpu
    { _register = Registers
        { _af = 0x01B0
        , _bc = 0x0013
        , _de = 0x00D8
        , _hl = 0x014D
        , _pc = 0x0100
        , _sp = 0xFFFE
        }
    , _interruptEnable = True
    , _tclock = 0
    }

initialPpu :: Ppu
initialPpu = Ppu
    { _display = Seq.replicate 144 $ Seq.replicate 160 (toPixel False False)
    , _clock   = 0
    }
