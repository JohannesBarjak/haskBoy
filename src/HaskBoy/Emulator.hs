{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}

module HaskBoy.Emulator where

import Control.Lens

import HaskBoy.Cpu (Cpu, newCpu)
import HaskBoy.Mmu (Mmu)
import HaskBoy.Ppu (Ppu, newPpu)

data Emulator = Emulator
    { _mmu :: !Mmu
    , _cpu :: !Cpu
    , _ppu :: !Ppu
    }

makeLenses ''Emulator

initialEmulator :: Mmu -> Emulator
initialEmulator _mmu = Emulator
    { _mmu
    , _cpu = newCpu
    , _ppu = newPpu
    }
