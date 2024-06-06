{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}

module HaskBoy.Emulator where

import Control.Lens

import HaskBoy.Cpu (Cpu, HasCpu(..), newCpu)
import HaskBoy.Mmu (Mmu)
import HaskBoy.Ppu (Ppu, HasPpu(..), newPpu)

data Emulator = Emulator
    { _mmu :: !Mmu
    , _emulatorCpu :: !Cpu
    , _emulatorPpu :: !Ppu
    }

makeClassy ''Emulator

instance HasCpu Emulator where
    cpu = emulatorCpu

instance HasPpu Emulator where
    ppu = emulatorPpu

initialEmulator :: Mmu -> Emulator
initialEmulator _mmu = Emulator
    { _mmu
    , _emulatorCpu = newCpu
    , _emulatorPpu = newPpu
    }
