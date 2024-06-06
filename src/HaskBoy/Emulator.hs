{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}

module HaskBoy.Emulator where

import Control.Lens

import HaskBoy.Cpu (Cpu, HasCpu(..), HasRegisters(..), newCpu)
import HaskBoy.Mmu (Mmu, HasMmu(..))
import HaskBoy.Ppu (Ppu, HasPpu(..), newPpu)

data Emulator = Emulator
    { _emulatorMmu :: !Mmu
    , _emulatorCpu :: !Cpu
    , _emulatorPpu :: !Ppu
    }

makeClassy ''Emulator

instance HasMmu Emulator where
    mmu = emulatorMmu

instance HasCpu Emulator where
    cpu = emulatorCpu

instance HasPpu Emulator where
    ppu = emulatorPpu

instance HasRegisters Emulator where
    registers = cpu.register

initialEmulator :: Mmu -> Emulator
initialEmulator _emulatorMmu = Emulator
    { _emulatorMmu
    , _emulatorCpu = newCpu
    , _emulatorPpu = newPpu
    }
