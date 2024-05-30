module HaskBoy.Emulator.Execution (cycleEmulator) where

import Control.Lens
import Control.Monad (when)
import Control.Monad.State.Strict

import HaskBoy.Cpu
import HaskBoy.Cpu.Execution
import HaskBoy.Emulator

import HaskBoy.Ppu
import HaskBoy.Ppu.Execution

cycleEmulator :: Integer -> State Emulator ()
cycleEmulator cycles
    = when (cycles > 0) $ do
        oldTime <- use (cpu.tclock)
        cycleCpu
        newTime <- use (cpu.tclock)

        ppu.clock .= newTime `quot` 8
        ppuCycle

        cycleEmulator (cycles - (newTime - oldTime))
