module HaskBoy.Emulator.Execution (cycleEmulator) where

import Control.Lens
import Control.Monad (when, void)
import Control.Monad.State.Strict
import Control.Monad.Trans.Maybe

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
        void $ runMaybeT cyclePpu

        cycleEmulator (cycles - (newTime - oldTime))
