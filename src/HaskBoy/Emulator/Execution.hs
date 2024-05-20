module HaskBoy.Emulator.Execution (cycleCpu) where

import Control.Lens
import Control.Monad (when)
import Control.Monad.State.Strict

import HaskBoy.Cpu
import HaskBoy.Cpu.Execution
import HaskBoy.Cpu.Instructions

import HaskBoy.Emulator
import HaskBoy.Ppu
import HaskBoy.Ppu.Execution

cycleCpu :: Integer -> State Emulator ()
cycleCpu cycles
    = when (cycles > 0) $ do
        instr <- consumeByte
        execute =<< toInstruction instr

        instrCost <- use (cpu.tclock)
        cpu.tclock .= 0

        ppu.clock += instrCost * 2
        ppuCycle

        cycleCpu (cycles - instrCost)
