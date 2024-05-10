module HaskBoy.Emulator.Execution (cycleCpu) where

import HaskBoy.Emulator

import HaskBoy.Ppu
import HaskBoy.Ppu.Execution

import HaskBoy.Cpu
import HaskBoy.Cpu.Execution
import HaskBoy.Cpu.Instructions

import Control.Lens
import Control.Monad.State.Strict

cycleCpu :: Integer -> State Emulator ()
cycleCpu cycles
    = when (cycles > 0) $ do
        instr <- consumeByte
        execute =<< toInstruction instr

        instrCost <- use (cpu.tclock)
        cpu.tclock .= 0

        ppu.clock += instrCost

        ppuTime <- use (ppu.clock)

        when (ppuTime > 455) $ do
            ppu.clock -= 456
            mmu.ly += 1

            lineY <- use (mmu.ly)
            when (lineY < 144) drawTiles

        cycleCpu (cycles - instrCost)
