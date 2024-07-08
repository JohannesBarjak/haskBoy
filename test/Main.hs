module Main where

import Control.Lens
import Control.Monad.State.Strict

import HaskBoy.Cpu
import HaskBoy.Cpu.Instructions

import HaskBoy.Emulator
import HaskBoy.Mmu

import HaskBoy.Ppu as Ppu
import HaskBoy.Ppu.Execution as Ppu.Exec

import Test.Hspec
import Test.QuickCheck
import Data.Bits (complement)

main :: IO ()
main = hspec do
    describe "Ppu.twoCompl" do
        it "is similar to complement from Data.Bits" $ property \x ->
            Ppu.Exec.twoCompl x `shouldBe` fromIntegral (complement x)
    pure ()
