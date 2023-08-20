{-# LANGUAGE ImportQualifiedPost #-}
module Main where

import Emulator
import HaskBoy.Mmu

import Cpu
import Cpu.Instructions

import HaskBoy.Ppu

import Test.Hspec
import Test.QuickCheck

import Control.Lens
import Control.Monad.State.Strict

import Data.Vector qualified as V
import Data.Word
import Data.Bits

import Data.Sequence as Seq
import Test.Hspec.QuickCheck (modifyMaxSuccess)

main :: IO ()
main = do
    hspec $ do
        describe "HaskBoy.Mmu" $ do
            testAddr
            testAddr16
        describe "HaskBoy.Ppu" $ do
            testToPixel
            testToColor

    quickCheck prop_twoCompl
    quickCheck prop_ReadMemAlwaysZero
    quickCheck prop_BLensPutGet
    quickCheck prop_ALensPutGet
    quickCheck prop_BCLensConsistency
    quickCheck prop_DecBCRegs
    quickCheck prop_StackFunConsistency
    quickCheck prop_RegisterConsistency

testAddr :: Spec
testAddr = describe "Mmu.addr" $ do
    context "when writing to rom" . modifyMaxSuccess (const 0x8000) $
        it "ignores writes" $
            forAll (choose (0,0x7FFF)) $ \nn v ->
                evalState ((addr nn .= v) *> use (addr nn)) testMmu == evalState (use (addr nn)) testMmu
    context "when writing to vram" . modifyMaxSuccess (const 0x2000) $
        it "saves writes" $
            forAll (choose (0x8000, 0x9FFF)) $ \nn v ->
                evalState ((addr nn .= v) *> use (addr nn)) testMmu == v

testAddr16 :: Spec
testAddr16 = describe "Mmu.addr16" $ do
    context "when writing to wram" $
        it "writes bytes in little endian" $
            evalState littleEndianTestCode testMmu == (0x06, 0xB9)

    where littleEndianTestCode = do
            addr16 0xFF80 .= 0xB906
            (,) <$> use (addr 0xFF80) <*> use (addr 0xFF81)

testToPixel :: Spec
testToPixel =
    describe "Ppu.toPixel" $ do
        it "converts a pair of booleans to a pixel" $ do
            toPixel False False `shouldBe` I0
            toPixel True  False `shouldBe` I1
            toPixel False True  `shouldBe` I2
            toPixel True  True  `shouldBe` I3
        
testToColor :: Spec
testToColor =
    describe "Ppu.toColor" $ do
        it "converts from pixel to color" $ do
            property $ \pixel ->
                        result 0xE4 pixel == toEnum (fromEnum pixel) &&
                        result 0x1B pixel == toEnum (3 - fromEnum pixel) &&
                        result 0x00 pixel == White &&
                        result 0x55 pixel == LightGray &&
                        result 0xAA pixel == DarkGray &&
                        result 0xFF pixel == Black

    where result :: Word8 -> Pixel -> Color
          result palette pixel = flip evalState testMmu $ do
            raw 0xFF47 .= palette
            toColor pixel

testEmulator :: Emulator
testEmulator = Emulator
    { _mmu = testMmu
    , _cpu = testCpu
    , _ppu = testPpu
    }

testMmu :: Mmu
testMmu = Mmu
        { _rom0  = Seq.replicate 0x4000 0
        , _rom1  = Seq.replicate 0x4000 0
        , _vram  = Seq.replicate 0x2000 0
        , _eram  = Seq.replicate 0x2000 0
        , _wram0 = Seq.replicate 0x1000 0
        , _wram1 = Seq.replicate 0x1000 0
        , _oam   = Seq.replicate 0xA0 0
        , _ioreg = Seq.replicate 0x80 0
        , _hram  = Seq.replicate 0x7F 0
        , _ie    = 0
        }

testCpu :: Cpu
testCpu = Cpu
    { _register = Register
        { _af = 0x01B0
        , _bc = 0x0013
        , _de = 0x00D8
        , _hl = 0x014D
        , _pc = 0x0000
        , _sp = 0xFFFE
        }
    , _tclock = 0
    }

testPpu :: Ppu
testPpu = Ppu
    { _display = V.replicate (256 * 256) (toPixel False False)
    , _clock   = 0
    }

prop_StackFunConsistency :: Word16 -> Bool
prop_StackFunConsistency v = v == evalState testCode testEmulator
    where testCode = do
            pushStack v
            popStack

prop_RegisterConsistency :: Word16 -> Bool
prop_RegisterConsistency v = all (v ==) $ evalState testCode testEmulator
    where testCode = do
            cpu.register.af .= v
            cpu.register.bc .= v
            cpu.register.de .= v
            cpu.register.hl .= v

            traverse use [cpu.register.af, cpu.register.bc, cpu.register.de, cpu.register.hl]

prop_DecBCRegs :: Word8 -> Property
prop_DecBCRegs x = x > 0 ==> x - 1 == y
    where y = flip evalState testEmulator $ do
            cpu.register.b .= x
            dec (cpu.register.b)
            use (cpu.register.b)

prop_ReadMemAlwaysZero :: Address -> Property
prop_ReadMemAlwaysZero nn = (nn < 0xFEA0 || nn > 0xFEFF) ==>
    evalState (use (mmu.addr nn)) testEmulator == 0

prop_BLensPutGet :: Word8 -> Bool
prop_BLensPutGet x = evalState (register.b .= x >> use (register.b)) testCpu == x

prop_ALensPutGet :: Word8 -> Bool
prop_ALensPutGet x = evalState (register.a .= x >> use (register.a)) testCpu == x

prop_BCLensConsistency :: Word8 -> Word8 -> Bool
prop_BCLensConsistency x y = flip evalState testCpu $ do
    register.bc .= fromIntegral x `shiftL` 8
    b1 <- use (register.b)
    register.b .= x
    b2 <- use (register.b)

    register.c .= y
    c2 <- use (register.c)
    register.bc .= fromIntegral y
    c1 <- use (register.c)

    pure (c1 == c2 && b1 == b2 && b1 == x && c1 == y)

prop_twoCompl :: Word8 -> Bool
prop_twoCompl x
    | x < 128   = twoCompl x >= 0
    | otherwise = twoCompl x <= 0
