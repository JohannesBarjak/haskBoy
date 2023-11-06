{-# LANGUAGE ImportQualifiedPost #-}
module Main where

import HaskBoy.Emulator
import HaskBoy.Mmu

import HaskBoy.Cpu
import HaskBoy.Cpu.Instructions

import HaskBoy.Ppu
import HaskBoy.Ppu.Execution

import Test.Hspec
import Test.QuickCheck

import Control.Lens
import Control.Monad.State.Strict

import Data.Vector qualified as V
import Data.Word

import Data.Sequence qualified as Seq
import Test.Hspec.QuickCheck (modifyMaxSuccess)
import Data.Foldable (for_)

main :: IO ()
main = do
    hspec $ do
        describe "HaskBoy.Cpu" $ do
            testRegisters
        describe "HaskBoy.Mmu" $ do
            testAddr
            testAddr16
        describe "HaskBoy.Ppu" $ do
            testToPixel
            testToColor
        describe "HaskBoy.Ppu.Execution" $ do
            testTileRow
            testGetTile

    quickCheck prop_DecBCRegs
    quickCheck prop_StackFunConsistency

testAddr :: Spec
testAddr = describe "addr" $ do
    context "when writing to rom" . modifyMaxSuccess (const 0x8000) $
        it "ignores writes" $
            forAll (choose (0,0x7FFF)) $ \nn v ->
                evalState ((addr nn .= v) *> use (addr nn)) testMmu == evalState (use (addr nn)) testMmu
    context "when writing to vram" . modifyMaxSuccess (const 0x2000) $
        it "saves writes" $
            forAll (choose (0x8000, 0x9FFF)) $ \nn v ->
                evalState ((addr nn .= v) *> use (addr nn)) testMmu == v

testAddr16 :: Spec
testAddr16 = describe "addr16" $ do
    context "when writing to wram" $
        it "writes bytes in little endian" $
            evalState littleEndianTestCode testMmu == (0x06, 0xB9)

    where littleEndianTestCode = do
            addr16 0xFF80 .= 0xB906
            (,) <$> use (addr 0xFF80) <*> use (addr 0xFF81)

testToPixel :: Spec
testToPixel =
    describe "toPixel" $ do
        it "converts a pair of booleans to a pixel" $ do
            toPixel False False `shouldBe` I0
            toPixel True  False `shouldBe` I1
            toPixel False True  `shouldBe` I2
            toPixel True  True  `shouldBe` I3
        
testToColor :: Spec
testToColor =
    describe "toColor" $ do
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

testRegisters :: Spec
testRegisters = do
    describe "16bit registers" $
        it "writes a value to 16bit registers and checks write integrity" $
            property $ \v -> all (v ==) (evalState (writeToRegisters v wrs) testCpu)
    describe "8bit registers" $
        it "writes a value to 8bit registers and checks write integrity" $
            property $ \v -> all (v ==) (evalState (writeToRegisters v brs) testCpu)

    where writeToRegisters :: a -> [ALens' Registers a] -> State Cpu [a]
          writeToRegisters v rs = do
            for_ (map cloneLens rs) $ \r -> do
                register.r .= v
            
            traverse (use . (register.) . cloneLens) rs

          wrs = [af, bc, de, hl, pc, sp]
          brs = [a, flag, b, c, d, e, h, l]

testTileRow :: Spec
testTileRow =
    describe "tileRow" $ do
        it "always returns even enums when the first byte is zero" $
            property $ \v -> all (even . fromEnum) (tileRow (0,v))

        it "always returns odd enums when the first byte is 0xFF" $
            property $ \v -> all (odd . fromEnum) (tileRow (0xFF,v))

testGetTile :: Spec
testGetTile =
    describe "getTile" $ do
        it "always returns an 8x8 tile" $
            property $ \v -> flip evalState testMmu $ do
                tile <- getTile v
                pure (length tile == 8 && all ((== 8) . V.length) tile)

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
    { _register = Registers
        { _af = 0x01B0
        , _bc = 0x0013
        , _de = 0x00D8
        , _hl = 0x014D
        , _pc = 0x0000
        , _sp = 0xFFFE
        }
    , _interruptEnable = True
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

prop_DecBCRegs :: Word8 -> Property
prop_DecBCRegs x = x > 0 ==> x - 1 == y
    where y = flip evalState testEmulator $ do
            cpu.register.b .= x
            dec (cpu.register.b)
            use (cpu.register.b)
