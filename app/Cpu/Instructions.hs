{-# LANGUAGE RankNTypes #-}

module Cpu.Instructions
    ( inc, dec
    , xorA
    , jr, call, ret
    , cmp
    , add, sub, sbc
    , rl, bit
    , consumeByte, consumeWord
    , popStack, pushStack
    ) where

import HaskBoy.Emulator
import HaskBoy.Mmu
import HaskBoy.Cpu

import Debug.Trace (traceM)

import Data.Word (Word8, Word16)
import Data.Bits (Bits(xor, (.&.), shiftL))

import Control.Monad.State.Strict
import Control.Lens

import Foreign.Marshal.Utils (fromBool, toBool)

inc :: Lens' Emulator Word8 -> State Emulator ()
inc r = do
    cpu.register.hcarry <~ (== 0xF) . (.&. 0xF) <$> use r

    r += 1

    cpu.register.zero <~ not . toBool <$> use r
    cpu.register.subOp .= False

dec :: Lens' Emulator Word8 -> State Emulator ()
dec r = do
    cpu.register.hcarry <~ not . toBool . (.&. 0xF) <$> use r

    r -= 1

    cpu.register.zero <~ not . toBool <$> use r
    cpu.register.subOp .= True

jr :: Bool -> State Emulator ()
jr cond = do
    if cond then do
        pc' <- use (cpu.register.pc)
        sb <- use (mmu.addr pc')

        cpu.register.pc .= fromIntegral (fromIntegral pc' + twoCompl sb + 1)
        traceM $ "signed byte: " ++ show (twoCompl sb)
        cpu.tclock += 12
    else do
        cpu.register.pc += 1 -- Consume byte
        cpu.tclock += 8

cmp :: Word8 -> State Emulator ()
cmp n = do
    v <- use (cpu.register.a)

    cpu.register.zero   .= (v == n)
    cpu.register.carry  .= (v < n)
    cpu.register.hcarry .= (v .&. 0xF < n .&. 0xF)
    cpu.register.subOp  .= True

call :: Address -> State Emulator ()
call nn = do
    pushStack =<< use (cpu.register.pc)
    cpu.register.pc .= nn

ret :: State Emulator ()
ret = do
    v <- popStack
    cpu.register.pc .= v

sbc :: Word8 -> State Emulator ()
sbc v = zoom cpu $ do
    a' <- use (register.a)
    oldCarry <- use (register.carry)
    let n = v + fromIntegral (fromEnum oldCarry)

    register.a .= (a' - n)

    register.carry .= (a' < n)
    register.hcarry .= ((v .&. 0xF) + fromIntegral (fromEnum oldCarry) > a' .&. 0xF)
    register.zero <~ (use (register.a) <&> (== 0))
    register.subOp .= True

add :: Word8 -> State Emulator ()
add v = zoom cpu $ do
    a' <- use (register.a)
    register.hcarry .= ((a' .&. 0xF) +  (v .&. 0xF) > 0xF)
    register.carry .= (fromIntegral a' + fromIntegral v > (0xFF :: Int))

    register.a .= (a' + v)

    register.zero <~ (use (register.a) <&> (== 0))
    register.subOp .= False

sub :: Word8 -> State Emulator ()
sub v = zoom cpu $ do
    a' <- use (register.a)
    register.carry .= (v > a')
    register.hcarry .= (v .&. 0xF > a' .&. 0xF)

    register.a .= (a' - v)

    register.zero <~ (use (register.a) <&> (== 0))
    register.subOp .= True

bit :: Int -> Word8 -> State Emulator ()
bit n v = zoom cpu $ do
    register.zero .= (v .&. shiftL 1 n == 0)
    register.hcarry .= True
    register.subOp .= False

rl :: Lens' Register Word8 -> State Emulator ()
rl r = zoom cpu $ do
    oldCarry <- use (register.carry)

    -- Set carry flag to the register's 7th bit
    register.carry <~ newCarry

    register.r %= (`shiftL` 1)
    register.r += fromBool oldCarry

    register.zero <~ not . toBool <$> use (register.r)
    register.subOp .= False
    register.hcarry .= False

    where newCarry = do
            v <- use (register.r)
            pure $ toBool (v .&. (1 `shiftL` 7))

-- Xor register A
xorA :: Word8 -> State Emulator ()
xorA v = zoom cpu $ do
    register.a %= xor v

    register.hcarry .= False
    register.carry  .= False
    register.subOp  .= False

    register.zero <~ not . toBool <$> use (register.a)

-- Read the current and following byte as a 16-bit word
-- and then increase the pc register by 2
consumeWord :: State Emulator Word16
consumeWord = do
    nn <- use (cpu.register.pc)
    cpu.register.pc += 2

    use (mmu.addr16 nn)

-- Read the current byte and then increase the pc register
consumeByte :: State Emulator Word8
consumeByte = do
    nn <- use (cpu.register.pc)
    cpu.register.pc += 1

    use (mmu.addr nn)

-- Pop 16-bit stack
popStack :: State Emulator Word16
popStack = do
    nn <- use (cpu.register.sp)
    cpu.register.sp += 2

    use (mmu.addr16 nn)

-- Push to the 16-bit stack
pushStack :: Word16 -> State Emulator ()
pushStack v = do
    cpu.register.sp -= 2
    nn <- use (cpu.register.sp)

    mmu.addr16 nn .= v
