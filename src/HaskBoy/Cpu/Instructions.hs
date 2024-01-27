{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ImportQualifiedPost #-}

module HaskBoy.Cpu.Instructions
    ( inc, dec
    , and, xor, or
    , jr, call, jmp, ret
    , cmp
    , add, sub, sbc
    , rl, bit, swap
    , consumeByte, consumeWord
    , popStack, pushStack
    ) where

import Prelude hiding (and, or)

import HaskBoy.Emulator
import HaskBoy.Mmu
import HaskBoy.Cpu

import Data.Word (Word8, Word16)
import Data.Bits (Bits((.&.), (.|.), shiftL), (.<<.), (.>>.))
import Data.Bits qualified as Bits

import Control.Monad.State.Strict
import Control.Lens

import Foreign.Marshal.Utils (fromBool, toBool)

inc :: ALens' Emulator Word8 -> State Emulator ()
inc r = do
    v <- use (cloneLens r)
    let result = v + 1

    cpu.register.zero .= (result == 0)
    cpu.register.hcarry .= (v .&. 0xF == 0xF)
    cpu.register.subOp .= False

    cloneLens r .= result

dec :: ALens' Emulator Word8 -> State Emulator ()
dec r = do
    v <- use (cloneLens r)
    let result = v - 1

    cpu.register.zero .= (result == 0)
    cpu.register.hcarry .= (v .&. 0xF == 0)
    cpu.register.subOp .= True

    cloneLens r .= result

jr :: Bool -> State Emulator ()
jr jump = do
    sb <- consumeByte
    nn <- fromIntegral <$> use (cpu.register.pc)

    if jump then do
        jmp $ fromIntegral (nn + twoCompl sb)
        cpu.tclock += 12

    else cpu.tclock += 8

cmp :: ALens' Emulator Word8 -> State Emulator ()
cmp r = do
    a' <- use (cpu.register.a)
    n <- use (cloneLens r)

    cpu.register.zero .= (a' == n)
    cpu.register.carry .= (a' < n)
    cpu.register.hcarry .= (a' .&. 0xF < n .&. 0xF)
    cpu.register.subOp .= True

call :: Address -> State Emulator ()
call nn = do
    pushStack =<< use (cpu.register.pc)
    jmp nn

jmp :: Address -> State Emulator ()
jmp nn = cpu.register.pc .= nn

ret :: State Emulator ()
ret = jmp =<< popStack

sbc :: Word8 -> State Emulator ()
sbc n = do
    a' <- use (cpu.register.a)
    carry' <- use (cpu.register.carry)
    let result = a' - n + fromBool carry'

    cpu.register.zero .= (result == 0)
    cpu.register.hcarry .= (a' .&. 0xF < (n .&. 0xF) + fromBool carry')
    cpu.register.carry .= (fromIntegral a' < (fromIntegral n + fromBool carry' :: Int))
    cpu.register.subOp .= True

    cpu.register.a .= result

add :: Word8 -> State Emulator ()
add n = do
    a' <- use (cpu.register.a)
    let result = a' + n

    cpu.register.zero .= (result == 0)
    cpu.register.hcarry .= ((a' .&. 0xF) + (n .&. 0xF) > 0xF)
    cpu.register.carry .= (toInteger a' + toInteger n > 0xFF)
    cpu.register.subOp .= False

    cpu.register.a .= result

sub :: ALens' Emulator Word8 -> State Emulator ()
sub n = do
    -- Subtraction in the Gameboy sets flags in the same way as comparison
    cmp n
    v <- use (cloneLens n)
    cpu.register.a -= v

bit :: Int -> ALens' Emulator Word8 -> State Emulator ()
bit n r = do
    v <- use (cloneLens r)

    cpu.register.zero .= (v .&. shiftL 1 n == 0)
    cpu.register.hcarry .= True
    cpu.register.subOp .= False

rl :: Lens' Registers Word8 -> State Emulator ()
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

-- This instructions swaps nibbles
swap :: ALens' Emulator Word8 -> State Emulator ()
swap r = do
    v <- use (cloneLens r)
    let result = v .>>. 4 .|. v .<<. 4;

    cpu.register.zero .= (result == 0)
    cpu.register.hcarry .= False
    cpu.register.carry .= False
    cpu.register.subOp .= False

    cloneLens r .= result

or :: Word8 -> State Emulator ()
or n = do
    a' <- use (cpu.register.a)
    let result = a' .|. n

    cpu.register.zero .= (result == 0)
    cpu.register.hcarry .= False
    cpu.register.carry .= False
    cpu.register.subOp .= False

    cpu.register.a .= result

xor :: Word8 -> State Emulator ()
xor n = do
    a' <- use (cpu.register.a)
    let result = Bits.xor a' n

    cpu.register.zero .= (result == 0)
    cpu.register.hcarry .= False
    cpu.register.carry .= False
    cpu.register.subOp .= False

    cpu.register.a .= result

and :: Word8 -> State Emulator ()
and n = do
    a' <- use (cpu.register.a)
    let result = a' .&. n

    cpu.register.zero .= (result == 0)
    cpu.register.hcarry .= True
    cpu.register.carry .= False
    cpu.register.subOp .= False

    cpu.register.a .= result

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
