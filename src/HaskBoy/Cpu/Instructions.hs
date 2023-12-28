{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ImportQualifiedPost #-}

module HaskBoy.Cpu.Instructions
    ( inc, dec
    , byteAnd, xor, byteOr
    , jr, call, jmp, ret
    , cmp
    , add, sub, sbc
    , rl, bit
    , consumeByte, consumeWord
    , popStack, pushStack
    ) where

import HaskBoy.Emulator
import HaskBoy.Mmu
import HaskBoy.Cpu

import Data.Word (Word8, Word16)
import Data.Bits (Bits((.&.), (.|.), shiftL))
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

cmp :: Word8 -> State Registers ()
cmp n = do
    a' <- use a

    zero .= (a' == n)
    carry .= (a' < n)
    hcarry .= (a' .&. 0xF < n .&. 0xF)
    subOp .= True

call :: Address -> State Emulator ()
call nn = do
    pushStack =<< use (cpu.register.pc)
    jmp nn

jmp :: Address -> State Emulator ()
jmp nn = cpu.register.pc .= nn

ret :: State Emulator ()
ret = jmp =<< popStack

sbc :: Word8 -> State Registers ()
sbc n = do
    a' <- use a
    carry' <- use carry
    let result = a' - n + fromBool carry'

    zero .= (result == 0)
    hcarry .= (a' .&. 0xF < (n .&. 0xF) + fromBool carry')
    carry .= (fromIntegral a' < (fromIntegral n + fromBool carry' :: Int))
    subOp .= True

    a .= result

add :: Word8 -> State Emulator ()
add n = do
    a' <- use a
    let result = a' + n

    zero .= (result == 0)
    hcarry .= ((a' .&. 0xF) + (n .&. 0xF) > 0xF)
    carry .= (fromIntegral a' + fromIntegral n > (0xFF :: Int))
    subOp .= False

    a .= result

sub :: Word8 -> State Registers ()
sub n = do
    -- Subtraction in the Gameboy sets flags in the same way as comparison
    cmp n
    a -= n

bit :: Int -> Word8 -> State Registers ()
bit n v = do
    zero .= (v .&. shiftL 1 n == 0)
    hcarry .= True
    subOp .= False

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

byteOr :: Word8 -> State Emulator ()
byteOr n = do
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

byteAnd :: Word8 -> State Emulator ()
byteAnd n = do
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
