module HaskBoy.Cpu.Instructions
    ( inc, dec
    , and, xor, or
    , jr, call, jmp, ret
    , cmp
    , add, sub, sbc
    , add16
    , rl, bit, swap
    , cpl
    , consumeByte, consumeWord
    , popStack, pushStack
    ) where

import Control.Lens
import Control.Monad.State.Strict

import Data.Bits ((.&.), (.|.), shiftL, (.<<.), (.>>.), complement)
import Data.Bits qualified as Bits
import Data.Word (Word8, Word16)
import Foreign.Marshal.Utils (fromBool, toBool)

import HaskBoy.Cpu
import HaskBoy.Emulator
import HaskBoy.Mmu

import Prelude hiding (and, or)

inc :: (MonadState s m, HasRegisters s) => ALens' s Word8 -> m ()
inc r = do
    v <- use (cloneLens r)
    let result = v + 1

    zero .= (result == 0)
    hcarry .= (v .&. 0xF == 0xF)
    subOp .= False

    r #= result

dec :: (MonadState s m, HasRegisters s) => ALens' s Word8 -> m ()
dec r = do
    v <- use (cloneLens r)
    let result = v - 1

    zero .= (result == 0)
    hcarry .= (v .&. 0xF == 0)
    subOp .= True

    r #= result

jr :: Bool -> State Emulator ()
jr jump = do
    sb <- consumeByte
    nn <- fromIntegral <$> use (cpu.register.pc)

    if jump then do
        jmp $ fromIntegral (nn + twoCompl sb)
        cpu.tclock += 12

    else cpu.tclock += 8

cmp :: (MonadState s m, HasRegisters s) => ALens' s Word8 -> m ()
cmp r = do
    a <- use (af.upperByte)
    n <- use (cloneLens r)

    zero .= (a == n)
    carry .= (a < n)
    hcarry .= (a .&. 0xF < n .&. 0xF)
    subOp .= True

call :: Address -> State Emulator ()
call nn = do
    pushStack =<< use (cpu.register.pc)
    jmp nn

jmp :: (MonadState s m, HasRegisters s) => Address -> m ()
jmp nn = pc .= nn

ret :: State Emulator ()
ret = jmp =<< popStack

sbc :: Word8 -> State Emulator ()
sbc n = do
    a <- use (cpu.register.af.upperByte)
    carry' <- use (cpu.register.carry)
    let result = a - n + fromBool carry'

    cpu.register.zero .= (result == 0)
    cpu.register.hcarry .= (a .&. 0xF < (n .&. 0xF) + fromBool carry')
    cpu.register.carry .= (fromIntegral a < (fromIntegral n + fromBool carry' :: Int))
    cpu.register.subOp .= True

    cpu.register.af.upperByte .= result

add :: Word8 -> State Emulator ()
add n = do
    a <- use (cpu.register.af.upperByte)
    let result = a + n

    cpu.register.zero .= (result == 0)
    cpu.register.hcarry .= ((a .&. 0xF) + (n .&. 0xF) > 0xF)
    cpu.register.carry .= (toInteger a + toInteger n > 0xFF)
    cpu.register.subOp .= False

    cpu.register.af.upperByte .= result

add16 :: ALens' Emulator Word16 -> State Emulator ()
add16 wl = do
    v <- use (cpu.register.hl)
    w <- use (cloneLens wl)

    cpu.register.hcarry .= ((v .&. 0x07FF) + (w .&. 0x07FF) > 0x07FF)
    cpu.register.carry .= (v > 0xFFFF - w)
    cpu.register.subOp .= False

    cpu.register.hl .= v + w

sub :: ALens' Emulator Word8 -> State Emulator ()
sub n = do
    -- Subtraction in the Gameboy sets flags in the same way as comparison
    cmp n
    v <- use (cloneLens n)
    cpu.register.af.upperByte -= v

bit :: Int -> ALens' Emulator Word8 -> State Emulator ()
bit n r = do
    v <- use (cloneLens r)

    cpu.register.zero .= (v .&. shiftL 1 n == 0)
    cpu.register.hcarry .= True
    cpu.register.subOp .= False

rl :: (MonadState s m, HasRegisters s) => Lens' s Word8 -> m ()
rl r = do
    oldCarry <- use carry

    -- Set carry flag to the register's 7th bit
    carry <~ newCarry

    r %= (`shiftL` 1)
    r += fromBool oldCarry

    zero <~ not . toBool <$> use r
    subOp .= False
    hcarry .= False

    where newCarry = toBool . (.&. (1 `shiftL` 7)) <$> use r

-- This instructions swaps nibbles
swap :: (MonadState s m, HasRegisters s) => ALens' s Word8 -> m ()
swap r = do
    v <- use (cloneLens r)
    let result = v .>>. 4 .|. v .<<. 4;

    zero .= (result == 0)
    hcarry .= False
    carry .= False
    subOp .= False

    r #= result

or :: (MonadState s m, HasRegisters s) => ALens' s Word8 -> m ()
or vl = do
    a <- use (af.upperByte)
    n <- use (cloneLens vl)
    let result = a .|. n

    zero .= (result == 0)
    hcarry .= False
    carry .= False
    subOp .= False

    af.upperByte .= result

xor :: (MonadState s m, HasRegisters s) => Word8 -> m ()
xor n = do
    a <- use (af.upperByte)
    let result = Bits.xor a n

    zero .= (result == 0)
    hcarry .= False
    carry .= False
    subOp .= False

    af.upperByte .= result

and :: (MonadState s m, HasRegisters s) => Word8 -> m ()
and n = do
    a <- use (af.upperByte)
    let result = a .&. n

    zero .= (result == 0)
    hcarry .= True
    carry .= False
    subOp .= False

    af.upperByte .= result

cpl :: (MonadState s m, HasRegisters s) => m ()
cpl = do
        af.upperByte %= complement
        hcarry .= True
        subOp .= True

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

    use (mmu.cloneLens (addr nn))

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
