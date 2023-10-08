{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ImportQualifiedPost #-}

module HaskBoy.Cpu.Instructions
    ( inc, dec
    , byteAnd, xor, byteOr
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
import Data.Bits (Bits((.&.), (.|.), shiftL))
import Data.Bits qualified as Bits

import Control.Monad.State.Strict
import Control.Lens

import Foreign.Marshal.Utils (fromBool, toBool)

inc :: Lens' Emulator Word8 -> State Emulator ()
inc r = do
    v <- use r
    let result = v + 1

    cpu.register.zero .= (result == 0)
    cpu.register.hcarry .= (v .&. 0xF == 0xF)
    cpu.register.subOp .= False

    r .= result

dec :: Lens' Emulator Word8 -> State Emulator ()
dec r = do
    v <- use r
    let result = v - 1

    cpu.register.zero .= (result == 0)
    cpu.register.hcarry .= (v .&. 0xF == 0)
    cpu.register.subOp .= True

    r .= result

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

cmp :: Word8 -> State Registers ()
cmp n = do
    v <- use a

    zero .= (v == n)
    carry .= (v < n)
    hcarry .= (v .&. 0xF < n .&. 0xF)
    subOp .= True

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

sub :: Word8 -> State Registers ()
sub n = do
    v <- use a

    -- Subtraction in the Gameboy sets flags by comparing
    cmp n
    a .= v - n

bit :: Int -> Word8 -> State Emulator ()
bit n v = zoom cpu $ do
    register.zero .= (v .&. shiftL 1 n == 0)
    register.hcarry .= True
    register.subOp .= False

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

byteOr :: Word8 -> State Registers ()
byteOr n = do
    v <- use a
    let result = v .|. n

    zero .= (result == 0)
    hcarry .= False
    carry .= False
    subOp .= False

    a .= result

xor :: Word8 -> State Registers ()
xor n = do
    v <- use a
    let result = Bits.xor v n

    zero .= (result == 0)
    hcarry .= False
    carry .= False
    subOp .= False

    a .= result

byteAnd :: Word8 -> State Registers ()
byteAnd n = do
    v <- use a
    let result = v .&. n

    zero .= (result == 0)
    hcarry .= True
    carry .= False
    subOp .= False

    a .= result

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
