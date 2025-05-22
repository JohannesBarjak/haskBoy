module HaskBoy.Cpu.Instructions
    ( inc, dec
    , and, xor, or
    , jr, call, jmp, ret
    , cmp
    , add, sub, sbc
    , add16
    , rl, bit, swap
    , res, cpl
    , consumeByte, consumeWord
    , popStack, pushStack, stackStore
    , Argument(..)
    , readArg, writeArg
    ) where

import Control.Lens
import Control.Monad.State.Strict

import Data.Bits ((.&.), (.|.), shiftL, (.<<.), (.>>.), complement)
import Data.Bits qualified as Bits
import Data.Word (Word8, Word16)
import Foreign.Marshal.Utils (fromBool, toBool)

import HaskBoy.Cpu
import HaskBoy.Mmu

import Prelude hiding (and, or)

data Argument s where
    Register :: HasRegisters s => (ALens' s Word8) -> Argument s
    Address :: Word16 -> Argument s

readArg :: HasMmu s => Argument s -> Getter s Word8
readArg (Register r) = cloneLens r
readArg (Address  a) = readM a

writeArg :: HasMmu s => Argument s -> Setter' s Word8
writeArg (Register r) = cloneLens r
writeArg (Address  a) = writeM a

inc :: (MonadState s m, HasRegisters s, HasMmu s) => Word8 -> m Word8
inc v = do
    let result = v + 1

    zero .= (result == 0)
    hcarry .= (v .&. 0xF == 0xF)
    subOp .= False

    pure result

dec :: (MonadState s m, HasRegisters s) => Word8 -> m Word8
dec v = do
    let result = v - 1

    zero .= (result == 0)
    hcarry .= (v .&. 0xF == 0)
    subOp .= True

    pure result

jr :: Bool -> (MonadState s m, HasCpu s, HasRegisters s, HasMmu s) => m ()
jr jump = do
    sb <- consumeByte
    nn <- fromIntegral <$> use pc

    if jump then do
        jmp $ fromIntegral (nn + twoCompl sb)
        tclock += 12

    else tclock += 8

cmp :: (MonadState s m, HasRegisters s) => Word8 -> m ()
cmp n = do
    a <- use (af.upperByte)

    zero .= (a == n)
    carry .= (a < n)
    hcarry .= (a .&. 0xF < n .&. 0xF)
    subOp .= True

call :: (MonadState s m, HasRegisters s, HasMmu s) => Address -> m ()
call nn = do
    pushStack =<< use pc
    jmp nn

jmp :: (MonadState s m, HasRegisters s) => Address -> m ()
jmp nn = pc .= nn

ret :: (MonadState s m, HasRegisters s, HasMmu s) => m ()
ret = jmp =<< popStack

sbc :: (MonadState s m, HasRegisters s) => Word8 -> m ()
sbc n = do
    a <- use (af.upperByte)
    c <- use carry

    let result = a - n + fromBool c

    zero .= (result == 0)
    hcarry .= (a .&. 0xF < (n .&. 0xF) + fromBool c)
    carry .= (fromIntegral a < (fromIntegral n + fromBool c :: Int))
    subOp .= True

    af.upperByte .= result

add :: (MonadState s m, HasRegisters s) => Word8 -> m ()
add n = do
    a <- use (af.upperByte)
    let result = a + n

    zero .= (result == 0)
    hcarry .= ((a .&. 0xF) + (n .&. 0xF) > 0xF)
    carry .= (toInteger a + toInteger n > 0xFF)
    subOp .= False

    af.upperByte .= result

add16 :: (MonadState s m, HasRegisters s) => Word16 -> m ()
add16 w = do
    v <- use hl

    hcarry .= ((v .&. 0x07FF) + (w .&. 0x07FF) > 0x07FF)
    carry .= (v > 0xFFFF - w)
    subOp .= False

    hl .= v + w

sub :: (MonadState s m, HasRegisters s) => Word8 -> m ()
sub n = do
    -- Subtraction in the Gameboy sets flags in the same way as comparison
    cmp n
    af.upperByte -= n

bit :: (MonadState s m, HasRegisters s) => Int -> Word8 -> m ()
bit n v = do
    zero .= (v .&. shiftL 1 n == 0)
    hcarry .= True
    subOp .= False

res :: Int -> Word8 -> Word8
res n v = v .&. (1 .<<. n)

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
swap :: (MonadState s m, HasRegisters s) => Word8 -> m Word8
swap v = do
    let result = v .>>. 4 .|. v .<<. 4;

    zero .= (result == 0)
    hcarry .= False
    carry .= False
    subOp .= False

    pure result

or :: (MonadState s m, HasRegisters s) => Word8 -> m ()
or n = do
    a <- use (af.upperByte)

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
consumeWord :: (MonadState s m, HasRegisters s, HasMmu s) => m Word16
consumeWord = do
    nn <- use pc
    pc += 2

    use (mmu.addr16 nn)

-- Read the current byte and then increase the pc register
consumeByte :: (MonadState s m, HasRegisters s, HasMmu s) => m Word8
consumeByte = do
    nn <- use pc
    pc += 1

    use (readM nn)

stackStore :: (MonadState s m, HasRegisters s) => Word8 -> m ()
stackStore v = do
  p <- use sp

  zero .= False
  subOp .= False
  hcarry .= (fromIntegral v .&. 0xF + (p .&. 0xF) > 0xF)
  carry .= (toInteger v + toInteger p > 0xFF)

  hl .= p + fromIntegral v


-- Pop 16-bit stack
popStack :: (MonadState s m, HasRegisters s, HasMmu s) => m Word16
popStack = do
    nn <- use sp
    sp += 2

    use (mmu.addr16 nn)

-- Push to the 16-bit stack
pushStack :: (MonadState s m, HasRegisters s, HasMmu s) => Word16 -> m ()
pushStack v = do
    sp -= 2
    nn <- use sp

    mmu.addr16 nn .= v
