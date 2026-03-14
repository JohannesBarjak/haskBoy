{-# LANGUAGE TypeApplications #-}
module HaskBoy.Cpu.Instructions
  ( inc, dec
  , and, xor, or
  , jr, callC, call
  , jmp, ret
  , cmp
  , add, adc, sub, sbc
  , daa
  , add16, addi8
  , rl, sla, rr, sra
  , bit, swap
  , rocA, rotA, res
  , cpl, scf, ccf
  , consumeByte, consumeWord
  , popStack, pushStack, stackStore
  , Argument(..), Argument16(..)
  , read8, write8, read16, write16
  , mcycle
  ) where

import Control.Lens
import Control.Monad.State.Strict
import Control.Monad (when)

import Data.Bits ((.&.), (.|.), shiftL, shiftR, (.<<.), (.>>.))
import Data.Bits qualified as B

import Data.Word (Word8, Word16)
import Data.Int (Int8)

import Data.Bool (bool)
import Foreign.Marshal.Utils (fromBool, toBool)

import HaskBoy.Cpu
import HaskBoy.Mmu

import Prelude hiding (and, or)

data Argument s where
  Register :: HasRegisters s => (ALens' s Word8) -> Argument s
  Address :: Word16 -> Argument s

data Argument16 s where
  Register16 :: HasRegisters s => (ALens' s Word16) -> Argument16 s
  Address16  :: Word16 -> Argument16 s

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
  sb <- fromIntegral . (fromIntegral @_ @Int8) <$> consumeByte
  nn <- use pc

  if jump then do
    jmp $ nn + sb
    mcycle 3

  else mcycle 2

cmp :: (MonadState s m, HasRegisters s) => Word8 -> m ()
cmp n = do
  a <- use (af.upperByte)

  zero .= (a == n)
  carry .= (a < n)
  hcarry .= (a .&. 0xF < n .&. 0xF)
  subOp .= True

callC :: (MonadState s m, HasRegisters s, HasMmu s, HasCpu s) => Bool -> Address -> m ()
callC c a = do
  when c do
    mcycle 3

    pushStack =<< use (register.pc)
    jmp a

  mcycle 3

call :: (MonadState s m, HasRegisters s, HasMmu s) => Address -> m ()
call nn = do
  pushStack =<< use pc
  jmp nn

jmp :: (MonadState s m, HasRegisters s) => Address -> m ()
jmp nn = pc .= nn

ret :: (MonadState s m, HasRegisters s, HasMmu s) => m ()
ret = jmp =<< popStack

daa :: (MonadState s m, HasRegisters s) => m ()
daa = do
  a <- use (af.upperByte)

  h <- use hcarry
  c <- use carry
  s <- use subOp

  let offTens = bool 0 0x60 (not s && a > 0x99 || c)
  let offOnes = bool 0 0x06 (not s && a .&. 0xF > 0x9 || h)

  let result = if s then
        a - offOnes - offTens
      else a + offOnes + offTens

  zero .= (result == 0)
  hcarry .= False
  carry .= (offTens /= 0)

  af.upperByte .= result

sbc :: (MonadState s m, HasRegisters s) => Word8 -> m ()
sbc n = do
  a <- use (af.upperByte)
  c <- use carry

  let result = a - n - fromBool c

  zero .= (result == 0)
  hcarry .= (a .&. 0xF < (n .&. 0xF) + fromBool c)
  carry .= (fromIntegral a < (fromIntegral n + fromBool c :: Int))
  subOp .= True

  af.upperByte .= result

addi8 :: (MonadState s m, HasRegisters s) => Int8 -> m ()
addi8 i = do
  p <- use sp
  let r = fromIntegral i

  zero  .= False
  subOp .= False

  hcarry .= (p .&. 0x0F + r .&. 0x0F > 0x0F)
  carry  .= (p .&. 0xFF + r .&. 0xFF > 0xFF)

  sp .= p + r

add :: (MonadState s m, HasRegisters s) => Word8 -> m ()
add n = do
  a <- use (af.upperByte)
  let result = a + n

  zero .= (result == 0)
  hcarry .= ((a .&. 0xF) + (n .&. 0xF) > 0xF)
  carry .= (toInteger a + toInteger n > 0xFF)
  subOp .= False

  af.upperByte .= result

adc :: (MonadState s m, HasRegisters s) => Word8 -> m ()
adc n = do
  a <- use (af.upperByte)
  c <- fromBool <$> use carry
  let result = a + n + c

  zero .= (result == 0)
  hcarry .= ((a .&. 0xF) + (n .&. 0xF) + c > 0xF)
  carry .= (toInteger a + toInteger n + toInteger c > 0xFF)
  subOp .= False

  af.upperByte .= result

add16 :: (MonadState s m, HasRegisters s) => Word16 -> m ()
add16 w = do
  v <- use hl

  hcarry .= ((v .&. 0x0FFF) + (w .&. 0x0FFF) > 0x0FFF)
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

rl :: (MonadState s m, HasRegisters s, HasMmu s) => Argument s -> m ()
rl r = do
  c <- fromBool <$> use carry
  v <- use (read8 r)

  write8 r <.= shiftL v 1 + c >>= rotFlags (v .&. 0x80)

sla :: (MonadState s m, HasRegisters s, HasMmu s) => Argument s -> m ()
sla r = do
  v <- use (read8 r)
  write8 r <.= shiftL v 1 >>= rotFlags (v .&. 0x80)

rr :: (MonadState s m, HasRegisters s, HasMmu s) => Argument s -> m ()
rr r = do
  v <- use (read8 r)
  c <- fromBool <$> use carry
  let result = (shiftR v 1 .&. 0x7F) .|. shiftL c 7

  write8 r .= result
  rotFlags (v .&. 1) result

sra :: (MonadState s m, HasRegisters s, HasMmu s) => Argument s -> m ()
sra r = do
  v <- use (read8 r)
  let b7 = v .&. 0x80 in let b0 = v .&. 1 in
    write8 r <.= shiftR v 1 .|. b7 >>= rotFlags b0

rotFlags :: (HasRegisters s, MonadState s m) => Word8 -> Word8 -> m ()
rotFlags b r = do
  zero   .= (r == 0)
  subOp  .= False
  hcarry .= False
  carry  .= toBool b

rocA :: (MonadState s m, HasRegisters s) => Bool -> m ()
rocA right = do
  a <- use (af.upperByte)

  af.upperByte .= B.rotate a (bool 1 (-1) right)

  zero   .= False
  subOp  .= False
  hcarry .= False
  carry .= toBool (a .&. bool 0x80 1 right)

-- | The boolean determines whether to rotate left or right, False is left and True is right.
rotA :: (MonadState s m, HasRegisters s) => Bool -> m ()
rotA right = do
  c <- use carry
  a <- use (af.upperByte)

  zero   .= False
  subOp  .= False
  hcarry .= False

  if right then do
     carry .= toBool (a .&. 1)
     af.upperByte .= (shiftR a 1 .&. 0x7F) .|. shiftL (fromBool c) 7
   else do
     carry .= toBool (a .&. 0x80)
     af.upperByte .= (shiftL a 1 .&. 0xFE) + fromBool c

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
  let result = B.xor a n

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
  af.upperByte %= B.complement
  hcarry .= True
  subOp .= True

scf :: (MonadState s m, HasRegisters s) => m ()
scf = do
  subOp  .= False
  hcarry .= False
  carry  .= True

ccf :: (MonadState s m, HasRegisters s) => m ()
ccf = do
  subOp  .= False
  hcarry .= False
  carry  %= not

-- Function for machine cycles.
mcycle :: (MonadState s m, HasCpu s) => Integer -> m ()
mcycle v = tclock += (v * 4)

-- Read the current and following byte as a 16-bit word
-- and then increase the pc register by 2
consumeWord :: (MonadState s m, HasRegisters s, HasMmu s) => m Word16
consumeWord = do
  nn <- use pc
  pc += 2

  use (readM16 nn)

-- Read the current byte and then increase the pc register
consumeByte :: (MonadState s m, HasRegisters s, HasMmu s) => m Word8
consumeByte = do
  nn <- use pc
  pc += 1

  use (readM nn)

stackStore :: (MonadState s m, HasRegisters s) => Word8 -> m ()
stackStore v = do
  p <- use sp
  let r = fromIntegral (fromIntegral v :: Int8)

  zero .= False
  subOp .= False
  hcarry .= (r .&. 0x0F + p .&. 0x0F > 0x0F)
  carry  .= (r .&. 0xFF + p .&. 0xFF > 0xFF)

  hl .= p + r


-- Pop 16-bit stack
popStack :: (MonadState s m, HasRegisters s, HasMmu s) => m Word16
popStack = do
  nn <- use sp
  sp += 2

  use (readM16 nn)

-- Push to the 16-bit stack
pushStack :: (MonadState s m, HasRegisters s, HasMmu s) => Word16 -> m ()
pushStack v = do
  sp -= 2
  nn <- use sp

  writeM16 nn .= v

read8 :: HasMmu s => Argument s -> Getter s Word8
read8 (Register r) = cloneLens r
read8 (Address  a) = readM a

write8 :: HasMmu s => Argument s -> Setter' s Word8
write8 (Register r) = cloneLens r
write8 (Address  a) = writeM a

read16 :: HasMmu s => Argument16 s -> Getter s Word16
read16 (Register16 r) = cloneLens r
read16 (Address16  a) = readM16 a

write16 :: HasMmu s => Argument16 s -> Setter' s Word16
write16 (Register16 r) = cloneLens r
write16 (Address16  a) = writeM16 a
