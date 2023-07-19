{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE GADTs           #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RankNTypes      #-}

module Cpu
    ( Register(..)
    , af, bc, de, hl, sp, pc
    , a, flag, b, c, d, e, h, l
    , zero, subOp, hcarry, carry
    , Cpu(..)
    , register
    , tclock
    , twoCompl
    ) where

import Data.Word (Word8, Word16)
import Control.Lens
import Data.Bits (Bits(shiftL, shiftR, complement), (.|.), (.&.))

import Foreign.Marshal (toBool)

data Register = Register
    { _af :: Word16
    , _bc :: Word16
    , _de :: Word16
    , _hl :: Word16
    , _sp :: Word16
    , _pc :: Word16
    }

data Cpu = Cpu
    { _register :: Register
    , _tclock   :: Integer
    }

makeLenses ''Register
makeLenses ''Cpu

a :: Lens' Register Word8
a = lens _a (\reg v -> reg&af %~ setUpperByte v)

flag :: Lens' Register Word8
flag = lens _flag (\regs@Register{_af} v -> regs { _af = fromIntegral v .|. (_af .&. 0xFF00) })

b :: Lens' Register Word8
b = lens _b (\reg v -> reg&bc %~ setUpperByte v)

c :: Lens' Register Word8
c = lens _c (\reg v -> reg&bc %~ setLowerByte v)

d :: Lens' Register Word8
d = lens _d (\reg v -> reg&de %~ setUpperByte v)

e :: Lens' Register Word8
e = lens _e (\reg v -> reg&de %~ setLowerByte v)

h :: Lens' Register Word8
h = lens _h (\reg v -> reg&hl %~ setUpperByte v)

l :: Lens' Register Word8
l = lens _l (\reg v -> reg&hl %~ setLowerByte v)

setUpperByte :: Word8 -> Word16 -> Word16
setUpperByte v w = (fromIntegral v `shiftL` 8) .|. (w .&. 0xFF)

setLowerByte :: Word8 -> Word16 -> Word16
setLowerByte v w = fromIntegral v .|. (w .&. 0xFF00)

_a :: Register -> Word8
_a Register{_af} = fromIntegral $ _af `shiftR` 8

_flag :: Register -> Word8
_flag Register{_af} = fromIntegral $ _af .&. 0x00FF

_b :: Register -> Word8
_b Register{_bc} = fromIntegral $ _bc `shiftR` 8

_c :: Register -> Word8
_c Register{_bc} = fromIntegral $ _bc .&. 0x00FF

_d :: Register -> Word8
_d Register{_de} = fromIntegral $ _de `shiftR` 8

_e :: Register -> Word8
_e Register{_de} = fromIntegral $ _de .&. 0x00FF

_h :: Register -> Word8
_h Register{_hl} = fromIntegral $ _hl `shiftR` 8

_l :: Register -> Word8
_l Register{_hl} = fromIntegral $ _hl .&. 0x00FF

zero :: Lens' Register Bool
zero = lens (`readBit` 7) (`assignBit` 7)

subOp :: Lens' Register Bool
subOp = lens (`readBit` 6) (`assignBit` 6)

hcarry :: Lens' Register Bool
hcarry = lens (`readBit` 5) (`assignBit` 5)

carry :: Lens' Register Bool
carry = lens (`readBit` 4) (`assignBit` 4)

-- Read bit in flag register
readBit :: Register -> Int -> Bool
readBit r i = toBool $ (r^.flag `shiftR` i) .&. 1

-- Assign bit in flag register
assignBit :: Register -> Int -> Bool -> Register
assignBit r i True  = r&flag .~ r^.flag .|. (1 `shiftL` i)
assignBit r i False = r&flag .~ r^.flag .&. complement (1 `shiftL` i)

-- Two's complement is used in some instructions to get signed args
twoCompl :: Word8 -> Int
twoCompl r8
    | r8 < 128  = fromIntegral r8
    | otherwise = -(256 - fromIntegral r8)
