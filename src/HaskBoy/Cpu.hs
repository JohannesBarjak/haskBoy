{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE GADTs           #-}

module HaskBoy.Cpu
    ( Registers(..)
    , af, bc, de, hl, sp, pc
    , a, flag, b, c, d, e, h, l
    , zero, subOp, hcarry, carry
    , Cpu(..)
    , register
    , interruptEnable
    , tclock
    , twoCompl
    ) where

import HaskBoy.BitOps

import Data.Word (Word8, Word16)
import Control.Lens
import Data.Bits (shiftL, shiftR, (.|.), (.&.))

data Cpu = Cpu
    { _register        :: !Registers
    , _interruptEnable :: !Bool
    , _tclock          :: !Integer -- ^ The Cpu clock uses tcycles
    }

-- | Store 16bit registers
data Registers = Registers
    { _af :: !Word16
    , _bc :: !Word16
    , _de :: !Word16
    , _hl :: !Word16
    , _sp :: !Word16
    , _pc :: !Word16
    }

makeLenses ''Cpu
makeLenses ''Registers

a, flag, b, c, d, e, h, l :: Lens' Registers Word8

a = lens _a (\reg v -> reg&af %~ setUpperByte v)
flag = lens _flag (\reg v -> reg&af %~ setLowerByte v)
b = lens _b (\reg v -> reg&bc %~ setUpperByte v)
c = lens _c (\reg v -> reg&bc %~ setLowerByte v)
d = lens _d (\reg v -> reg&de %~ setUpperByte v)
e = lens _e (\reg v -> reg&de %~ setLowerByte v)
h = lens _h (\reg v -> reg&hl %~ setUpperByte v)
l = lens _l (\reg v -> reg&hl %~ setLowerByte v)

_a ,_flag ,_b ,_c ,_d ,_e ,_h ,_l :: Registers -> Word8

_a Registers{_af} = fromIntegral $ _af `shiftR` 8
_flag Registers{_af} = fromIntegral $ _af .&. 0x00FF
_b Registers{_bc} = fromIntegral $ _bc `shiftR` 8
_c Registers{_bc} = fromIntegral $ _bc .&. 0x00FF
_d Registers{_de} = fromIntegral $ _de `shiftR` 8
_e Registers{_de} = fromIntegral $ _de .&. 0x00FF
_h Registers{_hl} = fromIntegral $ _hl `shiftR` 8
_l Registers{_hl} = fromIntegral $ _hl .&. 0x00FF

setUpperByte :: Word8 -> Word16 -> Word16
setUpperByte v w = (fromIntegral v `shiftL` 8) .|. (w .&. 0xFF)

setLowerByte :: Word8 -> Word16 -> Word16
setLowerByte v w = fromIntegral v .|. (w .&. 0xFF00)

zero ,subOp ,hcarry ,carry :: Lens' Registers Bool
zero = flag.bit 7
subOp = flag.bit 6
hcarry = flag.bit 5
carry = flag.bit 4

-- | Convert byte into a signed 'Int' using two's complement
twoCompl :: Word8 -> Int
twoCompl r8
    | r8 < 128  = fromIntegral r8
    | otherwise = -(256 - fromIntegral r8)
