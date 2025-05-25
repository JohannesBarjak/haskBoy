{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE GADTs           #-}

module HaskBoy.Cpu
  ( Cpu(..)
  , HasCpu(..)
  , upperByte, lowerByte
  , zero, subOp, hcarry, carry
  , Registers(..)
  , HasRegisters(..)
  , twoCompl
  , newCpu
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

makeClassy ''Cpu
makeClassy ''Registers

instance HasRegisters Cpu where
  registers = register

newCpu :: Cpu
newCpu = Cpu
  { _register = Registers
    { _af = 0x01B0
    , _bc = 0x0013
    , _de = 0x00D8
    , _hl = 0x014D
    , _pc = 0x0100
    , _sp = 0xFFFE
    }
  , _interruptEnable = True
  , _tclock = 0
  }

upperByte :: Lens' Word16 Word8
upperByte = lens (fromIntegral . (`shiftR` 8)) setUpperByte
  where setUpperByte w v = (fromIntegral v `shiftL` 8) .|. (w .&. 0xFF)

lowerByte :: Lens' Word16 Word8
lowerByte = lens (fromIntegral . (.&. 0x00FF)) setLowerByte
  where setLowerByte w v = fromIntegral v .|. (w .&. 0xFF00)

zero ,subOp ,hcarry ,carry :: HasRegisters s =>  Lens' s Bool
zero = af.lowerByte.bit 7
subOp = af.lowerByte.bit 6
hcarry = af.lowerByte.bit 5
carry = af.lowerByte.bit 4

-- | Convert byte into a signed 'Int' using two's complement
twoCompl :: Word8 -> Int
twoCompl r8
  | r8 < 128  = fromIntegral r8
  | otherwise = -(256 - fromIntegral r8)
