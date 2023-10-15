{-# LANGUAGE RankNTypes #-}

module HaskBoy.BitOps where

import Data.Word
import Data.Bits

import Control.Lens
import Foreign.Marshal (toBool)

bit :: Int -> Lens' Word8 Bool
bit i = lens (`takeBit` i) (`assignBit` i)

takeBit :: Word8 -> Int -> Bool
takeBit v i = toBool (shiftR v i .&. 1)

assignBit :: Word8 -> Int -> Bool -> Word8
assignBit v i True  = v .|. (1 `shiftL` i)
assignBit v i False = v .&. complement (1 `shiftL` i)
