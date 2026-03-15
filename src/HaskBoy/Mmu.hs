{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}

module HaskBoy.Mmu
  ( Address
  , Mmu(..)
  , HasMmu(..)
  , addr16
  , readM, writeM, readM16, writeM16
  , ObjAttr(..)
  , yPos, xPos, tlIdx
  , toMemory, Mapper(..)
  , objPri, yFlip, xFlip, dmgPal
  ) where

import Control.Lens

import Data.Sequence qualified as Seq
import Data.Sequence (Seq)

import Data.Word (Word8, Word16)
import Data.Bits (shiftL, shiftR, (.|.), (.&.))
import Data.Ix (inRange)

import HaskBoy.BitOps

data Mmu = Mmu
  { _rom   :: !(Seq Word8)
  , _vram  :: !(Seq Word8)
  , _eram  :: !(Seq Word8)
  , _wram  :: !(Seq Word8)
  , _oam   :: !(Seq ObjAttr)
  , _proh  :: !(Seq Word8)
  , _ioreg :: !(Seq Word8)
  , _hram  :: !(Seq Word8)
  , _ie    :: !Word8

  , _mapper :: !Mapper
  }

data Mapper
  = Bank0
  | RawAccess

data ObjAttr = ObjAttr
  { _yPos    :: !Word8
  , _xPos    :: !Word8
  , _tlIdx   :: !Word8
  , _objAttr :: !Word8
  }

type Address = Word16

makeClassy ''Mmu
makeLenses ''ObjAttr

toMemory :: [Word8] -> Mapper -> Maybe Mmu
toMemory xs m = do
  let _rom   = Seq.fromList xs
  let _vram  = Seq.replicate 0x2000 0
  let _eram  = Seq.replicate 0x2000 0
  let _wram  = Seq.replicate 0x2000 0
  let _oam   = Seq.replicate 40 (ObjAttr 0 0 0 0)
  let _proh  = Seq.replicate 0x60 0
  let _ioreg = Seq.replicate 0x80 0
  let _hram  = Seq.replicate 0x7F 0
  let _ie    = 0
  let _mapper = m;

  Just $ Mmu {..}

writeM :: HasMmu s => Address -> Setter' s Word8
writeM a = sets writeMmu
  where addrSpace = flip inRange a
        writeBank mem bank f start = mem&bank %~ Seq.adjust' f (fromIntegral a - start)
        writeMmu f mem
          | addrSpace (0x0000, 0x7FFF) = handleWrite mem $ writeBank mem rom f 0
          | addrSpace (0x8000, 0x9FFF) = writeBank mem vram f 0x8000
          | addrSpace (0xA000, 0xBFFF) = writeBank mem eram f 0xA000
          | addrSpace (0xC000, 0xCFFF) = writeBank mem wram f 0xC000
          | addrSpace (0xD000, 0xDFFF) = writeBank mem wram f 0xC000
          | addrSpace (0xE000, 0xEFFF) = handleWrite mem $ writeBank mem wram f 0xE000
          | addrSpace (0xF000, 0xFDFF) = handleWrite mem $ writeBank mem wram f 0xE000
          | addrSpace (0xFE00, 0xFE9F) = let idx = fromIntegral a - 0xFE00 in
                                          mem&oam %~ Seq.adjust' (adjustOam idx f) (idx `rem` 40)
          | addrSpace (0xFEA0, 0xFEFF) = handleWrite mem $ writeBank mem proh f 0xFEA0
          | addrSpace (0xFF00, 0xFF7F) = writeBank mem ioreg f 0xFF00
          | addrSpace (0xFF80, 0xFFFE) = writeBank mem hram  f 0xFF80
          | otherwise                  = mem&ie %~ f
        handleWrite mem f = case mem^.mapper of
          RawAccess -> f
          Bank0 -> mem

readM :: HasMmu s => Address -> Getter s Word8
readM a = to readMmu
  where addrSpace = flip inRange a
        readBank mem bank start = Seq.index (mem^.bank) (fromIntegral a - start)
        readMmu mem
          | addrSpace (0x0000, 0x7FFF) = readBank mem rom  0x0000
          | addrSpace (0x8000, 0x9FFF) = readBank mem vram 0x8000
          | addrSpace (0xA000, 0xBFFF) = readBank mem eram 0xA000
          | addrSpace (0xC000, 0xCFFF) = readBank mem wram 0xC000
          | addrSpace (0xD000, 0xDFFF) = readBank mem wram 0xC000
          | addrSpace (0xE000, 0xEFFF) = readBank mem wram 0xE000
          | addrSpace (0xF000, 0xFDFF) = readBank mem wram 0xE000
          | addrSpace (0xFE00, 0xFE9F) = readOam (mem^.oam) (fromIntegral a - 0xFE00)

          | addrSpace (0xFEA0, 0xFEFF) = case mem^.mapper of
                                           RawAccess -> readBank mem proh 0xFEA0
                                           Bank0 -> 0xFF

          | addrSpace (0xFF00, 0xFF7F) = readBank mem ioreg 0xFF00
          | addrSpace (0xFF80, 0xFFFE) = readBank mem hram  0xFF80
          | otherwise                  = mem^.ie

-- | Provides restricted access to a Word in the 'Mmu'.
-- The Word is created by a pair of bytes in little endian order.
addr16 :: Address -> Lens' Mmu Word16
addr16 i = lens (view $ readM16 i) (flip . set $ writeM16 i)

readM16 :: HasMmu s => Address -> Getter s Word16
readM16 a = to $ \mem ->
  let ub = fromIntegral $ mem^.readM (a + 1) in
  let lb = fromIntegral $ mem^.readM a in
    shiftL ub 8 .|. lb

writeM16 :: HasMmu s => Address -> Setter' s Word16
writeM16 a = sets $ \f mem -> let v = f a in
  let ub = fromIntegral (shiftR v 8) in
  let lb = fromIntegral (v .&. 0xFF) in
    mem&writeM (a + 1) .~ ub
       &writeM a .~ lb

readOam :: Seq ObjAttr -> Int -> Word8
readOam mem av = extractByte $ mem^?!ix idx
  where idx = fromIntegral $ av `rem` 40

        extractByte = view $ [yPos, xPos, tlIdx, objAttr] !! oai
        oai = av `rem` 4

adjustOam :: Int -> (Word8 -> Word8) -> ObjAttr -> ObjAttr
adjustOam a f obj = case oai of
  0 -> obj&yPos %~ f
  1 -> obj&xPos %~ f
  2 -> obj&tlIdx %~ f
  3 -> obj&objAttr %~ f
  _ -> error "Invalid argument for writeOam"

  where oai = a `rem` 4

objPri, yFlip, xFlip, dmgPal :: Lens' ObjAttr Bool
objPri = lens (view $ objAttr.bit 7) (flip . set $ objAttr.bit 7)
yFlip  = lens (view $ objAttr.bit 6) (flip . set $ objAttr.bit 6)
xFlip  = lens (view $ objAttr.bit 5) (flip . set $ objAttr.bit 5)
dmgPal = lens (view $ objAttr.bit 4) (flip . set $ objAttr.bit 4)
