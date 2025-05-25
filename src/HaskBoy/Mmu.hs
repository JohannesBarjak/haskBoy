{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}

module HaskBoy.Mmu
  ( Address
  , Mmu(..)
  , HasMmu(..)
  , addr16, raw
  , readM, writeM
  , ObjAttr(..)
  , yPos, xPos, tlIdx
  , toMemory
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
  , _ioreg :: !(Seq Word8)
  , _hram  :: !(Seq Word8)
  , _ie    :: !Word8
  }

data ObjAttr = ObjAttr
  { _yPos    :: !Word8
  , _xPos    :: !Word8
  , _tlIdx   :: !Word8
  , _objAttr :: !Word8
  }

type Address = Word16

makeClassy ''Mmu
makeLenses ''ObjAttr

toMemory :: [Word8] -> Maybe Mmu
toMemory xs = do
  let _rom   = Seq.fromList xs
  let _vram  = Seq.replicate 0x2000 0
  let _eram  = Seq.replicate 0x2000 0
  let _wram  = Seq.replicate 0x2000 0
  let _oam   = Seq.replicate 40 (ObjAttr 0 0 0 0)
  let _ioreg = Seq.replicate 0x80 0
  let _hram  = Seq.replicate 0x7F 0
  let _ie    = 0

  Just $ Mmu {..}

readM :: HasMmu s => Address -> Getter s Word8
readM a = to writeMmu
  where writeMmu mem
          | inRange (0x0000, 0x7FFF) a = Seq.index (mem^.rom) (fromIntegral a)
          | inRange (0x8000, 0x9FFF) a = Seq.index (mem^.vram) (fromIntegral a - 0x8000)
          | inRange (0xA000, 0xBFFF) a = Seq.index (mem^.eram) (fromIntegral a - 0xA000)
          | inRange (0xC000, 0xCFFF) a = Seq.index (mem^.wram) (fromIntegral a - 0xC000)
          | inRange (0xD000, 0xDFFF) a = Seq.index (mem^.wram) (fromIntegral a - 0xC000)
          | inRange (0xE000, 0xEFFF) a = Seq.index (mem^.wram) (fromIntegral a - 0xE000)
          | inRange (0xF000, 0xFDFF) a = Seq.index (mem^.wram) (fromIntegral a - 0xE000)
          | inRange (0xFE00, 0xFE9F) a = readOam (mem^.oam) (fromIntegral a - 0xFE00)
          | inRange (0xFEA0, 0xFEFF) a = 0xFF
          | inRange (0xFF00, 0xFF7F) a = Seq.index (mem^.ioreg) (fromIntegral a - 0xFF00)
          | inRange (0xFF80, 0xFFFE) a = Seq.index (mem^.hram) (fromIntegral a - 0xFF80)
          | otherwise                  = mem^.ie

writeM :: HasMmu s => Address -> Setter' s Word8
writeM a = sets writeMmu
  where writeMmu f mem
          | inRange (0x0000, 0x7FFF) a = mem
          | inRange (0x8000, 0x9FFF) a = mem&vram .~ Seq.adjust' f (fromIntegral a - 0x8000) (mem^.vram)
          | inRange (0xA000, 0xBFFF) a = mem&eram .~ Seq.adjust' f (fromIntegral a - 0xA000) (mem^.eram)
          | inRange (0xC000, 0xCFFF) a = mem&wram .~ Seq.adjust' f (fromIntegral a - 0xC000) (mem^.wram)
          | inRange (0xD000, 0xDFFF) a = mem&wram .~ Seq.adjust' f (fromIntegral a - 0xC000) (mem^.wram)
          | inRange (0xE000, 0xEFFF) a = mem
          | inRange (0xF000, 0xFDFF) a = mem
          | inRange (0xFE00, 0xFE9F) a = let idx = fromIntegral a - 0xFE00 in
                                          mem&oam .~ Seq.adjust' (adjustOam idx f) (idx `rem` 40) (mem^.oam)
          | inRange (0xFEA0, 0xFEFF) a = mem
          | inRange (0xFF00, 0xFF7F) a = mem&ioreg .~ Seq.adjust' f (fromIntegral a - 0xFF00) (mem^.ioreg)
          | inRange (0xFF80, 0xFFFE) a = mem&hram .~ Seq.adjust' f (fromIntegral a - 0xFF80) (mem^.hram)
          | otherwise                  = mem&ie %~ f

raw :: HasMmu s => Address -> Lens' s Word8
raw i = lens readMmu writeMmu
  where readMmu mem
          | inRange (0x0000, 0x7FFF) i = mem^?!rom.ix (fromIntegral i)
          | inRange (0x8000, 0x9FFF) i = mem^?!vram.ix (fromIntegral i - 0x8000)
          | inRange (0xA000, 0xBFFF) i = mem^?!eram.ix (fromIntegral i - 0xA000)
          | inRange (0xC000, 0xCFFF) i = mem^?!wram.ix (fromIntegral i - 0xC000)
          | inRange (0xD000, 0xDFFF) i = mem^?!wram.ix (fromIntegral i - 0xC000)
          | inRange (0xE000, 0xEFFF) i = mem^?!wram.ix (fromIntegral i - 0xE000)
          | inRange (0xF000, 0xFDFF) i = mem^?!wram.ix (fromIntegral i - 0xE000)
          | inRange (0xFE00, 0xFE9F) i = readOam (mem^.oam) (fromIntegral i - 0xFE00)
          | inRange (0xFEA0, 0xFEFF) i = 0xFF
          | inRange (0xFF00, 0xFF7F) i = mem^?!ioreg.ix (fromIntegral i - 0xFF00)
          | inRange (0xFF80, 0xFFFE) i = mem^?!hram.ix (fromIntegral i - 0xFF80)
          | otherwise                  = mem^?!ie

        writeMmu mem v
          | inRange (0x0000, 0x3FFF) i = mem
          | inRange (0x4000, 0x7FFF) i = mem
          | inRange (0x8000, 0x9FFF) i = mem&vram.ix (fromIntegral i - 0x8000) .~ v
          | inRange (0xA000, 0xBFFF) i = mem&eram.ix (fromIntegral i - 0xA000) .~ v
          | inRange (0xC000, 0xCFFF) i = mem&wram.ix (fromIntegral i - 0xC000) .~ v
          | inRange (0xD000, 0xDFFF) i = mem&wram.ix (fromIntegral i - 0xC000) .~ v
          | inRange (0xE000, 0xEFFF) i = mem
          | inRange (0xF000, 0xFDFF) i = mem
          | inRange (0xFE00, 0xFE9F) i = mem&oam %~ writeOam (fromIntegral i - 0xFE00) v
          | inRange (0xFEA0, 0xFEFF) i = mem
          | inRange (0xFF00, 0xFF7F) i = mem&ioreg.ix (fromIntegral i - 0xFF00) .~ v
          | inRange (0xFF80, 0xFFFE) i = mem&hram.ix (fromIntegral i - 0xFF80) .~ v
          | otherwise                  = mem&ie .~ v

-- | Provides restricted access to a Word in the 'Mmu'.
-- The Word is created by a pair of bytes in little endian order.
addr16 :: Address -> Lens' Mmu Word16
addr16 i = lens (readWord i) (flip $ writeWord i)

readWord :: Address -> Mmu -> Word16
readWord i mmu' = fromIntegral ub `shiftL` 8 .|. fromIntegral lb
  where ub = readByte (i + 1) mmu'
        lb = readByte i mmu'

writeWord :: Address -> Word16 -> Mmu -> Mmu
writeWord i v mmu' = writeByte i lb $ writeByte (i + 1) ub mmu'
  where ub = fromIntegral $ v `shiftR` 8
        lb = fromIntegral $ v .&. 0xFF

readByte :: Address -> Mmu -> Word8
readByte i mem
  | inRange (0x0000, 0x7FFF) i = mem^?!rom.ix (fromIntegral i)
  | inRange (0x8000, 0x9FFF) i = mem^?!vram.ix (fromIntegral i - 0x8000)
  | inRange (0xA000, 0xBFFF) i = mem^?!eram.ix (fromIntegral i - 0xA000)
  | inRange (0xC000, 0xCFFF) i = mem^?!wram.ix (fromIntegral i - 0xC000)
  | inRange (0xD000, 0xDFFF) i = mem^?!wram.ix (fromIntegral i - 0xC000)
  | inRange (0xE000, 0xEFFF) i = mem^?!wram.ix (fromIntegral i - 0xE000)
  | inRange (0xF000, 0xFDFF) i = mem^?!wram.ix (fromIntegral i - 0xE000)
  | inRange (0xFE00, 0xFE9F) i = readOam (mem^.oam) (fromIntegral i - 0xFE00)
  | inRange (0xFEA0, 0xFEFF) i = 0xFF
  | i == 0xFF00 = 0xCF
  | inRange (0xFF00, 0xFF7F) i = mem^?!ioreg.ix (fromIntegral i - 0xFF00)
  | inRange (0xFF80, 0xFFFE) i = mem^?!hram.ix (fromIntegral i - 0xFF80)
  | otherwise                  = mem^?!ie

readOam :: Seq ObjAttr -> Int -> Word8
readOam mem av = extractByte $ mem^?!ix idx
  where idx = fromIntegral $ av `rem` 40

        extractByte = view $ [yPos, xPos, tlIdx, objAttr] !! oai
        oai = av `rem` 4

writeOam :: Int -> Word8 -> Seq ObjAttr -> Seq ObjAttr
writeOam av v mem = case oai of
  0 -> mem&ix idx.yPos .~ v
  1 -> mem&ix idx.xPos .~ v
  2 -> mem&ix idx.tlIdx .~ v
  3 -> mem&ix idx.objAttr .~ v
  _ -> error "Invalid argument for writeOam"

  where idx = fromIntegral $ av `rem` 40
        oai = av `rem` 4

adjustOam :: Int -> (Word8 -> Word8) -> ObjAttr -> ObjAttr
adjustOam a f obj = case oai of
  0 -> obj&yPos %~ f
  1 -> obj&xPos %~ f
  2 -> obj&tlIdx %~ f
  3 -> obj&objAttr %~ f
  _ -> error "Invalid argument for writeOam"

  where oai = a `rem` 4

writeByte :: Address -> Word8 -> Mmu -> Mmu
writeByte i v mem
  | inRange (0x0000, 0x3FFF) i = mem
  | inRange (0x4000, 0x7FFF) i = mem
  | inRange (0x8000, 0x9FFF) i = mem&vram.ix (fromIntegral i - 0x8000) .~ v
  | inRange (0xA000, 0xBFFF) i = mem&eram.ix (fromIntegral i - 0xA000) .~ v
  | inRange (0xC000, 0xCFFF) i = mem&wram.ix (fromIntegral i - 0xC000) .~ v
  | inRange (0xD000, 0xDFFF) i = mem&wram.ix (fromIntegral i - 0xC000) .~ v
  | inRange (0xE000, 0xEFFF) i = mem
  | inRange (0xF000, 0xFDFF) i = mem
  | inRange (0xFE00, 0xFE9F) i = mem&oam %~ writeOam (fromIntegral i - 0xFE00) v
  | inRange (0xFEA0, 0xFEFF) i = mem
  | inRange (0xFF00, 0xFF7F) i = let rdOnly = [0x44] in
      if i `notElem` rdOnly then
        mem&ioreg.ix (fromIntegral i - 0xFF00) .~ v
      else mem

  | inRange (0xFF80, 0xFFFE) i = mem&hram.ix (fromIntegral i - 0xFF80) .~ v
  | otherwise                  = mem&ie .~ v

objPri, yFlip, xFlip, dmgPal :: Lens' ObjAttr Bool
objPri = lens (^.objAttr.bit 7) (\obj v -> obj&objAttr.bit 7 .~ v)
yFlip  = lens (^.objAttr.bit 6) (\obj v -> obj&objAttr.bit 6 .~ v)
xFlip  = lens (^.objAttr.bit 5) (\obj v -> obj&objAttr.bit 5 .~ v)
dmgPal = lens (^.objAttr.bit 4) (\obj v -> obj&objAttr.bit 4 .~ v)
