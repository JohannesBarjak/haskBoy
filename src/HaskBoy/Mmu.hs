{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes      #-}

module HaskBoy.Mmu
    ( Address
    , Mmu(..)
    , rom0, rom1
    , vram, eram, wram0, wram1
    , oam, ioreg, hram, ie
    , addr, addr16
    , ObjAttr(..)
    , yPos, xPos, tlIdx
    ) where

import Control.Lens

import Data.Sequence
import Data.Word (Word8, Word16)
import Data.Bits

import Data.Ix (Ix(inRange))

data Mmu = Mmu
    { _rom0  :: !(Seq Word8)
    , _rom1  :: !(Seq Word8)
    , _vram  :: !(Seq Word8)
    , _eram  :: !(Seq Word8)
    , _wram0 :: !(Seq Word8)
    , _wram1 :: !(Seq Word8)
    , _oam   :: !(Seq ObjAttr)
    , _ioreg :: !(Seq Word8)
    , _hram  :: !(Seq Word8)
    , _ie    :: !Word8
    }

data ObjAttr = ObjAttr
    { _yPos  :: !Word8
    , _xPos  :: !Word8
    , _tlIdx :: !Word8
    }

type Address = Word16

makeLenses ''Mmu
makeLenses ''ObjAttr

-- | Restricted access to the 'Mmu'
addr :: Address -> Lens' Mmu Word8
addr i = lens (readByte i) (flip $ writeByte i)

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
    | inRange (0x0000, 0x3FFF) i = mem^?!rom0.ix i
    | inRange (0x4000, 0x7FFF) i = mem^?!rom1.ix (fromIntegral i - 0x4000)
    | inRange (0x8000, 0x9FFF) i = mem^?!vram.ix (fromIntegral i - 0x8000)
    | inRange (0xA000, 0xBFFF) i = mem^?!eram.ix (fromIntegral i - 0xA000)
    | inRange (0xC000, 0xCFFF) i = mem^?!wram0.ix (fromIntegral i - 0xC000)
    | inRange (0xD000, 0xDFFF) i = mem^?!wram1.ix (fromIntegral i - 0xD000)
    | inRange (0xE000, 0xEFFF) i = mem^?!wram0.ix (fromIntegral i - 0xE000)
    | inRange (0xF000, 0xFDFF) i = mem^?!wram1.ix (fromIntegral i - 0xF000)
    | inRange (0xFE00, 0xFE9F) i = readOam (mem^.oam) (fromIntegral i - 0xFE00)
    | inRange (0xFEA0, 0xFEFF) i = 0xFF
    | inRange (0xFF00, 0xFF7F) i = mem^?!ioreg.ix (fromIntegral i - 0xFF00)
    | inRange (0xFF80, 0xFFFE) i = mem^?!hram.ix (fromIntegral i - 0xFF80)
    | otherwise                  = mem^?!ie

readOam :: Seq ObjAttr -> Int -> Word8
readOam mem av = extractByte oai $ mem^?!ix idx
    where idx = fromIntegral $ av `rem` 40
          oai = av `rem` 4

          extractByte 0 (ObjAttr y _  _) = y
          extractByte 1 (ObjAttr _ x  _) = x
          extractByte 2 (ObjAttr _ _ tI) = tI
          extractByte _ _ = undefined

writeOam :: Int -> Word8 -> Seq ObjAttr -> Seq ObjAttr
writeOam av v mem = case oai of
        0 -> mem&ix idx.yPos .~ v
        1 -> mem&ix idx.xPos .~ v
        2 -> mem&ix idx.tlIdx .~ v
        _ -> undefined

    where idx = fromIntegral $ av `rem` 40
          oai = av `rem` 4

writeByte :: Address -> Word8 -> Mmu -> Mmu
writeByte i v mem
    | inRange (0x0000, 0x3FFF) i = mem
    | inRange (0x4000, 0x7FFF) i = mem
    | inRange (0x8000, 0x9FFF) i = mem&vram.ix (fromIntegral i - 0x8000) .~ v
    | inRange (0xA000, 0xBFFF) i = mem&eram.ix (fromIntegral i - 0xA000) .~ v
    | inRange (0xC000, 0xCFFF) i = mem&wram0.ix (fromIntegral i - 0xC000) .~ v
    | inRange (0xD000, 0xDFFF) i = mem&wram1.ix (fromIntegral i - 0xD000) .~ v
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
