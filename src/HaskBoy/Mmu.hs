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

data AddrType
    = Bank0 !Int
    | Bank1 !Int
    | VRam !Int
    | ERam !Int
    | WRam0 !Int
    | WRam1 !Int
    | EWRam0 !Int
    | EWRam1 !Int
    | OAM !Int
    | NoUse !Int
    | IOReg !Int
    | HRam !Int
    | Ie

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

-- Map an address to the appropriate memory type.
addrType :: Address -> AddrType
addrType i
    | inRange (0x0000, 0x3FFF) i = Bank0  $ fromIntegral i
    | inRange (0x4000, 0x7FFF) i = Bank1  $ fromIntegral i - 0x4000
    | inRange (0x8000, 0x9FFF) i = VRam   $ fromIntegral i - 0x8000
    | inRange (0xA000, 0xBFFF) i = ERam   $ fromIntegral i - 0xA000
    | inRange (0xC000, 0xCFFF) i = WRam0  $ fromIntegral i - 0xC000
    | inRange (0xD000, 0xDFFF) i = WRam1  $ fromIntegral i - 0xD000
    | inRange (0xE000, 0xEFFF) i = EWRam0 $ fromIntegral i - 0xE000
    | inRange (0xF000, 0xFDFF) i = EWRam1 $ fromIntegral i - 0xF000
    | inRange (0xFE00, 0xFE9F) i = OAM    $ fromIntegral i - 0xFE00
    | inRange (0xFEA0, 0xFEFF) i = NoUse  $ fromIntegral i - 0xFEA0
    | inRange (0xFF00, 0xFF7F) i = IOReg  $ fromIntegral i - 0xFF00
    | inRange (0xFF80, 0xFFFE) i = HRam   $ fromIntegral i - 0xFF80
    | otherwise                  = Ie

readByte :: Address -> Mmu -> Word8
readByte idx mem = do
    case addrType idx of
        Bank0  i -> mem^?!rom0.ix i
        Bank1  i -> mem^?!rom1.ix i
        VRam   i -> mem^?!vram.ix i
        ERam   i -> mem^?!eram.ix i
        WRam0  i -> mem^?!wram0.ix i
        WRam1  i -> mem^?!wram1.ix i
        EWRam0 i -> mem^?!wram0.ix i
        EWRam1 i -> mem^?!wram1.ix i
        OAM    i -> readOam (mem^.oam) i
        NoUse  _ -> 0xFF
        IOReg  i -> mem^?!ioreg.ix i
        HRam   i -> mem^?!hram.ix i
        Ie       -> mem^?!ie

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
writeByte idx v mem = do
    case addrType idx of
        Bank0  _ -> mem
        Bank1  _ -> mem
        VRam   i -> mem&vram.ix i .~ v
        ERam   i -> mem&eram.ix i .~ v
        WRam0  i -> mem&wram0.ix i .~ v
        WRam1  i -> mem&wram1.ix i .~ v
        EWRam0 _ -> mem
        EWRam1 _ -> mem
        OAM    i -> mem&oam %~ writeOam i v
        NoUse  _ -> mem
        IOReg  i -> let rdOnly = [0x44] in
            if i `notElem` rdOnly then 
                mem&ioreg.ix i .~ v
            else mem

        HRam   i -> mem&hram.ix i .~ v
        Ie       -> mem&ie .~ v
