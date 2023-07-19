{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes      #-}

module Mmu
    ( Address
    , Mmu(..)
    , addr, raw
    , addr16
    , readByte, writeByte
    , readWord, writeWord
    ) where

import Control.Lens

import Control.Monad.State.Strict

import Data.Vector (Vector)
import Data.Word (Word8, Word16)
import Data.Bits

import Debug.Trace (trace)
import Numeric (showHex)

import Data.Ix (Ix(inRange))

data Mmu = Mmu
    { _rom0  :: Vector Word8
    , _rom1  :: Vector Word8
    , _vram  :: Vector Word8
    , _eram  :: Vector Word8
    , _wram0 :: Vector Word8
    , _wram1 :: Vector Word8
    , _oam   :: Vector Word8
    , _ioreg :: Vector Word8
    , _hram  :: Vector Word8
    , _ie    :: Word8
    }

type Address = Word16

data AddrType
    = Bank0 Int
    | Bank1 Int
    | VRam Int
    | ERam Int
    | WRam0 Int
    | WRam1 Int
    | EWRam0 Int
    | EWRam1 Int
    | OAM Int
    | NoUse Int
    | IOReg Int
    | HRam Int
    | Ie

makeLenses ''Mmu

addr :: Address -> Lens' Mmu Word8
addr i = lens (readByte' i) (flip $ writeByte' i)

addr16 :: Address -> Lens' Mmu Word16
addr16 i = lens (readWord' i) (flip $ writeWord' i)

raw :: Address -> Lens' Mmu Word8
raw idx = lens (readByte' idx) $ \mem v -> case addrType idx of
    Bank0  i -> mem&rom0.ix i .~ v
    Bank1  i -> mem&rom1.ix i .~ v
    VRam   i -> mem&vram.ix i .~ v
    ERam   i -> mem&eram.ix i .~ v
    WRam0  i -> mem&wram0.ix i .~ v
    WRam1  i -> mem&wram1.ix i .~ v
    EWRam0 i -> mem&wram0.ix i .~ v
    EWRam1 i -> mem&wram1.ix i .~ v
    OAM    i -> mem&oam.ix i .~ v
    NoUse  _ -> mem
    IOReg  i -> mem&ioreg.ix i .~ v
    HRam   i -> mem&oam.ix i .~ v
    Ie       -> mem&ie .~ v

readByte :: Address -> State Mmu Word8
readByte idx = readByte' idx <$> get

writeByte :: Address -> Word8 -> State Mmu ()
writeByte idx v = modify $ writeByte' idx v

readWord :: Address -> State Mmu Word16
readWord i = do
    ub <- readByte (i + 1)
    lb <- readByte i

    pure $ fromIntegral ub `shiftL` 8 .|. fromIntegral lb

readWord' :: Address -> Mmu -> Word16
readWord' i mmu' = fromIntegral ub `shiftL` 8 .|. fromIntegral lb
    where ub = readByte' (i + 1) mmu'
          lb = readByte' i mmu'

writeWord :: Address -> Word16 -> State Mmu ()
writeWord i v = do
    writeByte (i + 1) ub
    writeByte i lb
    where (ub, lb) = (fromIntegral $ v `shiftR` 8, fromIntegral $ v .&. 0xFF)

writeWord' :: Address -> Word16 -> Mmu -> Mmu
writeWord' i v mmu' = writeByte' i lb $ writeByte' (i + 1) ub mmu'
    where ub = fromIntegral $ v `shiftR` 8
          lb = fromIntegral $ v .&. 0xFF

-- Map an address to the appropriate memory type
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

readByte' :: Address -> Mmu -> Word8
readByte' idx mem = do
    case addrType idx of
        Bank0  i -> mem^?!rom0.ix i
        Bank1  i -> mem^?!rom1.ix i
        VRam   i -> mem^?!vram.ix i
        ERam   i -> mem^?!eram.ix i
        WRam0  i -> mem^?!wram0.ix i
        WRam1  i -> mem^?!wram1.ix i
        EWRam0 i -> mem^?!wram0.ix i
        EWRam1 i -> mem^?!wram1.ix i
        OAM    i -> mem^?!oam.ix i
        NoUse  _ -> 0xFF
        IOReg  i -> mem^?!ioreg.ix i
        HRam   i -> mem^?!hram.ix i
        Ie       -> mem^?!ie

writeByte' :: Address -> Word8 -> Mmu -> Mmu
writeByte' idx v mem = do
    case addrType idx of
        Bank0  _ -> trace (invalidWriteMessage "Bank0") mem
        Bank1  _ -> trace (invalidWriteMessage "Bank1") mem
        VRam   i -> mem&vram.ix i .~ v
        ERam   i -> mem&eram.ix i .~ v
        WRam0  i -> mem&wram0.ix i .~ v
        WRam1  i -> mem&wram1.ix i .~ v
        EWRam0 _ -> trace (invalidWriteMessage "Echo WRam0") mem
        EWRam1 _ -> trace (invalidWriteMessage "Echo WRam1") mem
        OAM    _ -> trace (invalidWriteMessage "OAM") mem
        NoUse  _ -> mem
        IOReg  i -> mem&ioreg.ix i .~ v
        HRam   i -> mem&hram.ix i .~ v
        Ie       -> mem&ie .~ v
    where invalidWriteMessage tp = "Invalid write to " ++ tp ++ " Address: " ++ showHex idx ""
