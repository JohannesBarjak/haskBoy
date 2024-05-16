{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE OverloadedLists #-}

module HaskBoy.Ppu.Execution
    ( drawTiles
    , bgScanline
    , bgTileMaps
    , getTileRow, tileRow
    , scx, scy
    , ly, lyc
    , ppuMode
    ) where

import Control.Lens
import Control.Monad (forM_, join)
import Control.Monad.State.Strict

import Data.Bits ((.&.), shiftR, (.|.))
import Data.Bool (bool)
import Data.Ix (inRange)
import Data.Maybe (fromMaybe)

import Data.Sequence (Seq)
import Data.Sequence qualified as Seq

import Data.Word (Word8)
import Foreign.Marshal (toBool)

import HaskBoy.BitOps
import HaskBoy.Emulator
import HaskBoy.Mmu
import HaskBoy.Ppu

-- TODO: Implement scanline wraparound.

drawTiles :: State Emulator ()
drawTiles = do
    lineY <- fromIntegral <$> use (mmu.ly)
    ppu.display.ix lineY <~ zoom mmu bgScanline
    drawSprites

-- TODO: Implement 40 sprite limit.
drawSprites :: State Emulator ()
drawSprites = do
    size <- bool 8 16 <$> use (mmu.objSize)
    lineY <- use (mmu.ly)

    let visibleY obj = inRange ((lineY + 1 - size, lineY)&both +~ 16) (obj^.yPos)

    -- Only Y visibility affects the maximum of 10
    -- sprite objects per scanline limit.
    scanAttrs <- Seq.take 10 . Seq.filter visibleY <$> use (mmu.oam)

    tileAddrMode <- bool 0xFF 0xFE <$> use (mmu.objSize)
    let getTileAddr v = 0x8000 + (fromIntegral (v .&. tileAddrMode) * 16)

    forM_ scanAttrs $ \obj -> do
        let rowIndex = size - (obj^.yPos - lineY + size - 16)
        let tileIndex = getTileAddr (obj^.tlIdx)

        spriteRow <- zoom mmu $ getTileRow rowIndex tileIndex

        let writeSprite i v = fromMaybe v $
                Seq.lookup (i - fromIntegral (obj^.xPos) + 8) spriteRow

        ppu.display.ix (fromIntegral lineY) %= Seq.mapWithIndex writeSprite

bgScanline :: State Mmu (Seq Pixel)
bgScanline = do
    y <- liftA2 (+) (use ly) (use scy)
    let (tileIndex, rowIndex) = (y `quotRem` 8)&both %~ fromIntegral

    btd <- use bgTileData
    bgScan <- bgTileMaps tileIndex >>= traverse (getTileRow rowIndex . tileAddress btd)

    scrollX <- use scx
    pure $ Seq.cycleTaking 160 $ Seq.drop (fromIntegral scrollX) (join bgScan)

    where tileAddress True  idx = 0x8000 + (fromIntegral idx * 16)
          tileAddress False idx = 0x9000 + (fromIntegral (twoCompl idx) * 16)

bgTileMaps :: Word8 -> State Mmu (Seq Word8)
bgTileMaps tI = go 32 tileIndex []
    where tileIndex = 0x9800 + fromIntegral tI * 32

          go :: Int -> Address -> Seq Word8 -> State Mmu (Seq Word8)
          go 0 idx ts = (ts |>) <$> use (cloneLens $ addr idx)
          go i idx ts = do
            !t <- use (cloneLens $ addr idx)
            go (i - 1) (idx + 1) (ts |> t)

getTileRow :: Word8 -> Address -> State Mmu (Seq Pixel)
getTileRow ri ta
    = let ra = ta + (fromIntegral ri * 2) in tileRow
        <$> use (cloneLens $ addr ra)
        <*> use (cloneLens $ addr (ra + 1))

twoCompl :: Word8 -> Int
twoCompl r8
    | r8 < 128  = fromIntegral r8
    | otherwise = -(256 - fromIntegral r8)

-- | Get a single tile row from a pair of bytes
tileRow :: Word8 -> Word8 -> Seq Pixel
tileRow v1 v2 = Seq.zipWith toPixel (toBits v1) (toBits v2)

    where toBits :: Word8 -> Seq Bool
          toBits v = do
            i <- [7,6..0]
            pure $ toBool $ (v `shiftR` i) .&. 1

ppuMode :: Lens' Mmu Pixel
ppuMode = lens _ppuMode $ \mem v ->
    mem&ioreg.ix 0x41 .~ ((mem^?!ioreg.ix 0x41) .&. 0xFC) .|. fromIntegral (fromEnum v)

_ppuMode :: Mmu -> Pixel
_ppuMode mem = toEnum . fromIntegral $ (mem^?!ioreg.ix 0x41) .&. 3

scx, scy :: Lens' Mmu Word8
scx = lens (^?!ioreg.ix 0x43) (\mem v -> mem&ioreg.ix 0x43 .~ v)
scy = lens (^?!ioreg.ix 0x42) (\mem v -> mem&ioreg.ix 0x42 .~ v)

lyc :: Lens' Mmu Word8
lyc = lens (^?!ioreg.ix 0x45) (\mem v -> mem&ioreg.ix 0x45 .~ v)

ly :: Lens' Mmu Word8
ly = lens (^?!ioreg.ix 0x44) (\mem v -> mem&ioreg.ix 0x44 .~ v)

objEnable, objSize, bgTileData, winEnable, lcdEnable :: Lens' Mmu Bool

objEnable = lens (^.lcdc.bit 1) (\mem v -> mem&lcdc.bit 1 .~ v)
objSize = lens (^.lcdc.bit 2) (\mem v -> mem&lcdc.bit 2 .~ v)

bgTileData = lens (^.lcdc.bit 4) (\mem v -> mem&lcdc.bit 4 .~ v)
winEnable = lens (^.lcdc.bit 5) (\mem v -> mem&lcdc.bit 5 .~ v)
lcdEnable = lens (^.lcdc.bit 7) (\mem v -> mem&lcdc.bit 7 .~ v)

lcdc :: Lens' Mmu Word8
lcdc = lens (^?!ioreg.ix 0x40) (\mem v -> mem&ioreg.ix 0x40 .~ v)
