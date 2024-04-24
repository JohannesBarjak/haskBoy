{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}

module HaskBoy.Ppu.Execution
    ( drawTiles
    , bgScanline
    , bgTileMaps
    , getTileRow, tileRow
    , scx, scy
    , ly, lyc
    , ppuMode
    ) where

import HaskBoy.Emulator

import HaskBoy.Mmu
import HaskBoy.Ppu
import HaskBoy.BitOps

import Control.Lens
import Control.Monad.State.Strict

import Data.Sequence (Seq)
import Data.Sequence qualified as Seq

import Data.Bits (Bits((.&.), shiftR, (.|.)))
import Data.Word (Word8)
import Foreign.Marshal (toBool)

import Data.Bool (bool)
import Data.Maybe (fromMaybe)
import Data.Ix (Ix(inRange))

import Control.Applicative (Applicative(liftA2))

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

    let visibleX obj = inRange (1, 167) (obj^.xPos)
    let visibleY obj = inRange ((lineY + 1 - size, lineY)&both +~ 16) (obj^.yPos)

    -- Only Y visibility affects the maximum of 10
    -- sprite objects per scanline limit.
    scanAttrs <- Seq.take 10 . Seq.filter visibleY <$> use (mmu.oam)

    tileAddrMode <- bool 0xFF 0xFE <$> use (mmu.objSize)
    let getTileAddr v = 0x8000 + (fromIntegral (v .&. tileAddrMode) * 16)

    forM_ scanAttrs $ \obj -> do
        let rowIndex = size - (obj^.yPos - lineY + size - 16)
        let tileIndex = getTileAddr (obj^.tlIdx)

        spriteRow <- zoom mmu $ getTileRow tileIndex rowIndex

        let writeSprite i v = fromMaybe v $
                Seq.lookup (i - fromIntegral (obj^.xPos) + 8) spriteRow

        ppu.display.ix (fromIntegral lineY) %=
            Seq.mapWithIndex writeSprite

bgScanline :: State Mmu (Seq Pixel)
bgScanline = do
    y <- liftA2 (+) (use ly) (use scy)
    let (tileIndex, rowIndex) = (y `quotRem` 8)&both %~ fromIntegral

    bgtd <- use bgTileData
    let tileAddress tI = if bgtd then
            0x8000 + (fromIntegral tI * 16)
        else 0x9000 + (fromIntegral (twoCompl tI) * 16)

    bgScan <- fmap join $
        traverse ((`getTileRow` rowIndex) . tileAddress)
        =<< bgTileMaps tileIndex

    scrollX <- use scx
    let bgEnd = Seq.drop (fromIntegral scrollX) bgScan

    pure $ if Seq.length bgEnd >= 160 then
            Seq.take 160 bgEnd else undefined -- TODO: Wrap around display

bgTileMaps :: Word8 -> State Mmu (Seq Word8)
bgTileMaps tI = sequence $ do
    i <- Seq.fromList [tileIndex * 32..(tileIndex * 32) + 32]
    pure $ use (addr (0x9800 + i))
    where tileIndex = fromIntegral tI

getTileRow :: Address -> Word8 -> State Mmu (Seq Pixel)
getTileRow tileAddress rowIndex = do
    tileRow <$> tileBytes (tileAddress + (fromIntegral rowIndex * 2))

    where tileBytes :: Address -> State Mmu (Word8, Word8)
          tileBytes i = liftA2 (,) (use (addr i)) (use (addr $ i + 1))

twoCompl :: Word8 -> Int
twoCompl r8
    | r8 < 128  = fromIntegral r8
    | otherwise = -(256 - fromIntegral r8)

-- | Get a single tile row from a pair of bytes
tileRow
    ::
    ( Word8 -- ^ Lower bits
    , Word8 -- ^ Upper bits
    )
    -> Seq Pixel

tileRow (v1,v2) = Seq.zipWith toPixel (toBits v1) (toBits v2)

    where toBits :: Word8 -> Seq Bool
          toBits v = Seq.fromList $ [toBool $ (v `shiftR` i) .&. 1 | i <- [7,6..0]]

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
