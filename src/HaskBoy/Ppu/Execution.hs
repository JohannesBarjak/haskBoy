{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE OverloadedLists     #-}

module HaskBoy.Ppu.Execution
    ( ppuCycle
    , drawTiles
    , bgScanline
    , tileRow
    , scx, scy
    , ly, lyc
    , ppuMode
    ) where

import Control.Lens
import Control.Monad (forM_, when, mfilter)
import Control.Monad.State.Strict

import Data.Bits ((.&.), shiftR, (.|.))
import Data.Bool (bool)

import Data.Function (on)
import Data.Ix (inRange)
import Data.Maybe (fromMaybe)

import Data.Sequence (Seq)
import Data.Sequence qualified as S

import Data.Word (Word8)
import Foreign.Marshal (toBool)

import HaskBoy.BitOps
import HaskBoy.Emulator
import HaskBoy.Mmu
import HaskBoy.Ppu

ppuCycle :: State Emulator ()
ppuCycle = do
    ppuTime <- use (ppu.clock)
    prevMode <- use (mmu.ppuMode)

    let mode = mfilter (/= prevMode) . Just
            $ case ppuTime `rem` 456 of
                x | x >= 172 -> HBlank
                x | x >= 80 -> VramRead
                _ -> OamRead

    mmu.ppuMode %= flip fromMaybe mode

    case mode of
        (Just OamRead) -> do
            mmu.ly += 1

            lineY <- use (mmu.ly)
            when (lineY < 144) drawTiles
        _ -> pure ()

drawTiles :: State Emulator ()
drawTiles = do
    lineY <- fromIntegral <$> use (mmu.ly)
    ppu.display.ix lineY <~ bgScanline <$> use mmu
    drawSprites . spriteScan =<< use mmu

-- TODO: Implement 40 sprite limit.
drawSprites :: Seq (ObjAttr, Seq Pixel) -> State Emulator ()
drawSprites sprites = forM_ sprites $ \(obj, srow) -> do
    let writeSprite i v = fromMaybe v $
            S.lookup (i - fromIntegral (obj^.xPos) + 8) srow

    lineY <- use (mmu.ly)
    ppu.display.ix (fromIntegral lineY) %= S.mapWithIndex writeSprite

spriteScan :: Mmu -> Seq (ObjAttr, Seq Pixel)
spriteScan mem = scanAttr <&> (,) <*> ((tileRow mem . ri) <*> ti)

    where scanAttr = S.take 10 $ S.filter visibleY (mem^.oam)
          visibleY obj = inRange ((mem^.ly + 1 - size, mem^.ly)&both +~ 16) (obj^.yPos)
          size = bool 8 16 (mem^.objSize)

          ti obj = 0x8000 + (fromIntegral (obj^.tlIdx .&. addrMode) * 16)
          ri obj = size - (obj^.yPos - (mem^.ly) + size - 16)
          addrMode = bool 0xFF 0xFE (mem^.objSize)

bgScanline :: Mmu -> Seq Pixel
bgScanline mem
    = (tileRow mem ri . tileAddress =<< bgTileMaps)
    & S.drop (fromIntegral $ mem^.scx)
    & S.cycleTaking 160

    where bgTileMaps = let ta = 0x9800 + fromIntegral ti * 32 in
            [mem^.cloneLens (addr i) | i <- [ta..ta + 32]]

          tileAddress idx
            = if mem^.bgTileData then 0x8000 + (fromIntegral idx * 16)
            else 0x9000 + (fromIntegral (twoCompl idx) * 16)

          (ti, ri) = (mem^.ly + mem^.scy) `quotRem` 8

tileRow :: Mmu -> Word8 -> Address -> Seq Pixel
tileRow mem ri ta = let ra = ta + (fromIntegral ri * 2) in
    buildRow (mem^.cloneLens (addr ra)) (mem^.cloneLens (addr $ ra + 1))

    -- | Get a single tile row from a pair of bytes
    where buildRow :: Word8 -> Word8 -> Seq Pixel
          buildRow v1 v2 = [on toPixel (toBool . (.&. 1) . (`shiftR` i)) v1 v2 | i <- [7,6..0]]

twoCompl :: Word8 -> Int
twoCompl r8
    | r8 < 128  = fromIntegral r8
    | otherwise = -(256 - fromIntegral r8)

ppuMode :: Lens' Mmu PpuMode
ppuMode = lens _ppuMode $ \mem v ->
    mem&ioreg.ix 0x41 .~ ((mem^?!ioreg.ix 0x41) .&. 0xFC) .|. fromIntegral (fromEnum v)

    where _ppuMode :: Mmu -> PpuMode
          _ppuMode mem = toEnum . fromIntegral $ (mem^?!ioreg.ix 0x41) .&. 3

lcdStat :: Lens' Mmu Word8
lcdStat = cloneLens $ raw 0xFF41

scx, scy :: Lens' Mmu Word8
scx = lens (^?!ioreg.ix 0x43) (\mem v -> mem&ioreg.ix 0x43 .~ v)
scy = lens (^?!ioreg.ix 0x42) (\mem v -> mem&ioreg.ix 0x42 .~ v)

lyc :: Lens' Mmu Word8
lyc = lens (^?!ioreg.ix 0x45) (\mem v -> mem&ioreg.ix 0x45 .~ v)

ly :: Lens' Mmu Word8
ly = cloneLens (raw 0xFF44)

objEnable, objSize, bgTileData, winEnable, lcdEnable :: Lens' Mmu Bool

objEnable = lens (^.lcdc.bit 1) (\mem v -> mem&lcdc.bit 1 .~ v)
objSize = lens (^.lcdc.bit 2) (\mem v -> mem&lcdc.bit 2 .~ v)

bgTileData = lens (^.lcdc.bit 4) (\mem v -> mem&lcdc.bit 4 .~ v)
winEnable = lens (^.lcdc.bit 5) (\mem v -> mem&lcdc.bit 5 .~ v)
lcdEnable = lens (^.lcdc.bit 7) (\mem v -> mem&lcdc.bit 7 .~ v)

lcdc :: Lens' Mmu Word8
lcdc = lens (^?!ioreg.ix 0x40) (\mem v -> mem&ioreg.ix 0x40 .~ v)
