{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE OverloadedLists     #-}

module HaskBoy.Ppu.Execution
    ( cyclePpu
    , drawTiles
    , bgScanline
    , tileRow
    , scx, scy
    , ly, lyc
    , ppuMode
    ) where

import Control.Lens

import Control.Monad (forM_, when, guard)
import Control.Monad.State.Strict
import Control.Monad.Trans.Maybe

import Data.Bits ((.&.), (.|.), testBit)
import Data.Bool (bool)

import Data.Function (on)
import Data.Ix (inRange)
import Data.Maybe (fromMaybe)

import Data.Sequence (Seq)
import Data.Sequence qualified as S
import Data.Word (Word8)

import HaskBoy.Mmu
import HaskBoy.Ppu

import HaskBoy.BitOps

cyclePpu :: (MonadState s m, HasMmu s, HasPpu s) => MaybeT m ()
cyclePpu = do
    prev_clk <- use clock
    pmode <- use ppuMode

    ly .= fromIntegral (prev_clk `quot` 456 `rem` 154)
    lineY <- use ly

    when (lineY == 144) do
        ppuMode .= VBlank
        writeM 0xFF0F .bit 0 .= True

    let mode = case prev_clk `rem` 456 of
            x | x >= 172 -> HBlank
            x | x >= 80 -> VramRead
            _ -> OamRead

    guard (lineY < 144 && mode /= pmode)
    ppuMode .= mode

    case mode of
        OamRead -> do
            lineCmp <- use lyc
            when (lineY == lineCmp) lycUpdate

        VramRead -> drawTiles
        _ -> pure ()

lycUpdate :: HasMmu s => MonadState s m => m ()
lycUpdate = do
    lycIE <- use (readM 0xFF41 . bit 6)
    when lycIE $ writeM 0xFF0F . bit 1 .= True

-- Draw background and sprites into the display.
drawTiles :: (HasMmu s, HasPpu s) => MonadState s m => m ()
drawTiles = do
    lineY <- fromIntegral <$> use ly
    display.ix lineY <~ bgScanline <$> use mmu
    drawSprites . spriteScan =<< use mmu

-- TODO: Implement 40 sprite limit.
drawSprites :: (HasMmu s, HasPpu s) => MonadState s m => Seq (ObjAttr, Seq Pixel) -> m ()
drawSprites sprites = forM_ sprites \(obj, srow) -> do
    let writeSprite i v = fromMaybe v $
            S.lookup (i - fromIntegral (obj^.xPos) + 8) srow

    lineY <- use ly
    ppu.display.ix (fromIntegral lineY) %= S.mapWithIndex writeSprite

spriteScan :: HasMmu s => s -> Seq (ObjAttr, Seq Pixel)
spriteScan mem = scanAttr <&> (,) <*> liftA2 (tileRow mem) ri ti

    where scanAttr = S.take 10 $ S.filter visibleY (mem^.oam)
          visibleY obj = inRange ((mem^.ly + 1 - size, mem^.ly)&both +~ 16) (obj^.yPos)
          size = bool 8 16 (mem^.objSize)

          ti obj = 0x8000 + (fromIntegral (obj^.tlIdx .&. addrMode) * 16)
          ri obj = size - (obj^.yPos - (mem^.ly) + size - 16)
          addrMode = bool 0xFF 0xFE (mem^.objSize)

bgScanline :: HasMmu s => s -> Seq Pixel
bgScanline mem
    = (tileRow mem ri . fromIntegral . tileAddress =<< bgTileMaps)
    & S.drop (fromIntegral $ mem^.scx)
    & S.cycleTaking 160

    where bgTileMaps = let ta = tileMapAddress + fromIntegral ti * 32 in
            [mem^.readM i | i <- [ta..ta + 32]]

          tileAddress :: Word8 -> Int
          tileAddress idx = if mem^.bgTileData
            then 0x8000 + (fromIntegral idx * 16)
            else 0x9000 + (twoCompl idx * 16)

          (ti, ri) = (mem^.ly + mem^.scy) `quotRem` 8
          tileMapAddress = bool 0x9800 0x9C00 (mem^.bgTileMap)

-- Tile row, can be either a background or a sprite tile
tileRow :: HasMmu s => s -> Word8 -> Address -> Seq Pixel
tileRow mem ri ta = let ra = ta + (fromIntegral ri * 2) in
    buildRow (mem^.readM ra) (mem^.readM (ra + 1))

    -- | Get a single tile row from a pair of bytes
    where buildRow :: Word8 -> Word8 -> Seq Pixel
          buildRow v1 v2 = [on toPixel (`testBit` i) v1 v2 | i <- [7,6..0]]

twoCompl :: Word8 -> Int
twoCompl b
    |   b < 128 = fromIntegral b
    | otherwise = -(256 - fromIntegral b)

ppuMode :: HasMmu s => Lens' s PpuMode
ppuMode = lens _ppuMode \mem v ->
    mem&ioreg.ix 0x41 .~ ((mem^?!ioreg.ix 0x41) .&. 0xFC) .|. fromIntegral (fromEnum v)

    where _ppuMode :: HasMmu s =>  s -> PpuMode
          _ppuMode mem = toEnum . fromIntegral $ (mem^?!ioreg.ix 0x41) .&. 3

scx, scy :: HasMmu s => Lens' s Word8
scx = lens (^?!ioreg.ix 0x43) \mem v -> mem&ioreg.ix 0x43 .~ v
scy = lens (^?!ioreg.ix 0x42) \mem v -> mem&ioreg.ix 0x42 .~ v

lyc :: HasMmu s => Lens' s Word8
lyc = lens (^?!ioreg.ix 0x45) \mem v -> mem&ioreg.ix 0x45 .~ v

ly :: HasMmu s => Lens' s Word8
ly = raw 0xFF44

objSize, bgTileMap, bgTileData :: HasMmu s => Lens' s Bool

objSize = lcdc.bit 2
bgTileMap = lcdc.bit 3
bgTileData = lcdc.bit 4

lcdc :: HasMmu s => Lens' s Word8
lcdc = lens (^?!ioreg.ix 0x40) \mem v -> mem&ioreg.ix 0x40 .~ v
