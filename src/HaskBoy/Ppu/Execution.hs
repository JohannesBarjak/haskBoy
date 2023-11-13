{-# LANGUAGE ImportQualifiedPost #-}

module HaskBoy.Ppu.Execution
    ( drawTiles
    , tiles, tileMaps
    , getTile, tileRow
    , scx, scy
    , ly, lyc
    , ppumode, _ppumode
    , bgTileData
    ) where

import HaskBoy.Emulator

import HaskBoy.Mmu
import HaskBoy.Ppu
import HaskBoy.BitOps

import Control.Lens
import Control.Monad.State.Strict

import Data.Vector (Vector)
import Data.Vector qualified as V

import Data.Sequence (Seq)
import Data.Sequence qualified as Seq

import Data.Bits (Bits((.&.), shiftR, (.|.)))
import Data.Word (Word8)
import Foreign.Marshal (toBool)

type Tiles = Seq Tile
type Tile = Seq (Seq Pixel)

drawTiles :: State Emulator ()
drawTiles = do
    ts <- zoom mmu tiles

    scrollX <- use (mmu.scx)
    scrollY <- use (mmu.scy)
    lineY <- use (mmu.ly)

    ppu.display.ix (fromIntegral lineY) .= bgScanline (scrollX, scrollY, lineY) ts

bgScanline :: (Word8, Word8, Word8) -> Tiles -> Seq Pixel
bgScanline (scrollX, scrollY, lineY) = bgScx scrollX . bgLy (scrollY + lineY)

bgScx :: Word8 -> Seq Pixel -> Seq Pixel
bgScx scrollX bgScan
    = if Seq.length bgEnd >= 160 then
        Seq.take 160 bgEnd else undefined

    where bgEnd = Seq.drop (fromIntegral scrollX) bgScan

-- 'Y' is scy + ly whenever ly < 144
bgLy :: Word8 -> Tiles -> Seq Pixel
bgLy y ts = (`Seq.index` rowIndex) =<< Seq.index (Seq.chunksOf 32 ts) tileIndex
    where (tileIndex, rowIndex) = (y `quotRem` 8)&both %~ fromIntegral

tiles :: State Mmu Tiles
tiles = traverse getTile =<< tileMaps

tileMaps :: State Mmu (Seq Word8)
tileMaps = sequence $ do
    i <- Seq.fromList [0..1023]
    pure $ use (addr (0x9800 + i))

-- | Get a tile using an index, which should come from one of the Gameboy's tilemaps
getTile :: Word8 -> State Mmu Tile
getTile tileIndex = do
    bgtd <- use bgTileData
    let tileAddress = if bgtd then
            0x8000 + (fromIntegral tileIndex * 16)
        else 0x9000 + (fromIntegral (twoCompl tileIndex) * 16)

    mapM (fmap tileRow . tileBytes . (tileAddress +) . (*2)) $ Seq.fromList [0..7]

    where tileBytes :: Address -> State Mmu (Word8, Word8)
          tileBytes i = do
            fstBits <- use (addr i)
            sndBits <- use (addr $ i + 1)
            pure (fstBits, sndBits)

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

ppumode :: Lens' Mmu Pixel
ppumode = lens _ppumode $ \mmu' v ->
    mmu'&raw 0xFF41 .~ ((mmu'^?!raw 0xFF41) .&. 0xFC) .|. fromIntegral (fromEnum v)

_ppumode :: Mmu -> Pixel
_ppumode mmu' = toEnum . fromIntegral $ (mmu'^?!raw 0xFF41) .&. 3

scx, scy :: Lens' Mmu Word8
scx = raw 0xFF43
scy = raw 0xFF42

lyc :: Lens' Mmu Word8
lyc = raw 0xFF45

ly :: Lens' Mmu Word8
ly = raw 0xFF44

bgTileData :: Lens' Mmu Bool
bgTileData = lens (^.lcdc.bit 4) (\mmu' v -> mmu'&lcdc.bit 4 .~ v)
    where lcdc :: Lens' Mmu Word8
          lcdc = raw 0xFF40
