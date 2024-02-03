{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE DataKinds           #-}

module HaskBoy.Ppu.Execution
    ( drawTiles
    , bgScanline
    , tileMaps
    , getTileRow, tileRow
    , scx, scy
    , ly, lyc
    , ppuMode
    ) where

import HaskBoy.Emulator

import HaskBoy.Mmu
import HaskBoy.Ppu
import HaskBoy.Ppu.LcdControl

import Control.Lens
import Control.Monad.State.Strict

import Data.Sequence (Seq)
import Data.Sequence qualified as Seq

import Data.Mod.Word qualified as Mod8

import Data.Bits (Bits((.&.), shiftR, (.|.)))
import Data.Word (Word8)
import Foreign.Marshal (toBool)

import Control.Applicative (Applicative(liftA2))

drawTiles :: State Emulator ()
drawTiles = do
    lineY <- use (mmu.ly)
    ppu.display.ix (fromIntegral $ Mod8.unMod lineY) <~ zoom mmu bgScanline

drawSprites :: State Emulator ()
drawSprites = undefined

bgScanline :: State Mmu (Seq Pixel)
bgScanline = do
    scrollX <- use scx
    bgScan <- bgScanlineRow =<< liftA2 (+) (use scy) (fromIntegral . Mod8.unMod <$> use ly)

    let bgEnd = Seq.drop (fromIntegral scrollX) bgScan

    pure $ if Seq.length bgEnd >= 160 then
            Seq.take 160 bgEnd else undefined

bgScanlineRow :: Word8 -> State Mmu (Seq Pixel)
bgScanlineRow y = fmap join $ traverse (getTileRow rowIndex) =<< tileMaps tileIndex
    where (tileIndex, rowIndex) = (y `quotRem` 8)&both %~ fromIntegral

tileMaps :: Integer -> State Mmu (Seq Word8)
tileMaps tileIndex = sequence $ do
    i <- fromIntegral <$> Seq.fromList [tileIndex * 32..(tileIndex * 32) + 32]
    pure $ use (addr (0x9800 + i))

getTileRow :: Integer -> Word8 -> State Mmu (Seq Pixel)
getTileRow rowIndex tileIndex = do
    bgtd <- use bgTileData
    let tileAddress = if bgtd then
            0x8000 + (fromIntegral tileIndex * 16)
        else 0x9000 + (fromIntegral (twoCompl tileIndex) * 16)

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
ppuMode = lens _ppuMode $ \mmu' v ->
    mmu'&raw 0xFF41 .~ ((mmu'^?!raw 0xFF41) .&. 0xFC) .|. fromIntegral (fromEnum v)

_ppuMode :: Mmu -> Pixel
_ppuMode mmu' = toEnum . fromIntegral $ (mmu'^?!raw 0xFF41) .&. 3

scx, scy :: Lens' Mmu Word8
scx = raw 0xFF43
scy = raw 0xFF42

lyc :: Lens' Mmu Word8
lyc = raw 0xFF45

ly :: Lens' Mmu (Mod8.Mod 154)
ly = lens (fromIntegral . (^.raw 0xFF44)) (\mmu' v -> mmu'&raw 0xFF44 .~ fromIntegral (Mod8.unMod v))
