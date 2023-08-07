{-# LANGUAGE ImportQualifiedPost #-}

module Ppu.Execution where

import Emulator
import Mmu
import Ppu

import Control.Lens
import Control.Monad.State.Strict

import Data.Vector (Vector)
import Data.Vector qualified as V

import Data.Bits
import Data.Word (Word8)
import Foreign.Marshal (toBool)

import Data.Foldable (for_)

import Control.Monad.ST
import Data.Vector.Mutable qualified as VM

drawTiles :: State Emulator ()
drawTiles = do
    t <- zoom mmu tiles
    dp <- use (ppu.display)

    ppu.display .= writeTiles t dp

writeTiles :: [[Vector ColorIndex]] -> Vector ColorIndex -> Vector ColorIndex
writeTiles ts dp = runST $ do
    mdp <- V.thaw dp
    for_ (zip [0..] ts) $ \(k, tl) -> do
        let x = (k `rem` 32) * 8
        let y = (k `quot` 32) * 8

        for_ (zip [0..] tl) $ \(i, row) -> do
            for_ (V.indexed row) $ \(j, v) -> do
                VM.write mdp (((y + i) * 32) + (x + j)) v
    V.freeze mdp

tiles :: State Mmu [[Vector ColorIndex]]
tiles = mapM rowPack =<< tileMaps

tileMaps :: State Mmu [Word8]
tileMaps = sequence [use (addr (0x9800 + (y * 32) + x)) | y <- [0..31], x <- [0..31]]

rowPack :: Word8 -> State Mmu [Vector ColorIndex]
rowPack tmIndex = mapM (fmap (uncurry tileRow) . tileBytes) [0..7]

    where tileBytes :: Word8 -> State Mmu (Word8, Word8)
          tileBytes i = do
            fstBits <- use (addr (si + fromIntegral (i * 2)))
            sndBits <- use (addr (si + fromIntegral (i * 2) + 1))
            pure (fstBits, sndBits)

          si = 0x8000 + fromIntegral tmIndex

-- tileRow converts two bytes into a row of pixels
tileRow :: Word8 -> Word8 -> Vector ColorIndex
tileRow v1 v2 = V.map toEnum $ V.zipWith (+) lowerPixelBits upperPixelBits
    -- These lists contain the upper and lower bits of the row's pixels
    where lowerPixelBits = toBits v1
          upperPixelBits = V.map (*2) (toBits v2)

          toBits :: Word8 -> Vector Int
          toBits v = do
                i <- V.fromList [7,6..0]
                pure $ fromIntegral $ (v `shiftR` i) .&. 1

ppumode :: Lens' Mmu PPUMode
ppumode = lens _ppumode $ \mmu' v ->
    mmu'&raw 0xFF41 .~ ((mmu'^?!raw 0xFF41) .&. 0xFC) .|. fromIntegral (fromEnum v)

_ppumode :: Mmu -> PPUMode
_ppumode mmu' = toEnum . fromIntegral $ (mmu'^?!raw 0xFF41) .&. 3

scx, scy :: Lens' Mmu Word8
scx = raw 0xFF43
scy = raw 0xFF42

lyc :: Lens' Mmu Word8
lyc = raw 0xFF45

ly :: Lens' Mmu Word8
ly = raw 0xFF44

bgTileData :: Lens' Mmu Bool
bgTileData = lens (\mmu' -> takeBit (mmu'^.lcdc) 4) (\mmu' v -> mmu'&lcdc .~ assignBit (mmu'^.lcdc) 4 v)
    where lcdc :: Lens' Mmu Word8
          lcdc = raw 0xFF40

takeBit :: Word8 -> Int -> Bool
takeBit v i = toBool (shiftR v i)

assignBit :: Word8 -> Int -> Bool -> Word8
assignBit v i True  = v .|. (1 `shiftL` i)
assignBit v i False = v .&. complement (1 `shiftL` i)
