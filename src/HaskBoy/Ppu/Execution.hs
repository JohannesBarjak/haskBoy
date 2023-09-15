{-# LANGUAGE ImportQualifiedPost #-}

module HaskBoy.Ppu.Execution
    ( drawTiles, writeTiles
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

writeTiles :: [[Vector Pixel]] -> Vector Pixel -> Vector Pixel
writeTiles ts dp = runST $ do
    mdp <- V.thaw dp
    for_ (zip [0..] ts) $ \(k, tl) -> do
        let x = (k `rem` 32) * 8
        let y = (k `quot` 32) * 8

        for_ (zip [0..] tl) $ \(i, row) -> do
            for_ (V.indexed row) $ \(j, v) -> do
                VM.write mdp (((y + i) * 32) + (x + j)) v
    V.freeze mdp

tiles :: State Mmu [[Vector Pixel]]
tiles = mapM getTile =<< tileMaps

tileMaps :: State Mmu [Word8]
tileMaps = sequence [use (addr (0x9800 + (y * 32) + x)) | y <- [0..31], x <- [0..31]]

-- | Get a tile using an index, which should come from one of the Gameboy's tilemaps
getTile :: Word8 -> State Mmu [Vector Pixel]
getTile tmIndex = mapM (fmap (uncurry tileRow) . tileBytes . (*2)) [0..7]

    where tileBytes :: Address -> State Mmu (Word8, Word8)
          tileBytes i = do
            fstBits <- use (addr (si + i))
            sndBits <- use (addr (si + i + 1))
            pure (fstBits, sndBits)

          si = 0x8000 + fromIntegral tmIndex

-- | Get a single tile row from a pair of bytes
tileRow
    :: Word8 -- ^ Lower bits
    -> Word8 -- ^ Upper bits
    -> Vector Pixel

tileRow v1 v2 = V.fromList $ zipWith toPixel (toBits v1) (toBits v2)

    where toBits :: Word8 -> [Bool]
          toBits v = [toEnum . fromIntegral $ (v `shiftR` i) .&. 1 | i <- [7,6..0]]

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
bgTileData = lens (\mmu' -> takeBit (mmu'^.lcdc) 4) (\mmu' v -> mmu'&lcdc .~ assignBit (mmu'^.lcdc) 4 v)
    where lcdc :: Lens' Mmu Word8
          lcdc = raw 0xFF40

takeBit :: Word8 -> Int -> Bool
takeBit v i = toBool (shiftR v i)

assignBit :: Word8 -> Int -> Bool -> Word8
assignBit v i True  = v .|. (1 `shiftL` i)
assignBit v i False = v .&. complement (1 `shiftL` i)
