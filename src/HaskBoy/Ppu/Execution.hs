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
import HaskBoy.BitOps

import Control.Lens
import Control.Monad.State.Strict

import Data.Vector (Vector)
import Data.Vector qualified as V

import Data.Bits (Bits((.&.), shiftR, (.|.)))
import Data.Word (Word8)
import Foreign.Marshal (toBool)

import Data.Foldable (for_)

import Control.Monad.ST
import Data.Vector.Mutable qualified as VM

type Tile = Vector (Vector Pixel)

drawTiles :: State Emulator ()
drawTiles = do
    t <- zoom mmu tiles
    dp <- use (ppu.display)

    ppu.display .= writeTiles t dp

writeTiles :: [Tile] -> Display -> Display
writeTiles ts dp = runST $ do
    mdp <- V.thaw dp
    for_ (zip [0..] ts) $ \(i, tile) -> do
        let x = (i `mod` 32) * 8
        let y = (i `div` 32) * 8

        writeTile mdp (x,y) tile
    V.freeze mdp

writeTile :: VM.PrimMonad m => VM.MVector (VM.PrimState m) Pixel -> (Int, Int) -> Tile -> m ()
writeTile mdp (x,y) tile = do
    for_ (V.indexed tile) $ \(j, row) -> do
        for_ (V.indexed row) $ \(i, pixel) -> do
            VM.write mdp (dpIndex (x + i) (y + j)) pixel
    where dpIndex i j = (j * 256) + i

tiles :: State Mmu [Tile]
tiles = traverse getTile =<< tileMaps

tileMaps :: State Mmu [Word8]
tileMaps = sequence $ do
    i <- [0..1023]
    pure $ use (addr (0x9800 + i))

-- | Get a tile using an index, which should come from one of the Gameboy's tilemaps
getTile :: Word8 -> State Mmu Tile
getTile tileIndex = do
    bgtd <- use bgTileData
    let tileAddress = if bgtd then
            0x8000 + (fromIntegral tileIndex * 16)
        else 0x9000 + (fromIntegral (twoCompl tileIndex) * 16)

    mapM (fmap tileRow . tileBytes . (tileAddress +) . (*2)) $ V.fromList [0..7]

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
    -> Vector Pixel

tileRow (v1,v2) = V.fromList $ zipWith toPixel (toBits v1) (toBits v2)

    where toBits :: Word8 -> [Bool]
          toBits v = [toBool $ (v `shiftR` i) .&. 1 | i <- [7,6..0]]

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
