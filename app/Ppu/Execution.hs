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
import Data.Bool (bool)
import Data.Foldable (for_)

step :: State Emulator ()
step = do
    m <- use (mmu.ppumode)
    t <- use (ppu.clock)

    case m of
        OAMRead  -> do
            when (t >= 80) $ do
                mmu.ppumode .= VRAMRead
                ppu.clock -= 80
        
        VRAMRead -> do
            when (t >= 172) $ do
                mmu.ppumode .= HBlank
                ppu.clock -= 172

                drawLine

        HBlank   -> do
            when (t >= 204) $ do
                mmu.ppumode .= OAMRead
                ppu.clock -= 204

                mmu.ly += 1
                y <- use (mmu.ly)

                when (y == 143) $ do
                    mmu.ppumode .= VBlank


        VBlank   -> do
            when (t >= 456) $ do
                ppu.clock -= 456

                mmu.ly += 1
                y <- use (mmu.ly)

                when (y > 153) $ do
                    mmu.ppumode .= OAMRead
                    mmu.ly .= 0

drawTiles :: State Emulator ()
drawTiles = do
    for_ [0..31] $ \j -> do
        for_ [0..31] $ \i -> do
            tmIndex <- use (mmu.addr ((j * 32) + i))
            drawTile tmIndex (fromIntegral i * 8) (fromIntegral j * 8)

drawTile :: Word8 -> Int -> Int -> State Emulator ()
drawTile tmIndex x y = do
        for_ [0..7] $ \i -> do
            row <- tileRow
                        <$> use (mmu.addr (si + fromIntegral (i * 2)))
                        <*> use (mmu.addr (si + fromIntegral (i * 2) + 1))

            for_ (V.indexed row) $ \(j, v) -> do
                ppu.display.ix (y + i). ix (x + j) .= v
    where si = 0x8800 + fromIntegral tmIndex

drawLine :: State Emulator ()
drawLine = do
    drawLineBackground

getBgTileMap :: State Emulator (Vector Int)
getBgTileMap = mapM (\i -> fromIntegral <$> use (mmu.addr i)) $ V.fromList [0x9800..0x9BFF]

drawLineBackground :: State Emulator ()
drawLineBackground = do
--    ppu.mapoffs <~ bool 0x9800 0x9C00 <$> use (mmu.bgTileData)

    v <- (.&. 255) <$> ((+) <$> use (mmu.ly) <*> use (mmu.scy))
--    ppu.mapoffs += fromIntegral (v `shiftR` 3)

    lineoffs <- (`shiftR` 3) <$> use (mmu.scx)

    x <- (.&. 7) <$> use (mmu.scx)
    y <- (.&. 7) <$> currentYOff

    for_ [0..159] $ \i -> do
        ppu.display.ix i.ix (fromIntegral x) .= C0

    where currentYOff = (+) <$> use (mmu.ly) <*> use (mmu.scy)

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

-- tileRow converts two bytes into a row of pixels
tileRow :: Word8 -> Word8 -> Vector Color
tileRow v1 v2 = V.map toEnum (V.zipWith (+) lowerPixelBits upperPixelBits)
    -- These lists contain the upper and lower bit of the row's pixels
    where lowerPixelBits = byteToBitLine v1
          upperPixelBits = V.map (*2) (byteToBitLine v2)

          byteToBitLine :: Word8 -> Vector Int
          byteToBitLine v = do
                i <- V.fromList [7,6..0]
                pure $ fromIntegral $ (v `shiftR` i) .&. 1

takeBit :: Word8 -> Int -> Bool
takeBit v i = toBool (shiftR v i)

assignBit :: Word8 -> Int -> Bool -> Word8
assignBit v i True  = v .|. (1 `shiftL` i)
assignBit v i False = v .&. complement (1 `shiftL` i)
