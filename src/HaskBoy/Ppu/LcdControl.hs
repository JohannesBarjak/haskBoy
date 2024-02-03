module HaskBoy.Ppu.LcdControl 
  ( bgTileData
  , objSize
  , objEnable
  , lcdEnable
  , winEnable
  ) where

import HaskBoy.BitOps
import HaskBoy.Mmu

import Control.Lens

import Data.Word (Word8)

objEnable :: Lens' Mmu Bool
objEnable = lens (^.lcdc.bit 1) (\mmu' v -> mmu'&lcdc.bit 1 .~ v)

objSize :: Lens' Mmu Bool
objSize = lens (^.lcdc.bit 2) (\mmu' v -> mmu'&lcdc.bit 2 .~ v)

bgTileData :: Lens' Mmu Bool
bgTileData = lens (^.lcdc.bit 4) (\mmu' v -> mmu'&lcdc.bit 4 .~ v)

winEnable :: Lens' Mmu Bool
winEnable = lens (^.lcdc.bit 5) (\mmu' v -> mmu'&lcdc.bit 5 .~ v)

lcdEnable :: Lens' Mmu Bool
lcdEnable = lens (^.lcdc.bit 7) (\mmu' v -> mmu'&lcdc.bit 7 .~ v)

lcdc :: Lens' Mmu Word8
lcdc = raw 0xFF40
