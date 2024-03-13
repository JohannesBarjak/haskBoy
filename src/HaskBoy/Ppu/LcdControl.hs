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
objEnable = lens (^.lcdc.bit 1) (\mem v -> mem&lcdc.bit 1 .~ v)

objSize :: Lens' Mmu Bool
objSize = lens (^.lcdc.bit 2) (\mem v -> mem&lcdc.bit 2 .~ v)

bgTileData :: Lens' Mmu Bool
bgTileData = lens (^.lcdc.bit 4) (\mem v -> mem&lcdc.bit 4 .~ v)

winEnable :: Lens' Mmu Bool
winEnable = lens (^.lcdc.bit 5) (\mem v -> mem&lcdc.bit 5 .~ v)

lcdEnable :: Lens' Mmu Bool
lcdEnable = lens (^.lcdc.bit 7) (\mem v -> mem&lcdc.bit 7 .~ v)

lcdc :: Lens' Mmu Word8
lcdc = lens (^?!ioreg.ix 0x40) (\mem v -> mem&ioreg.ix 0x40 .~ v)
