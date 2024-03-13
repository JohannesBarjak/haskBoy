{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE TemplateHaskell     #-}

module HaskBoy.Emulator where

import HaskBoy.Mmu
    ( Mmu(..)
    , ObjAttr(..)
    )

import HaskBoy.Cpu
    ( Cpu(..)
    , Registers(..)
    )

import HaskBoy.Ppu
    ( Ppu(..)
    , toPixel
    )

import Data.Word (Word8)
import Data.Sequence qualified as Seq

import Control.Lens

data Emulator = Emulator
    { _mmu :: !Mmu
    , _cpu :: !Cpu
    , _ppu :: !Ppu
    }

makeLenses ''Emulator

toMemory :: [Word8] -> Maybe Mmu
toMemory xs = if length xs == 0x8000
        then do
            Just $ Mmu
                { _rom0  = Seq.fromList r0
                , _rom1  = Seq.fromList r1
                , _vram  = Seq.replicate 0x2000 0
                , _eram  = Seq.replicate 0x2000 0
                , _wram0 = Seq.replicate 0x1000 0
                , _wram1 = Seq.replicate 0x1000 0
                , _oam   = Seq.replicate 40 (ObjAttr 0 0 0)
                , _ioreg = Seq.replicate 0x80 0
                , _hram  = Seq.replicate 0x7F 0
                , _ie    = 0
                }

        else Nothing
        where (r0,r1) = splitAt 0x4000 xs

initialEmulator :: Mmu -> Emulator
initialEmulator _mmu = Emulator
    { _mmu
    , _cpu = initialCpu
    , _ppu = initialPpu
    }

initialCpu :: Cpu
initialCpu = Cpu
    { _register = Registers
        { _af = 0x01B0
        , _bc = 0x0013
        , _de = 0x00D8
        , _hl = 0x014D
        , _pc = 0x0100
        , _sp = 0xFFFE
        }
    , _interruptEnable = True
    , _tclock = 0
    }

initialPpu :: Ppu
initialPpu = Ppu
    { _display = Seq.replicate 144 $ Seq.replicate 160 (toPixel False False)
    , _clock   = 0
    }
