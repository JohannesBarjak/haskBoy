{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE TemplateHaskell     #-}

module HaskBoy.Emulator where

import HaskBoy.Mmu (Mmu(..))

import HaskBoy.Cpu
    ( Cpu(..)
    , Register(..)
    )

import HaskBoy.Ppu
    ( Ppu(..)
    , toPixel
    )

import Data.Word (Word8)
import Data.Vector qualified as V
import Data.Sequence qualified as Seq

import Control.Lens

data Emulator = Emulator
    { _mmu :: Mmu
    , _cpu :: Cpu
    , _ppu :: Ppu
    }

makeLenses ''Emulator

logo :: [Word8]
logo =
    [ 0xCE, 0xED, 0x66, 0x66, 0xCC, 0x0D, 0x00, 0x0B
    , 0x03, 0x73, 0x00, 0x83, 0x00, 0x0C, 0x00, 0x0D
    , 0x00, 0x08, 0x11, 0x1F, 0x88, 0x89, 0x00, 0x0E
    , 0xDC, 0xCC, 0x6E, 0xE6, 0xDD, 0xDD, 0xD9, 0x99
    , 0xBB, 0xBB, 0x67, 0x63, 0x6E, 0x0E, 0xEC, 0xCC
    , 0xDD, 0xDC, 0x99, 0x9F, 0xBB, 0xB9, 0x33, 0x3E
    ]

toMemory :: [Word8] -> Maybe Mmu
toMemory xs = if length xs <= 0x8000
        then do
            let _rom0  = Seq.fromList $ xs <> replicate 4 0 <> logo <> replicate (0x7FFC - length logo - length xs) 0
            let _rom1  = Seq.replicate 0x4000 0
            let _vram  = Seq.replicate 0x2000 0
            let _eram  = Seq.replicate 0x2000 0
            let _wram0 = Seq.replicate 0x1000 0
            let _wram1 = Seq.replicate 0x1000 0
            let _oam   = Seq.replicate 0xA0 0
            let _ioreg = Seq.replicate 0x80 0
            let _hram  = Seq.replicate 0x7F 0
            let _ie    = 0
            Just $ Mmu {..}
        else Nothing

initialEmulator :: Mmu -> Emulator
initialEmulator _mmu = Emulator
    { _mmu
    , _cpu = initialCpu
    , _ppu = initialPpu
    }

initialMmu :: Mmu
initialMmu = Mmu
    { _rom0  = Seq.replicate 0x4000 0
    , _rom1  = Seq.replicate 0x4000 0
    , _vram  = Seq.replicate 0x2000 0
    , _eram  = Seq.replicate 0x2000 0
    , _wram0 = Seq.replicate 0x1000 0
    , _wram1 = Seq.replicate 0x1000 0
    , _oam   = Seq.replicate 0xA0 0
    , _ioreg = Seq.replicate 0x80 0
    , _hram  = Seq.replicate 0x7F 0
    , _ie    = 0
    }

initialCpu :: Cpu
initialCpu = Cpu
    { _register = Register
        { _af = 0x01B0
        , _bc = 0x0013
        , _de = 0x00D8
        , _hl = 0x014D
        , _pc = 0x0000
        , _sp = 0xFFFE
        }
    , _tclock = 0
    }

initialPpu :: Ppu
initialPpu = Ppu
    { _display = V.fromList $ replicate (256 * 256) (toPixel False False)
    , _clock   = 0
    }
