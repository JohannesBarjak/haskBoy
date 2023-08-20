{-# LANGUAGE LambdaCase #-}

module Cpu.Execution where

import HaskBoy.Emulator

import HaskBoy.Mmu
import HaskBoy.Cpu
import Cpu.Instructions

import Control.Lens
import Control.Monad.State.Strict

import Data.Word (Word8)
import Data.Bits (Bits((.&.), shiftR))

import Debug.Trace (trace, traceM)
import Numeric (showHex)

execute :: Word8 -> State Emulator ()
execute = \case
        0x00 -> cpu.tclock += 4

        0xAF -> do
            xorA =<< use (cpu.register.a)
            cpu.tclock += 4

        0x0A -> do
            nn <- use (cpu.register.bc)
            cpu.register.a <~ use (mmu.addr nn)

            cpu.tclock += 8

        0x1A -> do
            nn <- use (cpu.register.de)
            cpu.register.a <~ use (mmu.addr nn)

            cpu.tclock += 8

        0x2A -> do
            nn <- use (cpu.register.hl)

            cpu.register.a <~ use (mmu.addr nn)
            cpu.register.hl += 1

            cpu.tclock += 8

        0x3A -> do
            nn <- use (cpu.register.hl)

            cpu.register.a <~ use (mmu.addr nn)
            cpu.register.hl -= 1

            cpu.tclock += 8

        i | i .&. 0xC7 == 0x06 -> do
            v <- consumeByte
            cpu.tclock += 8

            case extractOctalArg 3 i of
                0 -> cpu.register.b .= v
                1 -> cpu.register.c .= v
                2 -> cpu.register.d .= v
                3 -> cpu.register.e .= v
                4 -> cpu.register.h .= v
                5 -> cpu.register.l .= v

                6 -> do
                    nn <- use (cpu.register.hl)
                    mmu.addr nn .= v

                    cpu.tclock += 4

                7 -> cpu.register.a .= v
                _ -> error "`Impossible` error on assign byte to register"

        i | i .&. 0xF8 == 0x40 -> do
            cpu.tclock += 4
            cpu.register.b <~ case extractOctalArg 0 i of
                0 -> use (cpu.register.b)
                1 -> use (cpu.register.c)
                2 -> use (cpu.register.d)
                3 -> use (cpu.register.e)
                4 -> use (cpu.register.h)
                5 -> use (cpu.register.l)

                6 -> do
                    cpu.tclock += 4

                    nn <- use (cpu.register.hl)
                    use (mmu.addr nn)

                7 -> use (cpu.register.a)
                _ -> error "`Impossible` error"


        0x49 -> cpu.tclock += 4

        0x4F -> zoom cpu $ do
            register.c <~ use (register.a)
            tclock += 4

        0x02 -> do
            nn <- use (cpu.register.bc)
            v  <- use (cpu.register.a)

            mmu.addr nn .= v

            cpu.tclock += 8

        0x12 -> do
            nn <- use (cpu.register.de)
            v  <- use (cpu.register.a)

            mmu.addr nn .= v

            cpu.tclock += 8

        0x22 -> do
            nn <- use (cpu.register.hl)
            v  <- use (cpu.register.a)

            mmu.addr nn .= v

            cpu.register.hl += 1
            cpu.tclock += 8

        0x32 -> do
            nn <- use (cpu.register.hl)
            v  <- use (cpu.register.a)

            mmu.addr nn .= v

            cpu.register.hl -= 1
            cpu.tclock += 8

        0x03 -> zoom cpu $ do
            register.bc += 1
            tclock += 8
        0x13 -> zoom cpu $ do
            register.de += 1
            tclock += 8
        0x23 -> zoom cpu $ do
            register.hl += 1
            tclock += 8
        0x33 -> zoom cpu $ do
            register.sp += 1
            tclock += 8

        0x77 -> do
            nn <- use (cpu.register.hl)
            v  <- use (cpu.register.a)

            mmu.addr nn .= v

            cpu.tclock += 8

        0xE0 -> do
            v <- fromIntegral <$> consumeByte
            mmu.addr (0xFF00 + v) <~ use (cpu.register.a)

            cpu.tclock += 12

        0xC9 -> do
            ret
            cpu.tclock += 16

        0xCB -> consumeByte >>= \case
            i | i .&. 0xF8 == 0x30 -> error "Unimplemented Instr"

            0x7c -> do
                bit 7 =<< use (cpu.register.h)
                cpu.tclock += 8
            0x11 -> do
                rl c
                cpu.tclock += 8

            arg  -> error $ "Invalid CB argument: " ++ showHex arg ""

        0xC1 -> do
            cpu.register.bc <~ (\x -> trace ("pop stack: 0x" ++ showHex x "") x) <$> popStack
            cpu.tclock += 12

        0xD1 -> do
            cpu.register.de <~ popStack
            cpu.tclock += 12

        0xE1 -> do
            cpu.register.hl <~ popStack
            cpu.tclock += 12

        0xC3 -> do
            cpu.register.pc <~ consumeWord
            cpu.tclock += 16

        0xCD -> do
            v <- consumeWord
            call v
            cpu.tclock += 24

        0xC5 -> do
            sp' <- use (cpu.register.sp)
            traceM ("stack pointer: 0x" ++ showHex sp' "")
            pushStack =<< use (cpu.register.bc)

        0x17 -> do
            rl a
            cpu.tclock += 4

        0x18 -> jr True
        0x20 -> jr . not =<< use (cpu.register.zero)
        0x28 -> jr =<< use (cpu.register.zero)

        0x01 -> do
            cpu.register.bc <~ consumeWord
            cpu.tclock += 12

        0x11 -> do
            cpu.register.de <~ consumeWord
            cpu.tclock += 12

        0x21 -> do
            cpu.register.hl <~ consumeWord
            cpu.tclock += 12

        0x31 -> do
            cpu.register.sp <~ consumeWord
            cpu.tclock += 12

        0xE2 -> do
            offset <- fromIntegral <$> use (cpu.register.c)
            mmu.addr (0xFF00 + offset) <~ use (cpu.register.a)

            cpu.tclock += 8

        i | i .&. 0xC7 == 0x04 -> do
            cpu.tclock += 4

            case extractOctalArg 3 i of
                0 -> inc (cpu.register.b)
                1 -> inc (cpu.register.c)
                2 -> inc (cpu.register.d)
                3 -> inc (cpu.register.e)
                4 -> inc (cpu.register.h)
                5 -> inc (cpu.register.l)

                6 -> do
                    nn <- use (cpu.register.hl)
                    inc (mmu.addr nn)

                    cpu.tclock += 8

                7 -> inc (cpu.register.a)
                _ -> error "`Impossible` error"

        i | i .&. 0xC7 == 0x05 -> do
            cpu.tclock += 4

            case extractOctalArg 3 i of
                0 -> dec (cpu.register.b)
                1 -> dec (cpu.register.c)
                2 -> dec (cpu.register.d)
                3 -> dec (cpu.register.e)
                4 -> dec (cpu.register.h)
                5 -> dec (cpu.register.l)

                6 -> do
                    nn <- use (cpu.register.hl)
                    dec (mmu.addr nn)

                    cpu.tclock += 8

                7 -> dec (cpu.register.a)
                _ -> error "`Impossible` error with a dec instruction"
            
        i | i .&. 0xF8 == 0x90 -> do
            cpu.tclock += 4

            case extractOctalArg 0 i of
                0 -> sub =<< use (cpu.register.b)
                1 -> sub =<< use (cpu.register.c)
                2 -> sub =<< use (cpu.register.d)
                3 -> sub =<< use (cpu.register.e)
                4 -> sub =<< use (cpu.register.h)
                5 -> sub =<< use (cpu.register.l)

                6 -> do
                    nn <- use (cpu.register.hl)
                    sub =<< use (mmu.addr nn)

                    cpu.tclock += 4

                7 -> sub =<< use (cpu.register.a)
                _ -> error "`Impossible` error"

        i | i .&. 0xF8 == 0x78 -> do
            cpu.tclock += 4

            case extractOctalArg 0 i of
                0 -> cpu.register.a <~ use (cpu.register.b)
                1 -> cpu.register.a <~ use (cpu.register.c)
                2 -> cpu.register.a <~ use (cpu.register.d)
                3 -> cpu.register.a <~ use (cpu.register.e)
                4 -> cpu.register.a <~ use (cpu.register.h)
                5 -> cpu.register.a <~ use (cpu.register.l)

                6 -> do
                    nn <- use (cpu.register.hl)
                    cpu.register.a <~ use (mmu.addr nn)

                    cpu.tclock += 4

                7 -> cpu.register.a <~ use (cpu.register.a)
                _ -> error "`Impossible` error"
            
        i | i .&. 0xF8 == 0x98 -> do
            cpu.tclock += 4

            sbc =<< case extractOctalArg 0 i of
                0 -> use (cpu.register.b)
                1 -> use (cpu.register.c)
                2 -> use (cpu.register.d)
                3 -> use (cpu.register.e)
                4 -> use (cpu.register.h)
                5 -> use (cpu.register.l)

                6 -> do
                    cpu.tclock += 4

                    nn <- use (cpu.register.hl)
                    use (mmu.addr nn)

                7 -> use (cpu.register.a)
                _ -> error "`Impossible` error"

        i | i .&. 0xF8 == 0x50 -> do
            cpu.tclock += 4

            case extractOctalArg 0 i of
                0 -> cpu.register.d <~ use (cpu.register.b)
                1 -> cpu.register.d <~ use (cpu.register.c)
                2 -> cpu.register.d <~ use (cpu.register.d)
                3 -> cpu.register.d <~ use (cpu.register.e)
                4 -> cpu.register.d <~ use (cpu.register.h)
                5 -> cpu.register.d <~ use (cpu.register.l)

                6 -> do
                    nn <- use (cpu.register.hl)
                    cpu.register.d <~ use (mmu.addr nn)

                    cpu.tclock += 4

                7 -> cpu.register.d <~ use (cpu.register.a)
                _ -> error "`impossible` error"

        i | i .&. 0xF8 == 0x60 -> do
            case extractOctalArg 0 i of
                0 -> cpu.register.h <~ use (cpu.register.b)
                1 -> cpu.register.h <~ use (cpu.register.c)
                2 -> cpu.register.h <~ use (cpu.register.d)
                3 -> cpu.register.h <~ use (cpu.register.e)
                4 -> cpu.register.h <~ use (cpu.register.h)
                5 -> cpu.register.h <~ use (cpu.register.l)
                6 -> do
                    nn <- use (cpu.register.hl)
                    cpu.register.h <~ use (mmu.addr nn)
                7 -> cpu.register.h <~ use (cpu.register.a)
                _ -> error "`impossible` error"

            case extractOctalArg 0 i of
                6 -> cpu.tclock += 8
                _ -> cpu.tclock += 4

        0xEA -> do
            nn <- consumeWord
            mmu.addr nn <~ use (cpu.register.a)

            cpu.tclock += 16

        0xF0 -> do
            v <- consumeByte
            cpu.register.a <~ use (mmu.addr (0xFF00 + fromIntegral v))

        0xBE -> do
            nn <- use (cpu.register.hl)
            cmp =<< use (mmu.addr nn)
            cpu.tclock += 8

        0x86 -> do
            nn <- use (cpu.register.hl)
            add =<< use (mmu.addr nn)
            cpu.tclock += 8

        0xFE -> do
            cmp =<< consumeByte
            cpu.tclock += 8

        instr -> error $ "Unimplemented instruction: 0x" ++ showHex instr ""

extractOctalArg :: (Bits a, Num a) => Int -> a -> a
extractOctalArg i v = shiftR v i .&. 7

halfCarry :: (Bits a, Num a) => a -> a -> Bool
halfCarry v1 v2 = (((v1 .&. 0x0F) + (v2 .&. 0x0F)) .&. 0x10) == 0x10
