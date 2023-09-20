{-# LANGUAGE LambdaCase #-}

module HaskBoy.Cpu.Execution where

import HaskBoy.Emulator

import HaskBoy.Mmu
import HaskBoy.Cpu
import HaskBoy.Cpu.Instructions

import Control.Lens
import Control.Monad.State.Strict

import Data.Word (Word8)
import Data.Bits (Bits((.&.), shiftR))

import Debug.Trace (trace, traceM)
import Numeric (showHex)

data Instruction
    = Nop
    | XorA (ALens' Emulator Word8)
    | Ld (ALens' Emulator Word8) (ALens' Emulator Word8)
    | Store (ALens' Emulator Word8)
    | Inc (ALens' Emulator Word8)
    | Dec (ALens' Emulator Word8)
    | Sub (ALens' Emulator Word8)
    | Sbc (ALens' Emulator Word8)

execute :: Word8 -> State Emulator ()
execute = \case
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

        0xF3 -> do
            cpu.interruptEnable .= False
            cpu.tclock += 4

        0xFE -> do
            cmp =<< consumeByte
            cpu.tclock += 8

        instr -> error $ "Unimplemented instruction: 0x" ++ showHex instr ""

execute' :: Instruction -> State Emulator ()
execute' = \case
        Nop  -> pure ()

        Ld rr lr -> cloneLens rr <~ use (cloneLens lr)

        Store r -> do
            v <- consumeByte
            cloneLens r .= v
        
        XorA r -> xorA =<< use (cloneLens r)

        Inc r -> inc (cloneLens r)
        Dec r -> dec (cloneLens r)

        Sub r -> sub =<< use (cloneLens r)
        Sbc r -> sbc =<< use (cloneLens r)

toInstruction :: Word8 -> State Emulator Instruction
toInstruction = \case
        0x00 -> do
            cpu.tclock += 4
            pure Nop

        i | i .&. 0xC7 == 0x04 -> do
            cpu.tclock += 4

            r <- argToRegister (extractOctalArg 3 i) >>= \case
                Right r -> pure (cpu.register.r)
                Left  r -> do
                    cpu.tclock += 8
                    pure (mmu.r)

            pure (Dec r)

        i | i .&. 0xC7 == 0x05 -> do
            cpu.tclock += 4

            r <- argToRegister (extractOctalArg 3 i) >>= \case
                Right r -> pure (cpu.register.r)
                Left  r -> do
                    cpu.tclock += 8
                    pure (mmu.r)

            pure (Dec r)

        i | i .&. 0xF8 == 0x90 -> do
            cpu.tclock += 4

            r <- argToRegister (extractOctalArg 0 i) >>= \case
                Right r -> pure (cpu.register.r)
                Left  r -> do
                    cpu.tclock += 4
                    pure (mmu.r)

            pure (Sub r)

        i | i .&. 0xF8 == 0x98 -> do
            cpu.tclock += 4

            r <- argToRegister (extractOctalArg 0 i) >>= \case
                Right r -> pure (cpu.register.r)
                Left  r -> do
                    cpu.tclock += 4
                    pure (mmu.r)

            pure (Sbc r)

        i | i .&. 0xF8 == 0xA8 -> do
            cpu.tclock += 4

            r <- argToRegister (extractOctalArg 0 i) >>= \case
                Right r -> pure (cpu.register.r)
                Left  r -> do
                    cpu.tclock += 4
                    pure (mmu.r)

            pure (XorA r)

        i | i .&. 0xC7 == 0x06 -> do
            cpu.tclock += 8

            r <- argToRegister (extractOctalArg 3 i) >>= \case
                Right r -> pure (cpu.register.r)
                Left  r -> do
                    cpu.tclock += 4
                    pure (mmu.r)

            pure (Store r)

        i | i .&. 0xF8 == 0x40 -> do
            cpu.tclock += 4

            r <- argToRegister (extractOctalArg 0 i) >>= \case
                Right r -> pure (cpu.register.r)
                Left  r -> do
                    cpu.tclock += 4
                    pure (mmu.r)
            
            pure (Ld (cpu.register.b) r)

        i | i .&. 0xF8 == 0x50 -> do
            cpu.tclock += 4

            r <- argToRegister (extractOctalArg 0 i) >>= \case
                Right r -> pure (cpu.register.r)
                Left  r -> do
                    cpu.tclock += 4
                    pure (mmu.r)
            
            pure (Ld (cpu.register.d) r)

        i | i .&. 0xF8 == 0x60 -> do
            cpu.tclock += 4

            r <- argToRegister (extractOctalArg 0 i) >>= \case
                Right r -> pure (cpu.register.r)
                Left  r -> do
                    cpu.tclock += 4
                    pure (mmu.r)
            
            pure (Ld (cpu.register.h) r)

        i | i .&. 0xF8 == 0x78 -> do
            cpu.tclock += 4

            r <- argToRegister (extractOctalArg 0 i) >>= \case
                Right r -> pure (cpu.register.r)
                Left  r -> do
                    cpu.tclock += 4
                    pure (mmu.r)
            
            pure (Ld (cpu.register.a) r)

        instr -> error $ "Unimplemented instruction: 0x" ++ showHex instr ""

argToRegister :: Word8 -> State Emulator (Either (ALens' Mmu Word8) (ALens' Register Word8))
argToRegister 0 = pure $ Right b
argToRegister 1 = pure $ Right c
argToRegister 2 = pure $ Right d
argToRegister 3 = pure $ Right e
argToRegister 4 = pure $ Right h
argToRegister 5 = pure $ Right l

argToRegister 6 = do
    nn <- use (cpu.register.hl)
    pure $ Left (addr nn)

argToRegister 7 = pure $ Right a
argToRegister _ = undefined

extractOctalArg :: (Bits a, Num a) => Int -> a -> a
extractOctalArg i v = shiftR v i .&. 7

halfCarry :: (Bits a, Num a) => a -> a -> Bool
halfCarry v1 v2 = (((v1 .&. 0x0F) + (v2 .&. 0x0F)) .&. 0x10) == 0x10
