{-# LANGUAGE LambdaCase #-}

module HaskBoy.Cpu.Execution where

import HaskBoy.Emulator

import HaskBoy.Mmu
import HaskBoy.Cpu
import HaskBoy.Cpu.Instructions

import Control.Lens
import Control.Monad.State.Strict

import Data.Word (Word8, Word16)
import Data.Bits (Bits((.&.), shiftR, complement))

import Numeric (showHex)

data Instruction
    = Nop
    | Xor (ALens' Emulator Word8)
    | Or (ALens' Emulator Word8)
    | Cpl
    | And Value
    | Ld (ALens' Emulator Word8) (ALens' Emulator Word8)
    | AHLI | HLIA
    | AHLD | HLDA
    | Store (ALens' Emulator Word8) Word8
    | Store16 (ALens' Emulator Word16) Word16
    | Inc (ALens' Emulator Word8)
    | Inc16 (ALens' Registers Word16)
    | Dec (ALens' Emulator Word8)
    | Dec16 (ALens' Registers Word16)
    | Add (ALens' Emulator Word8)
    | Sub (ALens' Emulator Word8)
    | Sbc (ALens' Emulator Word8)
    | Bit Int Word8
    | Cmp Word8
    | Jmp Word16
    | Jr Bool
    | Push Word16
    | Pop (ALens' Registers Word16)
    | PopAF
    | Call Word16
    | Ret
    | RetNZ
    | EnableInterrupt
    | DisableInterrupt

data Value
    = Register (ALens' Cpu Word8)
    | Address  (ALens' Mmu Word8)
    | Byte Word8

execute :: Instruction -> State Emulator ()
execute = \case
        Nop  -> pure ()

        Ld   lhs rhs -> cloneLens lhs <~ use (cloneLens rhs)

        Store   r v -> cloneLens r .= v
        Store16 r v -> cloneLens r .= v

        Xor r -> zoom (cpu.register) . xor =<< use (cloneLens r)
        Or r -> zoom (cpu.register) . byteOr =<< use (cloneLens r)

        Cpl -> do
            cpu.register.a %= complement
            cpu.register.hcarry .= True
            cpu.register.subOp .= True

        And (Byte v) -> zoom (cpu.register) (byteAnd v)
        And (Register r) -> zoom (cpu.register) . byteAnd =<< use (cpu.cloneLens r)
        And (Address v) -> zoom (cpu.register) . byteAnd =<< use (mmu.cloneLens v)

        Inc r -> inc (cloneLens r)
        Dec r -> dec (cloneLens r)
        Dec16 r -> cpu.register.cloneLens r -= 1

        Add r -> zoom (cpu.register) . add =<< use (cloneLens r)

        Sub r -> zoom (cpu.register) . sub =<< use (cloneLens r)
        Sbc r -> sbc =<< use (cloneLens r)

        Bit n v -> zoom (cpu.register) (bit n v)

        AHLI -> do
            nn <- use (cpu.register.hl)
            cpu.register.a <~ use (mmu.addr nn)

            cpu.register.hl += 1

        AHLD -> do
            nn <- use (cpu.register.hl)
            cpu.register.a <~ use (mmu.addr nn)

            cpu.register.hl -= 1

        HLIA -> do
            nn <- use (cpu.register.hl)
            mmu.addr nn <~ use (cpu.register.a)

            cpu.register.hl += 1

        HLDA -> do
            nn <- use (cpu.register.hl)
            mmu.addr nn <~ use (cpu.register.a)

            cpu.register.hl -= 1

        Inc16 r -> cpu.register.cloneLens r += 1

        Cmp v -> zoom (cpu.register) $ cmp v

        Jr v -> jr v
        Jmp v -> cpu.register.pc .= v

        Push v -> pushStack v
        Pop r -> cpu.register.cloneLens r <~ popStack

        PopAF -> do
            v <- use (cpu.register.flag)
            cpu.register.af <~ popStack
            cpu.register.flag .= v

        Call v -> call v
        Ret -> ret

        RetNZ -> do
            z <- use (cpu.register.zero)

            if z then
                cpu.tclock += 8
            else do
                cpu.tclock += 20
                ret

        EnableInterrupt -> cpu.interruptEnable .= True
        DisableInterrupt -> cpu.interruptEnable .= False

toInstruction :: Word8 -> State Emulator Instruction
toInstruction = \case
        0x00 -> do
            cpu.tclock += 4
            pure Nop

        0x01 -> do
            cpu.tclock += 12
            Store16 (cpu.register.bc) <$> consumeWord

        0x02 -> do
            cpu.tclock += 8

            nn <- use (cpu.register.bc)
            pure (Ld (mmu.addr nn) (cpu.register.a))

        0x03 -> do
            cpu.tclock += 8
            pure (Inc16 bc)

        i | i .&. 0xC7 == 0x04 -> do
            cpu.tclock += 4

            r <- argToRegister (extractOctalArg 3 i) >>= \case
                Right r -> pure (cpu.register.r)
                Left  r -> do
                    cpu.tclock += 8
                    pure (mmu.r)

            pure (Inc r)

        i | i .&. 0xC7 == 0x05 -> do
            cpu.tclock += 4

            r <- argToRegister (extractOctalArg 3 i) >>= \case
                Right r -> pure (cpu.register.r)
                Left  r -> do
                    cpu.tclock += 8
                    pure (mmu.r)

            pure (Dec r)

        i | i .&. 0xC7 == 0x06 -> do
            cpu.tclock += 8

            r <- argToRegister (extractOctalArg 3 i) >>= \case
                Right r -> pure (cpu.register.r)
                Left  r -> do
                    cpu.tclock += 4
                    pure (mmu.r)

            Store r <$> consumeByte

        0x0B -> do
            cpu.tclock += 8
            pure (Dec16 bc)

        0x13 -> do
            cpu.tclock += 8
            pure (Inc16 de)

        0x23 -> do
            cpu.tclock += 8
            pure (Inc16 hl)

        0x33 -> do
            cpu.tclock += 8
            pure (Inc16 sp)

        i | i .&. 0xF8 == 0x40 -> do
            cpu.tclock += 4

            r <- argToRegister (extractOctalArg 0 i) >>= \case
                Right r -> pure (cpu.register.r)
                Left  r -> do
                    cpu.tclock += 4
                    pure (mmu.r)
            
            pure (Ld (cpu.register.b) r)

        i | i .&. 0xF8 == 0x48 -> do
            cpu.tclock += 4

            r <- argToRegister (extractOctalArg 0 i) >>= \case
                Right r -> pure (cpu.register.r)
                Left  r -> do
                    cpu.tclock += 4
                    pure (mmu.r)

            pure (Ld (cpu.register.c) r)

        i | i .&. 0xF8 == 0x50 -> do
            cpu.tclock += 4

            r <- argToRegister (extractOctalArg 0 i) >>= \case
                Right r -> pure (cpu.register.r)
                Left  r -> do
                    cpu.tclock += 4
                    pure (mmu.r)
            
            pure (Ld (cpu.register.d) r)

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

            pure (Xor r)

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

        0x12 -> do
            cpu.tclock += 8

            nn <- use (cpu.register.de)
            pure (Ld (mmu.addr nn) (cpu.register.a))

        0x22 -> do
            cpu.tclock += 8
            pure HLIA

        0x31 -> do
            cpu.tclock += 12
            Store16 (cpu.register.sp) <$> consumeWord

        0x32 -> do
            cpu.tclock += 8
            pure HLDA

        0x0A -> do
            cpu.tclock += 8

            nn <- use (cpu.register.bc)
            pure (Ld (cpu.register.a) (mmu.addr nn))

        0x1A -> do
            cpu.tclock += 8

            nn <- use (cpu.register.de)
            pure (Ld (cpu.register.a) (mmu.addr nn))

        0x2A -> do
            cpu.tclock += 8
            pure AHLI

        0x3A -> do
            cpu.tclock += 8
            pure AHLD

        i | i .&. 0xF8 == 0x86 -> do
            cpu.tclock += 4

            r <- argToRegister (extractOctalArg 0 i) >>= \case
                Right r -> pure (cpu.register.r)
                Left  r -> do
                    cpu.tclock += 4
                    pure (mmu.r)

            pure (Add r)

        0x18 -> pure (Jr True)
        0x20 -> Jr . not <$> use (cpu.register.zero)
        0x28 -> Jr <$> use (cpu.register.zero)

        0x11 -> do
            cpu.tclock += 12
            Store16 (cpu.register.de) <$> consumeWord

        0x21 -> do
            cpu.tclock += 12
            Store16 (cpu.register.hl) <$> consumeWord

        0x2F -> do
            cpu.tclock += 4
            pure Cpl

        0x77 -> do
            cpu.tclock += 8

            nn <- use (cpu.register.hl)
            pure (Ld (mmu.addr nn) (cpu.register.a))

        i | i .&. 0xF8 == 0x80 -> do
            cpu.tclock += 4

            r <- argToRegister (extractOctalArg 0 i) >>= \case
                Right r -> pure (cpu.register.r)
                Left  r -> do
                    cpu.tclock += 4
                    pure (mmu.r)

            pure (Add r)

        i | i .&. 0xF8 == 0xA0 -> do
            cpu.tclock += 4

            argToRegister (extractOctalArg 0 i) >>= \case
                Right r -> pure . And $ Register (register.r)
                Left  r -> do
                    cpu.tclock += 4
                    pure (And . Address $ r)

        i | i .&. 0xF8 == 0xB0 -> do
            cpu.tclock += 4

            r <- argToRegister (extractOctalArg 0 i) >>= \case
                Right r -> pure (cpu.register.r)
                Left  r -> do
                    cpu.tclock += 4
                    pure (mmu.r)

            pure (Or r)

        0xC0 -> pure RetNZ

        0xC1 -> do
            cpu.tclock += 12
            pure (Pop bc)

        0xC3 -> do
            cpu.tclock += 16
            Jmp <$> consumeWord

        0xC5 -> do
            cpu.tclock += 16
            Push <$> use (cpu.register.bc)

        0xC9 -> do
            cpu.tclock += 16
            pure Ret

        0xCB -> consumeByte >>= \case
            0x7C -> do
                cpu.tclock += 8
                Bit 7 <$> use (cpu.register.h)

            arg -> error $ "Invalid CB argument: " ++ showHex arg ""

        0xCD -> do
            cpu.tclock += 24
            Call <$> consumeWord

        0xE0 -> do
            cpu.tclock += 12

            v <- fromIntegral <$> consumeByte
            pure (Ld (mmu.addr (0xFF00 + v)) (cpu.register.a))

        0xE1 -> do
            cpu.tclock += 12
            pure (Pop hl)

        0xE2 -> do
            cpu.tclock += 8
            v <- fromIntegral <$> use (cpu.register.c)
            pure (Ld (mmu.addr (0xFF00 + v)) (cpu.register.a))

        0xE5 -> do
            cpu.tclock += 16
            Push <$> use (cpu.register.hl)

        0xE6 -> do
            cpu.tclock += 8
            And . Byte <$> consumeByte

        0xEA -> do
            cpu.tclock += 16

            v <- consumeWord
            pure (Ld (mmu.addr v) (cpu.register.a))

        0xF0 -> do
            cpu.tclock += 12

            v <- fromIntegral <$> consumeByte
            pure (Ld (cpu.register.a) (mmu.addr (0xFF00 + v)))

        0xF1 -> do
            cpu.tclock += 12
            pure PopAF

        0xF3 -> do
            cpu.tclock += 4
            pure DisableInterrupt

        0xF5 -> do
            cpu.tclock += 16
            Push <$> use (cpu.register.af)

        0xFA -> do
            cpu.tclock += 16

            v <- consumeWord
            pure (Ld (cpu.register.a) (mmu.addr v))

        0xFB -> do
            cpu.tclock += 4
            pure EnableInterrupt

        0xFE -> do
            cpu.tclock += 8
            Cmp <$> consumeByte

        instr -> error $ "Unimplemented instruction: 0x" ++ showHex instr ""

argToRegister :: Word8 -> State Emulator (Either (ALens' Mmu Word8) (ALens' Registers Word8))
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
