{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE LambdaCase      #-}

module HaskBoy.Cpu.Execution (execute, toInstruction) where

import HaskBoy.Emulator

import HaskBoy.Mmu
import HaskBoy.Cpu
import HaskBoy.Cpu.Instructions

import Control.Lens
import Control.Monad.State.Strict

import Data.Word (Word8, Word16)
import Data.Bits (Bits((.&.), shiftR, complement))
import Data.Mod

import Numeric (showHex)

data Instruction
    = Nop
    | Xor ByteSource
    | Or ByteSource
    | Cpl
    | And ByteSource
    | Ld (ALens' Emulator Word8) (ALens' Emulator Word8)
    | Ld' ByteSource ByteSource
    | AHLI | HLIA
    | AHLD | HLDA
    | Store (ALens' Emulator Word8) Word8
    | Store16 (ALens' Emulator Word16) Word16
    | Inc ByteSource
    | Inc16 (ALens' Registers Word16)
    | Dec ByteSource
    | Dec16 (ALens' Registers Word16)
    | Add ByteSource
    | Sub ByteSource
    | Sbc ByteSource
    | Bit Int Word8
    | Cmp Word8
    | Jmp Word16
    | Jr Bool
    | Push Word16
    | Pop (ALens' Registers Word16)
    | PopAF
    | Call Word16
    | Ret (Maybe Condition)
    | EnableInterrupt
    | DisableInterrupt

data Condition
    = Z | NZ
    | C | NC

data ByteSource
    = Register (ALens' Cpu Word8)
    | Address  (ALens' Mmu Word8)
    | Byte Word8

data WordSource
    = Register16 (ALens' Cpu Word16)
    | Address16  (ALens' Mmu Word16)
    | Word Word16

makePrisms ''ByteSource
makePrisms ''WordSource
makePrisms ''Instruction

execute :: Instruction -> State Emulator ()
execute = \case
        Nop -> pure ()

        Ld lhs rhs -> cloneLens lhs <~ use (cloneLens rhs)

        Ld' lhs rhs -> do
            mcycle 1

            case lhs of
                Register lr -> do
                    case rhs of
                        Register rr -> cpu.cloneLens lr <~ use (cpu.cloneLens rr)
                        Address v -> do
                            mcycle 1
                            cpu.cloneLens lr <~ use (mmu.cloneLens v)
                Address v -> do
                    mcycle 1
                    case rhs of
                        Register r -> mmu.cloneLens v <~ use (cpu.cloneLens r)

        Store   r v -> cloneLens r .= v
        Store16 r v -> cloneLens r .= v

        Xor bs -> mcycle 1 *> case bs of
                Register r -> xor =<< use (cpu.cloneLens r)
                Address v -> mcycle 1 *> (xor =<< use (mmu.cloneLens v))
                Byte v -> mcycle 1 *> xor v

        Or bs -> mcycle 1 *> case bs of
                Register r -> byteOr =<< use (cpu.cloneLens r)
                Address v -> mcycle 1 *> (byteOr =<< use (mmu.cloneLens v))
                Byte v -> mcycle 1 *> byteOr v

        Cpl -> do
            cpu.register.a %= complement
            cpu.register.hcarry .= True
            cpu.register.subOp .= True

        And bs -> mcycle 1 *> case bs of
                Register r -> byteAnd =<< use (cpu.cloneLens r)
                Address v -> mcycle 1 *> (byteAnd =<< use (mmu.cloneLens v))
                Byte v -> do mcycle 1 *> byteAnd v

        Inc bs -> mcycle 1 *> case bs of
                Register r -> inc (cpu.r)
                Address av -> mcycle 2 *> inc (mmu.av)
                _ -> pure ()

        Dec bs -> mcycle 1 *> case bs of
                Register r -> dec (cpu.r)
                Address av -> mcycle 2 *> dec (mmu.av)
                _ -> pure ()

        Dec16 r -> cpu.register.cloneLens r -= 1

        Add bs -> mcycle 1 *> case bs of
                Register r -> add =<< use (cpu.cloneLens r)
                Address av -> mcycle 1 *> (add =<< use (mmu.cloneLens av))
                Byte v -> mcycle 1 *> add v

        Sub bs -> mcycle 1 *> case bs of
            Register r -> sub =<< use (cpu.cloneLens r)
            Address av -> mcycle 1 *> (sub =<< use (mmu.cloneLens av))
            Byte v -> mcycle 1 *> sub v

        Sbc v -> mcycle 1 *> case v of
            Register r -> sbc =<< use (cpu.cloneLens r)
            Address av -> mcycle 1 *> (sbc =<< use (mmu.cloneLens av))
            Byte bv -> mcycle 1 *> sbc bv

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

        Ret mk -> do
            cpu.tclock += 8

            case mk of
                Just k  -> do
                    cpu.tclock += 12

                    zoom cpu (condition k)
                        >>= flip when ret
                Nothing -> do
                    cpu.tclock += 8
                    ret

        EnableInterrupt -> cpu.interruptEnable .= True
        DisableInterrupt -> cpu.interruptEnable .= False

mcycle :: Integer -> State Emulator ()
mcycle v = cpu.tclock += (v * 4)

condition :: Condition -> State Cpu Bool
condition  C = use (register.carry)
condition NC = not <$> use (register.carry)
condition  Z = use (register.zero)
condition NZ = not <$> use (register.zero)

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

        i | i .&. 0xC7 == 0x04 -> Inc <$> argToByteSource (extractOctalArg 3 i)
        i | i .&. 0xC7 == 0x05 -> Dec <$> argToByteSource (extractOctalArg 3 i)

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

        0x2B -> do
            cpu.tclock += 8
            pure (Dec16 hl)

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

        i | i .&. 0xF8 == 0x58 -> Ld' (Register $ register.e) <$> argToByteSource (extractOctalArg 0 i)
        i | i .&. 0xF8 == 0x68 -> Ld' (Register $ register.l) <$> argToByteSource (extractOctalArg 0 i)

        i | i .&. 0xF8 == 0x70 -> do
            v <- use (cpu.register.hl)
            Ld' (Address $ addr v) <$> argToByteSource (extractOctalArg 0 i)

        i | i .&. 0xF8 == 0x90 -> Sub <$> argToByteSource (extractOctalArg 0 i)
        i | i .&. 0xF8 == 0x98 -> Sbc <$> argToByteSource (extractOctalArg 0 i)
        i | i .&. 0xF8 == 0xA8 -> Xor <$> argToByteSource (extractOctalArg 0 i)

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

        i | i .&. 0xF8 == 0x80 -> Add <$> argToByteSource (extractOctalArg 0 i)

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

        0x38 -> Jr <$> use (cpu.register.carry)

        0x77 -> do
            cpu.tclock += 8

            nn <- use (cpu.register.hl)
            pure (Ld (mmu.addr nn) (cpu.register.a))

        i | i .&. 0xF8 == 0xA0 -> And <$> argToByteSource (extractOctalArg 0 i)
        i | i .&. 0xF8 == 0xB0 -> Or <$> argToByteSource (extractOctalArg 0 i)

        0xC0 -> pure $ Ret (Just NZ)

        0xC1 -> do
            cpu.tclock += 12
            pure (Pop bc)

        0xC3 -> do
            cpu.tclock += 16
            Jmp <$> consumeWord

        0xC5 -> do
            cpu.tclock += 16
            Push <$> use (cpu.register.bc)

        0xC6 -> Add . Byte <$> consumeByte

        0xC9 -> do
            cpu.tclock += 16
            pure (Ret Nothing)

        0xCB -> consumeByte >>= \case
            0x7C -> do
                cpu.tclock += 8
                Bit 7 <$> use (cpu.register.h)

            arg -> error $ "Invalid CB argument: " ++ showHex arg ""

        0xCD -> do
            cpu.tclock += 24
            Call <$> consumeWord

        0xD0 -> pure $ Ret (Just NC)
        0xD6 -> Sub . Byte <$> consumeByte
        0xDE -> Sbc . Byte <$> consumeByte

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

        0xE6 -> And . Byte <$> consumeByte

        0xE9 -> do
            cpu.tclock += 4
            av <- use (cpu.register.hl)
            pure (Jmp av)

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

argToByteSource :: Word8 -> State Emulator ByteSource
argToByteSource 0 = pure $ Register (register.b)
argToByteSource 1 = pure $ Register (register.c)
argToByteSource 2 = pure $ Register (register.d)
argToByteSource 3 = pure $ Register (register.e)
argToByteSource 4 = pure $ Register (register.h)
argToByteSource 5 = pure $ Register (register.l)

argToByteSource 6 = do
    nn <- use (cpu.register.hl)
    pure $ Address (addr nn)

argToByteSource 7 = pure $ Register (register.a)
argToByteSource _ = undefined

extractOctalArg :: (Bits a, Num a) => Int -> a -> a
extractOctalArg i v = shiftR v i .&. 7
