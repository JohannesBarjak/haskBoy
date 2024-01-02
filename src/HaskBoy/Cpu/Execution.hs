{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE GADTs           #-}

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
    | Xor (Argument Word8)
    | Or (Argument Word8)
    | Cpl
    | And (Argument Word8)
    | Ld (Argument Word8) (Argument Word8)
    | Store16 (ALens' Emulator Word16) Word16
    | Inc (Argument Word8)
    | Inc16 (ALens' Registers Word16)
    | Dec (Argument Word8)
    | Dec16 (ALens' Registers Word16)
    | Add (Argument Word8)
    | Sub (Argument Word8)
    | Sbc (Argument Word8)
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

data Argument a where
    Register :: (ALens' Cpu a) -> Argument a
    Address :: (ALens' Mmu a) -> Argument a

execute :: Instruction -> State Emulator ()
execute = \case
        Nop -> mcycle 1

        Ld lhs rhs -> mcycle 1 *> case lhs of
                Register lr -> do
                    case rhs of
                        Register rr -> cpu.cloneLens lr <~ use (cpu.cloneLens rr)
                        Address av -> do
                            mcycle 1
                            cpu.cloneLens lr <~ use (mmu.cloneLens av)

                Address v -> mcycle 1 *> case rhs of
                        Register r -> mmu.cloneLens v <~ use (cpu.cloneLens r)
                        Address av -> do
                            mcycle 1
                            mmu.cloneLens v <~ use (mmu.cloneLens av)

        Store16 r v -> cloneLens r .= v

        Xor bs -> mcycle 1 *> case bs of
                Register r -> xor =<< use (cpu.cloneLens r)
                Address v -> mcycle 1 *> (xor =<< use (mmu.cloneLens v))

        Or bs -> mcycle 1 *> case bs of
                Register r -> byteOr =<< use (cpu.cloneLens r)
                Address v -> mcycle 1 *> (byteOr =<< use (mmu.cloneLens v))

        Cpl -> do
            cpu.register.a %= complement
            cpu.register.hcarry .= True
            cpu.register.subOp .= True

        And bs -> mcycle 1 *> case bs of
                Register r -> byteAnd =<< use (cpu.cloneLens r)
                Address v -> mcycle 1 *> (byteAnd =<< use (mmu.cloneLens v))

        Inc bs -> mcycle 1 *> case bs of
                Register r -> inc (cpu.r)
                Address av -> mcycle 2 *> inc (mmu.av)

        Dec bs -> mcycle 1 *> case bs of
                Register r -> dec (cpu.r)
                Address av -> mcycle 2 *> dec (mmu.av)

        Dec16 r -> do
            mcycle 2
            cpu.register.cloneLens r -= 1

        Add bs -> mcycle 1 *> case bs of
                Register r -> add =<< use (cpu.cloneLens r)
                Address av -> mcycle 1 *> (add =<< use (mmu.cloneLens av))

        Sub bs -> mcycle 1 *> case bs of
            Register r -> sub =<< use (cpu.cloneLens r)
            Address av -> mcycle 1 *> (sub =<< use (mmu.cloneLens av))

        Sbc v -> mcycle 1 *> case v of
            Register r -> sbc =<< use (cpu.cloneLens r)
            Address av -> mcycle 1 *> (sbc =<< use (mmu.cloneLens av))

        Bit n v -> zoom (cpu.register) (bit n v)

        Inc16 r -> do
            mcycle 2
            cpu.register.cloneLens r += 1

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

        Ret mk -> mcycle 2 *> case mk of
                Just k -> do
                    mcycle 3
                    zoom cpu (condition k) >>= flip when ret

                Nothing -> mcycle 2 *> ret

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
        0x00 -> pure Nop

        0x01 -> do
            cpu.tclock += 12
            Store16 (cpu.register.bc) <$> consumeWord

        0x02 -> do
            nn <- use (cpu.register.bc)
            pure $ Ld (Address $ addr nn) (Register $ register.a)

        0x03 -> pure (Inc16 bc)

        i | i .&. 0xC7 == 0x04 -> Inc <$> toArgument (extractOctalArg 3 i)
        i | i .&. 0xC7 == 0x05 -> Dec <$> toArgument (extractOctalArg 3 i)

        i | i .&. 0xC7 == 0x06 ->
            Ld <$> toArgument (extractOctalArg 3 i)
               <*> fmap (Address . addr) (cpu.register.pc <<+= 1)

        0x0B -> pure (Dec16 bc)
        0x13 -> pure (Inc16 de)
        0x1B -> pure (Dec16 de)
        0x2B -> pure (Dec16 hl)
        0x23 -> pure (Inc16 hl)
        0x33 -> pure (Inc16 sp)
        0x3B -> pure (Dec16 sp)

        i | i .&. 0xF8 == 0x40 -> Ld (Register $ register.b) <$> toArgument (extractOctalArg 0 i)
        i | i .&. 0xF8 == 0x48 -> Ld (Register $ register.c) <$> toArgument (extractOctalArg 0 i)
        i | i .&. 0xF8 == 0x50 -> Ld (Register $ register.d) <$> toArgument (extractOctalArg 0 i)
        i | i .&. 0xF8 == 0x58 -> Ld (Register $ register.e) <$> toArgument (extractOctalArg 0 i)
        i | i .&. 0xF8 == 0x60 -> Ld (Register $ register.h) <$> toArgument (extractOctalArg 0 i)
        i | i .&. 0xF8 == 0x68 -> Ld (Register $ register.l) <$> toArgument (extractOctalArg 0 i)

        i | i .&. 0xF8 == 0x70 -> do
            v <- use (cpu.register.hl)
            Ld (Address $ addr v) <$> toArgument (extractOctalArg 0 i)

        i | i .&. 0xF8 == 0x78 -> Ld (Register $ register.a) <$> toArgument (extractOctalArg 0 i)

        i | i .&. 0xF8 == 0x90 -> Sub <$> toArgument (extractOctalArg 0 i)
        i | i .&. 0xF8 == 0x98 -> Sbc <$> toArgument (extractOctalArg 0 i)
        i | i .&. 0xF8 == 0xA8 -> Xor <$> toArgument (extractOctalArg 0 i)

        0x12 -> do
            nn <- use (cpu.register.de)
            pure $ Ld (Address $ addr nn) (Register $ register.a)

        0x22 -> do
            nn <- cpu.register.hl <<+= 1
            pure $ Ld (Address $ addr nn) (Register $ register.a)

        0x31 -> do
            cpu.tclock += 12
            Store16 (cpu.register.sp) <$> consumeWord

        0x32 -> do
            nn <- cpu.register.hl <<-= 1
            pure $ Ld (Address $ addr nn) (Register $ register.a)

        0x0A -> do
            nn <- use (cpu.register.bc)
            pure $ Ld (Register $ register.a) (Address $ addr nn)

        0x1A -> do
            nn <- use (cpu.register.de)
            pure $ Ld (Register $ register.a) (Address $ addr nn)

        0x2A -> Ld (Register $ register.a) . Address . addr <$> (cpu.register.hl <<+= 1)
        0x3A -> Ld (Register $ register.a) . Address . addr <$> (cpu.register.hl <<-= 1)

        i | i .&. 0xF8 == 0x80 -> Add <$> toArgument (extractOctalArg 0 i)

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
            nn <- use (cpu.register.hl)
            pure $ Ld (Address $ addr nn) (Register $ register.a)

        i | i .&. 0xF8 == 0xA0 -> And <$> toArgument (extractOctalArg 0 i)
        i | i .&. 0xF8 == 0xB0 -> Or <$> toArgument (extractOctalArg 0 i)

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

        0xC6 -> Add . Address . addr <$> (cpu.register.pc <<+= 1)

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
        0xD6 -> Sub . Address . addr <$> (cpu.register.pc <<+= 1)
        0xDE -> Sbc . Address . addr <$> (cpu.register.pc <<+= 1)

        0xE0 -> do
            mcycle 1
            v <- fromIntegral <$> consumeByte
            pure $ Ld (Address $ addr (0xFF00 + v)) (Register $ register.a)

        0xE1 -> do
            cpu.tclock += 12
            pure (Pop hl)

        0xE2 -> do
            v <- fromIntegral <$> use (cpu.register.c)
            pure $ Ld (Address $ addr (0xFF00 + v)) (Register $ register.a)

        0xE5 -> do
            cpu.tclock += 16
            Push <$> use (cpu.register.hl)

        0xE6 -> And . Address . addr <$> (cpu.register.pc <<+= 1)

        0xE9 -> do
            cpu.tclock += 4
            av <- use (cpu.register.hl)
            pure (Jmp av)

        0xEA -> do
            mcycle 2
            v <- consumeWord
            pure $ Ld (Address $ addr v) (Register $ register.a)

        0xF0 -> do
            mcycle 1
            v <- fromIntegral <$> consumeByte
            pure $ Ld (Register $ register.a) (Address $ addr (0xFF00 + v))

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
            mcycle 2
            Ld (Register $ register.a) . Address . addr <$> consumeWord

        0xFB -> do
            cpu.tclock += 4
            pure EnableInterrupt

        0xFE -> do
            cpu.tclock += 8
            Cmp <$> consumeByte

        instr -> error $ "Unimplemented instruction: 0x" ++ showHex instr ""

toArgument :: Word8 -> State Emulator (Argument Word8)
toArgument 0 = pure $ Register (register.b)
toArgument 1 = pure $ Register (register.c)
toArgument 2 = pure $ Register (register.d)
toArgument 3 = pure $ Register (register.e)
toArgument 4 = pure $ Register (register.h)
toArgument 5 = pure $ Register (register.l)

toArgument 6 = do
    nn <- use (cpu.register.hl)
    pure $ Address (addr nn)

toArgument 7 = pure $ Register (register.a)
toArgument _ = undefined

extractOctalArg :: (Bits a, Num a) => Int -> a -> a
extractOctalArg i v = shiftR v i .&. 7
