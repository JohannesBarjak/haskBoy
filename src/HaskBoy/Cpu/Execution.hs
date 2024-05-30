{-# LANGUAGE DataKinds  #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs      #-}

module HaskBoy.Cpu.Execution
    ( cycleCpu
    , execute
    , toInstruction
    , handleInterrupts
    ) where

import Control.Lens
import Control.Monad (when)
import Control.Monad.State.Strict

import Data.Bits ((.&.), shiftR)
import Data.Word (Word8, Word16)
import HaskBoy.BitOps

import HaskBoy.Cpu
import HaskBoy.Cpu.Instructions hiding (bit)
import HaskBoy.Cpu.Instructions qualified as Instr

import HaskBoy.Emulator
import HaskBoy.Mmu
import Numeric (showHex)

data Instruction
    = Nop
    | Xor !(Argument Word8)
    | Or !(Argument Word8)
    | Cpl
    | And !(Argument Word8)
    | Ld !(Argument Word8) !(Argument Word8)
    | Store16 !(ALens' Emulator Word16) !Word16
    | Inc !(Argument Word8)
    | Inc16 !(ALens' Registers Word16)
    | Dec !(Argument Word8)
    | Dec16 !(ALens' Registers Word16)
    | Add !(Argument Word8)
    | Add16 !(ALens' Registers Word16)
    | StackStore !Word8
    | Sub !(Argument Word8)
    | Sbc !(Argument Word8)
    | Swap !(Argument Word8)
    | Bit !Int !(Argument Word8)
    | Set !Int !(Argument Word8)
    | Cmp !(Argument Word8)
    | Jmp !Word16
    | JmpC !(ALens' Registers Bool) !Word16
    | Jr !Bool
    | Push !Word16
    | Pop !(ALens' Registers Word16)
    | PopAF
    | Call !Word16
    | Rst !Word16
    | Ret !(Maybe (ALens' Registers Bool))
    | EnableInterrupt
    | DisableInterrupt

data Argument a where
    Register :: (ALens' Cpu a) -> Argument a
    Address :: (ALens' Mmu a) -> Argument a

cycleCpu :: State Emulator Integer
cycleCpu = undefined

handleInterrupts :: State Emulator (Maybe Integer)
handleInterrupts = do
    iEnable <- use (mmu.cloneLens (addr 0xFFFF))
    iflag <- use (mmu.cloneLens (addr 0xFF0F))

    -- VBlank interrupt
    if iflag^.bit 0 && iEnable^.bit 0
    then do
        pushStack =<< use (cpu.register.pc)
        cpu.register.pc .= 0x40
        mmu.cloneLens (addr 0xFF0F).bit 0 .= False
        pure (Just 4)

    else pure Nothing

execute :: Instruction -> State Emulator ()
execute = \case
    Nop -> mcycle 1

    Ld lhs rhs -> do
        mcycle 1
        mcycle (argCost 0 1 lhs)
        mcycle (argCost 0 1 rhs)

        fromArgument lhs <~ use (fromArgument rhs)

    Store16 r v -> mcycle 3 >> cloneLens r .= v

    StackStore v -> do
        mcycle 3
        p <- use (cpu.register.sp)

        cpu.register.zero .= False
        cpu.register.subOp .= False
        cpu.register.hcarry .= (fromIntegral v .&. 0xF + (p .&. 0xF) > 0xF)
        cpu.register.carry .= (toInteger v + toInteger p > 0xFF)

        cpu.register.hl .= p + fromIntegral v

    Xor arg -> do
        mcycle (argCost 1 2 arg)
        xor =<< use (fromArgument arg)

    Or arg -> do
        mcycle (argCost 1 2 arg)
        Instr.or (fromArgument arg)

    Cpl -> mcycle 1 >> cpl

    And arg -> do
        mcycle (argCost 1 2 arg)
        Instr.and =<< use (fromArgument arg)

    Inc arg -> do
        mcycle (argCost 1 3 arg)
        inc (fromArgument arg)

    Dec arg -> do
        mcycle (argCost 1 3 arg)
        dec (fromArgument arg)

    Dec16 r -> do
        mcycle 2
        cpu.register.cloneLens r -= 1

    Add arg -> do
        mcycle (argCost 1 2 arg)
        add =<< use (fromArgument arg)

    Add16 v -> mcycle 2 >> add16 (cpu.register.v)

    Sub arg -> do
        mcycle (argCost 1 2 arg)
        sub (fromArgument arg)

    Sbc arg -> do
        mcycle (argCost 1 2 arg)
        sbc =<< use (fromArgument arg)

    Swap arg -> do
        mcycle (argCost 2 4 arg)
        swap (fromArgument arg)

    Bit n arg -> do
        mcycle (argCost 2 3 arg)
        Instr.bit n (fromArgument arg)

    Set n arg -> do
        mcycle (argCost 2 4 arg)
        fromArgument arg.bit n .= True

    Inc16 r -> do
        mcycle 2
        cpu.register.cloneLens r += 1

    Cmp arg -> do
        mcycle (argCost 1 2 arg)
        cmp (fromArgument arg)

    Jr v -> jr v
    Jmp v -> cpu.register.pc .= v

    JmpC k w -> do
        mcycle 3
        use (cpu.register.cloneLens k) >>=
            flip when (mcycle 1 >> jmp w)

    Push v -> do
        mcycle 4
        pushStack v

    Pop r -> do
        mcycle 3
        cpu.register.cloneLens r <~ popStack

    PopAF -> do
        mcycle 3
        v <- use (cpu.register.flag)
        cpu.register.af <~ popStack
        cpu.register.flag .= v

    Call v -> call v

    Rst v -> do
        mcycle 4
        pushStack v
        jmp v

    Ret mk -> mcycle 2 >> case mk of
            Just k -> do
                mcycle 3
                use (cpu.register.cloneLens k) >>= flip when ret

            Nothing -> mcycle 2 >> ret

    EnableInterrupt -> mcycle 1 >> cpu.interruptEnable .= True
    DisableInterrupt -> mcycle 1 >> cpu.interruptEnable .= False

mcycle :: Integer -> State Emulator ()
mcycle v = cpu.tclock += (v * 4)

toInstruction :: Word8 -> State Emulator Instruction
toInstruction = \case
    0x00 -> pure Nop

    0x01 -> Store16 (cpu.register.bc) <$> consumeWord

    0x02 -> do
        nn <- use (cpu.register.bc)
        pure $ Ld (Address $ addr nn) (Register $ register.a)

    0x03 -> pure (Inc16 bc)

    i | i .&. 0xC7 == 0x04 -> Inc . toArgument 3 i <$> use cpu
    i | i .&. 0xC7 == 0x05 -> Dec . toArgument 3 i <$> use cpu

    i | i .&. 0xC7 == 0x06 ->
        Ld  . toArgument 3 i <$> use cpu
           <*> fmap (Address . addr) (cpu.register.pc <<+= 1)

    0x09 -> pure $ Add16 bc
    0x0B -> pure (Dec16 bc)
    0x13 -> pure (Inc16 de)
    0x19 -> pure $ Add16 de
    0x1B -> pure (Dec16 de)
    0x2B -> pure (Dec16 hl)
    0x23 -> pure (Inc16 hl)
    0x29 -> pure $ Add16 hl
    0x33 -> pure (Inc16 sp)
    0x39 -> pure $ Add16 sp
    0x3B -> pure (Dec16 sp)

    i | i .&. 0xF8 == 0x40 -> Ld (Register $ register.b) . toArgument 0 i <$> use cpu
    i | i .&. 0xF8 == 0x48 -> Ld (Register $ register.c) . toArgument 0 i <$> use cpu
    i | i .&. 0xF8 == 0x50 -> Ld (Register $ register.d) . toArgument 0 i <$> use cpu
    i | i .&. 0xF8 == 0x58 -> Ld (Register $ register.e) . toArgument 0 i <$> use cpu
    i | i .&. 0xF8 == 0x60 -> Ld (Register $ register.h) . toArgument 0 i <$> use cpu
    i | i .&. 0xF8 == 0x68 -> Ld (Register $ register.l) . toArgument 0 i <$> use cpu

    i | i .&. 0xF8 == 0x70 -> do
        v <- use (cpu.register.hl)
        Ld (Address $ addr v) . toArgument 0 i <$> use cpu

    i | i .&. 0xF8 == 0x78 -> Ld (Register $ register.a) . toArgument 0 i <$> use cpu

    i | i .&. 0xF8 == 0x90 -> Sub . toArgument 0 i <$> use cpu
    i | i .&. 0xF8 == 0x98 -> Sbc . toArgument 0 i <$> use cpu
    i | i .&. 0xF8 == 0xA8 -> Xor . toArgument 0 i <$> use cpu

    0x12 -> do
        nn <- use (cpu.register.de)
        pure $ Ld (Address $ addr nn) (Register $ register.a)

    0x22 -> do
        nn <- cpu.register.hl <<+= 1
        pure $ Ld (Address $ addr nn) (Register $ register.a)

    0x31 -> Store16 (cpu.register.sp) <$> consumeWord

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

    i | i .&. 0xF8 == 0x80 -> Add . toArgument 0 i <$> use cpu

    0x18 -> pure (Jr True)
    0x20 -> Jr . not <$> use (cpu.register.zero)
    0x28 -> Jr <$> use (cpu.register.zero)

    0x11 -> Store16 (cpu.register.de) <$> consumeWord
    0x21 -> Store16 (cpu.register.hl) <$> consumeWord

    0x2F -> pure Cpl

    0x30 -> Jr . not <$> use (cpu.register.carry)
    0x38 -> Jr <$> use (cpu.register.carry)

    0x77 -> do
        nn <- use (cpu.register.hl)
        pure $ Ld (Address $ addr nn) (Register $ register.a)

    i | i .&. 0xF8 == 0xA0 -> And . toArgument 0 i <$> use cpu
    i | i .&. 0xF8 == 0xB0 -> Or . toArgument 0 i <$> use cpu
    i | i .&. 0xF8 == 0xB8 -> Cmp . toArgument 0 i <$> use cpu

    0xC0 -> pure $ Ret . Just $ zero.lens not (const not)
    0xC8 -> pure $ Ret (Just zero)

    0xC1 -> pure (Pop bc)

    0xC3 -> do
        cpu.tclock += 16
        Jmp <$> consumeWord

    0xC5 -> Push <$> use (cpu.register.bc)
    0xC6 -> Add . Address . addr <$> (cpu.register.pc <<+= 1)

    0xC9 -> do
        cpu.tclock += 16
        pure (Ret Nothing)

    0xCA -> JmpC (zero.lens not (const not)) <$> consumeWord

    0xCB -> consumeByte >>= \case
        i | i .&. 0xF8 == 0x30 -> Swap . toArgument 0 i <$> use cpu
        i | i .&. 0xF8 == 0x48 -> Bit 1 . toArgument 0 i <$> use cpu
        i | i .&. 0xF8 == 0x78 -> Bit 7 . toArgument 0 i <$> use cpu

        i | i .&. 0xF8 == 0xC0 -> Set 0 . toArgument 0 i <$> use cpu
        i | i .&. 0xF8 == 0xC8 -> Set 1 . toArgument 0 i <$> use cpu
        i | i .&. 0xF8 == 0xD0 -> Set 2 . toArgument 0 i <$> use cpu
        i | i .&. 0xF8 == 0xD8 -> Set 3 . toArgument 0 i <$> use cpu
        i | i .&. 0xF8 == 0xE0 -> Set 4 . toArgument 0 i <$> use cpu
        i | i .&. 0xF8 == 0xE8 -> Set 5 . toArgument 0 i <$> use cpu
        i | i .&. 0xF8 == 0xF0 -> Set 6 . toArgument 0 i <$> use cpu
        i | i .&. 0xF8 == 0xF8 -> Set 7 . toArgument 0 i <$> use cpu

        arg -> error $ "Invalid CB argument: " ++ showHex arg ""

    0xCD -> do
        cpu.tclock += 24
        Call <$> consumeWord

    0xCF -> pure $ Rst 0x08

    0xD0 -> pure $ Ret . Just $ carry.lens not (const not)
    0xD1 -> pure (Pop de)
    0xD5 -> Push <$> use (cpu.register.de)
    0xD6 -> Sub . Address . addr <$> (cpu.register.pc <<+= 1)
    0xDE -> Sbc . Address . addr <$> (cpu.register.pc <<+= 1)
    0xDF -> pure $ Rst 0x18

    0xE0 -> do
        mcycle 1
        v <- fromIntegral <$> consumeByte
        pure $ Ld (Address $ addr (0xFF00 + v)) (Register $ register.a)

    0xE1 -> pure (Pop hl)

    0xE2 -> do
        v <- fromIntegral <$> use (cpu.register.c)
        pure $ Ld (Address $ addr (0xFF00 + v)) (Register $ register.a)

    0xE5 -> Push <$> use (cpu.register.hl)
    0xE6 -> And . Address . addr <$> (cpu.register.pc <<+= 1)

    0xE9 -> do
        cpu.tclock += 4
        av <- use (cpu.register.hl)
        pure (Jmp av)

    0xEA -> do
        mcycle 2
        v <- consumeWord
        pure $ Ld (Address $ addr v) (Register $ register.a)

    0xEF -> pure $ Rst 0x28

    0xF0 -> do
        mcycle 1
        v <- fromIntegral <$> consumeByte
        pure $ Ld (Register $ register.a) (Address $ addr (0xFF00 + v))

    0xF1 -> pure PopAF
    0xF3 -> pure DisableInterrupt
    0xF5 -> Push <$> use (cpu.register.af)
    0xF6 -> Or . Address . addr <$> (cpu.register.pc <<+= 1)
    0xF8 -> StackStore <$> consumeByte

    0xFA -> do
        mcycle 2
        Ld (Register $ register.a) . Address . addr <$> consumeWord

    0xFB -> pure EnableInterrupt
    0xFE -> Cmp . Address . addr <$> (cpu.register.pc <<+= 1)
    0xFF -> pure $ Rst 0x38

    instr -> error $ "Unimplemented instruction: 0x" ++ showHex instr ""

fromArgument :: Argument a -> Lens' Emulator a
fromArgument (Register r) = cpu.cloneLens r
fromArgument (Address as) = mmu.cloneLens as

toArgument :: Int -> Word8 -> Cpu -> Argument Word8
toArgument i n s = case shiftR n i .&. 7 of
    0 -> Register (register.b)
    1 -> Register (register.c)
    2 -> Register (register.d)
    3 -> Register (register.e)
    4 -> Register (register.h)
    5 -> Register (register.l)
    6 -> Address (addr (s^.register.hl))
    7 -> Register (register.a)
    _ -> error "Invalid instructionn argument"

argCost :: Integer -> Integer -> Argument a -> Integer
argCost rc _ (Register _) = rc
argCost _ ac (Address  _) = ac
