{-# LANGUAGE DataKinds  #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs      #-}

module HaskBoy.Cpu.Execution
    ( cycleCpu
    , execute
    , getInstruction
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
    | Xor Argument
    | Or Argument
    | Cpl
    | And Argument
    | Ld Argument Argument
    | Store16 !(ALens' Emulator Word16) !Word16
    | Inc Argument
    | Inc16 !(ALens' Registers Word16)
    | Dec Argument
    | Dec16 !(ALens' Registers Word16)
    | Add Argument
    | Add16 !(ALens' Registers Word16)
    | StackStore !Word8
    | Sub Argument
    | Sbc Argument
    | Swap Argument
    | Bit !Int Argument
    | Res !Int Argument
    | Set !Int Argument
    | Cmp Argument
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

data Argument where
    Register :: (ALens' Cpu Word8) -> Argument
    Address :: Word16 -> Argument

cycleCpu :: State Emulator ()
cycleCpu = do
    handleInterrupts
    execute =<< getInstruction

handleInterrupts :: State Emulator ()
handleInterrupts = mapM_ (uncurry handleInterrupt) [(0,0x40), (1,0x48)]

  where handleInterrupt n a = do
            iE <- use (readM 0xFFFF)
            iF <- use (readM 0xFF0F)

            when (iF^.bit n && iE^.bit n) $ do
                pushStack =<< use pc
                jmp a
                writeM 0xFF0F .bit n .= False
                mcycle 4

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
        register.cloneLens r -= 1

    Add arg -> do
        mcycle (argCost 1 2 arg)
        add =<< use (fromArgument arg)

    Add16 v -> mcycle 2 >> add16 (register.v)

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

    Res n arg -> do
        mcycle (argCost 2 4 arg)
        res n (fromArgument arg)

    Set n arg -> do
        mcycle (argCost 2 4 arg)
        fromArgument arg.bit n .= True

    Inc16 r -> do
        mcycle 2
        register.cloneLens r += 1

    Cmp arg -> do
        mcycle (argCost 1 2 arg)
        cmp (fromArgument arg)

    Jr v -> jr v
    Jmp v -> pc .= v

    JmpC k w -> do
        mcycle 3
        use (register.cloneLens k) >>=
            flip when (mcycle 1 >> jmp w)

    Push v -> do
        mcycle 4
        pushStack v

    Pop r -> do
        mcycle 3
        register.cloneLens r <~ popStack

    PopAF -> do
        mcycle 3
        v <- use (af.lowerByte)
        af <~ popStack
        af.lowerByte .= v

    Call v -> call v

    Rst v -> do
        mcycle 4
        pushStack v
        jmp v

    Ret mk -> mcycle 2 >> case mk of
            Just k -> do
                mcycle 3
                use (register.cloneLens k) >>= flip when ret

            Nothing -> mcycle 2 >> ret

    EnableInterrupt -> mcycle 1 >> interruptEnable .= True
    DisableInterrupt -> mcycle 1 >> interruptEnable .= False

mcycle :: Integer -> State Emulator ()
mcycle v = tclock += (v * 4)

getInstruction :: State Emulator Instruction
getInstruction = consumeByte >>= \case
    0x00 -> pure Nop

    0x01 -> Store16 bc <$> consumeWord

    0x02 -> do
        nn <- use bc
        pure $ Ld (Address nn) (Register $ af.upperByte)

    0x03 -> pure (Inc16 bc)

    i | i .&. 0xC7 == 0x04 -> Inc . toArgument 3 i <$> use cpu
    i | i .&. 0xC7 == 0x05 -> Dec . toArgument 3 i <$> use cpu

    i | i .&. 0xC7 == 0x06 -> do
        v <- pc <<+= 1
        Ld  . toArgument 3 i <$> use cpu ?? Address v

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

    i | i .&. 0xF8 == 0x40 -> Ld (Register $ bc.upperByte) . toArgument 0 i <$> use cpu
    i | i .&. 0xF8 == 0x48 -> Ld (Register $ bc.lowerByte) . toArgument 0 i <$> use cpu
    i | i .&. 0xF8 == 0x50 -> Ld (Register $ de.upperByte) . toArgument 0 i <$> use cpu
    i | i .&. 0xF8 == 0x58 -> Ld (Register $ de.lowerByte) . toArgument 0 i <$> use cpu
    i | i .&. 0xF8 == 0x60 -> Ld (Register $ hl.upperByte) . toArgument 0 i <$> use cpu
    i | i .&. 0xF8 == 0x68 -> Ld (Register $ hl.lowerByte) . toArgument 0 i <$> use cpu

    i | i .&. 0xF8 == 0x70 -> do
        v <- use hl
        Ld (Address v) . toArgument 0 i <$> use cpu

    i | i .&. 0xF8 == 0x78 -> Ld (Register $ af.upperByte) . toArgument 0 i <$> use cpu

    i | i .&. 0xF8 == 0x90 -> Sub . toArgument 0 i <$> use cpu
    i | i .&. 0xF8 == 0x98 -> Sbc . toArgument 0 i <$> use cpu
    i | i .&. 0xF8 == 0xA8 -> Xor . toArgument 0 i <$> use cpu

    0x12 -> do
        nn <- use de
        pure $ Ld (Address nn) (Register $ af.upperByte)

    0x22 -> do
        nn <- hl <<+= 1
        pure $ Ld (Address nn) (Register $ af.upperByte)

    0x31 -> Store16 sp <$> consumeWord

    0x32 -> do
        nn <- hl <<-= 1
        pure $ Ld (Address nn) (Register $ af.upperByte)

    0x0A -> do
        nn <- use bc
        pure $ Ld (Register $ af.upperByte) (Address nn)

    0x1A -> do
        nn <- use de
        pure $ Ld (Register $ af.upperByte) (Address nn)

    0x2A -> do
        v <- hl <<+= 1
        pure $ Ld (Register $ af.upperByte) (Address v)
    0x3A -> do
        v <- hl <<-= 1
        pure $ Ld (Register $ af.upperByte) (Address v)

    i | i .&. 0xF8 == 0x80 -> Add . toArgument 0 i <$> use cpu

    0x18 -> pure (Jr True)
    0x20 -> Jr . not <$> use zero
    0x28 -> Jr <$> use zero

    0x11 -> Store16 de <$> consumeWord
    0x21 -> Store16 hl <$> consumeWord

    0x2F -> pure Cpl

    0x30 -> Jr . not <$> use carry
    0x38 -> Jr <$> use carry

    0x77 -> do
        nn <- use hl
        pure $ Ld (Address nn) (Register $ af.upperByte)

    i | i .&. 0xF8 == 0xA0 -> And . toArgument 0 i <$> use cpu
    i | i .&. 0xF8 == 0xB0 -> Or . toArgument 0 i <$> use cpu
    i | i .&. 0xF8 == 0xB8 -> Cmp . toArgument 0 i <$> use cpu

    0xC0 -> pure $ Ret . Just $ zero.lens not (const not)
    0xC8 -> pure $ Ret (Just zero)

    0xC1 -> pure (Pop bc)

    0xC3 -> do
        tclock += 16
        Jmp <$> consumeWord

    0xC5 -> Push <$> use bc
    0xC6 -> do
        v <- pc <<+= 1
        pure $ Add (Address v)

    0xC9 -> do
        tclock += 16
        pure (Ret Nothing)

    0xCA -> JmpC (zero.lens not (const not)) <$> consumeWord

    0xCB -> consumeByte >>= \case
        i | i .&. 0xF8 == 0x30 -> Swap . toArgument 0 i <$> use cpu
        i | i .&. 0xF8 == 0x48 -> Bit 1 . toArgument 0 i <$> use cpu
        i | i .&. 0xF8 == 0x78 -> Bit 7 . toArgument 0 i <$> use cpu
        i | i .&. 0xF8 == 0x80 -> Res 0 . toArgument 0 i <$> use cpu

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
        tclock += 24
        Call <$> consumeWord

    0xCF -> pure $ Rst 0x08

    0xD0 -> pure $ Ret . Just $ carry.lens not (const not)
    0xD1 -> pure (Pop de)
    0xD5 -> Push <$> use de
    0xD6 -> do
        v <- pc <<+= 1
        pure $ Sub (Address v)
    0xD8 -> pure $ Ret $ Just carry
    0xDE -> do
        v <- pc <<+= 1
        pure $ Sbc (Address v)
    0xDF -> pure $ Rst 0x18

    0xE0 -> do
        mcycle 1
        v <- fromIntegral <$> consumeByte
        pure $ Ld (Address (0xFF00 + v)) (Register $ af.upperByte)

    0xE1 -> pure (Pop hl)

    0xE2 -> do
        v <- fromIntegral <$> use (bc.lowerByte)
        pure $ Ld (Address (0xFF00 + v)) (Register $ af.upperByte)

    0xE5 -> Push <$> use hl
    0xE6 -> do
        v <- pc <<+= 1
        pure $ And $ Address v

    0xE9 -> do
        tclock += 4
        av <- use hl
        pure (Jmp av)

    0xEA -> do
        mcycle 2
        v <- consumeWord
        pure $ Ld (Address v) (Register $ af.upperByte)

    0xEF -> pure $ Rst 0x28

    0xF0 -> do
        mcycle 1
        v <- fromIntegral <$> consumeByte
        pure $ Ld (Register $ af.upperByte) (Address (0xFF00 + v))

    0xF1 -> pure PopAF
    0xF3 -> pure DisableInterrupt
    0xF5 -> Push <$> use af
    0xF6 -> do
        v <- pc <<+= 1
        pure $ Or (Address v)
    0xF8 -> StackStore <$> consumeByte

    0xFA -> do
        mcycle 2
        Ld (Register $ af.upperByte) . Address <$> consumeWord

    0xFB -> pure EnableInterrupt
    0xFE -> do
        v <- pc <<+= 1
        pure $ Cmp $ Address v
    0xFF -> pure $ Rst 0x38

    instr -> error $ "Unimplemented instruction: 0x" ++ showHex instr ""

fromArgument :: Argument -> Lens' Emulator Word8
fromArgument (Register r) = cpu.cloneLens r
fromArgument (Address  a) = mmu.cloneLens (addr a)

toArgument :: Int -> Word8 -> Cpu -> Argument
toArgument i n s = case shiftR n i .&. 7 of
    0 -> Register (bc.upperByte)
    1 -> Register (bc.lowerByte)
    2 -> Register (de.upperByte)
    3 -> Register (de.lowerByte)
    4 -> Register (hl.upperByte)
    5 -> Register (hl.lowerByte)
    6 -> Address (s^.hl)
    7 -> Register (af.upperByte)
    _ -> error "Invalid instructionn argument"

argCost :: Integer -> Integer -> Argument -> Integer
argCost rc _ (Register _) = rc
argCost _ ac (Address  _) = ac
