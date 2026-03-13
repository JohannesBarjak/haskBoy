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
import Control.Monad.Reader

import Data.Bits ((.&.), shiftR, Bits)
import Data.Word (Word8, Word16)
import HaskBoy.BitOps

import HaskBoy.Cpu

import HaskBoy.Cpu.Instructions hiding (bit)
import HaskBoy.Cpu.Instructions qualified as Instr

import HaskBoy.Mmu
import Numeric (showHex)

data Instruction s
  = Nop
  | Xor (Argument s)
  | Or (Argument s)
  | Cpl
  | Scf
  | Ccf
  | And (Argument s)
  | Ld (Argument s) (Argument s)
  | Ld16 (Argument16 s) (Argument16 s)
  | Store16 !(ALens' s Word16) !Word16
  | Inc (Argument s)
  | Inc16 !(ALens' Registers Word16)
  | Dec (Argument s)
  | Dec16 !(ALens' Registers Word16)
  | Add (Argument s)
  | Adc (Argument s)
  | DAA
  | Add16 !(ALens' Registers Word16)
  | StackStore !Word8
  | Sub (Argument s)
  | Sbc (Argument s)
  | Swap (Argument s)
  | RotA Bool | RocA Bool
  | Bit !Int (Argument s)
  | Res !Int (Argument s)
  | Set !Int (Argument s)
  | Cmp (Argument s)
  | Jmp !Word16
  | JmpC !(Getter Registers Bool) !Word16
  | Jr !Bool
  | Push !Word16
  | Pop !(ALens' Registers Word16)
  | PopAF
  | Call !Word16
  | CallC !(Getter Registers Bool) !Word16
  | Rst !Word16
  | Ret !(Maybe (ALens' Registers Bool))
  | RetI
  | EnableInterrupt
  | DisableInterrupt

data Argument s where
  Register :: HasRegisters s => (ALens' s Word8) -> Argument s
  Address :: Word16 -> Argument s

data Argument16 s where
  Register16 :: HasRegisters s => (ALens' s Word16) -> Argument16 s
  Address16  :: Word16 -> Argument16 s

read8 :: HasMmu s => Argument s -> Getter s Word8
read8 (Register r) = cloneLens r
read8 (Address  a) = readM a

write8 :: HasMmu s => Argument s -> Setter' s Word8
write8 (Register r) = cloneLens r
write8 (Address  a) = writeM a

read16 :: HasMmu s => Argument16 s -> Getter s Word16
read16 (Register16 r) = cloneLens r
read16 (Address16  a) = readM16 a

write16 :: HasMmu s => Argument16 s -> Setter' s Word16
write16 (Register16 r) = cloneLens r
write16 (Address16  a) = writeM16 a

cycleCpu :: (MonadState s m, HasCpu s, HasRegisters s, HasMmu s) => m ()
cycleCpu = do
  handleInterrupts
  execute =<< getInstruction

handleInterrupts :: (MonadState s m, HasMmu s, HasCpu s, HasRegisters s) => m ()
handleInterrupts = mapM_ (uncurry handleInterrupt) [(0,0x40), (1,0x48)]

  where handleInterrupt n a = do
          iE <- use (readM 0xFFFF)
          iF <- use (readM 0xFF0F)

          when (iF^.bit n && iE^.bit n) $ do
            pushStack =<< use pc
            jmp a
            writeM 0xFF0F .bit n .= False
            mcycle 4

execute :: (MonadState s m, HasRegisters s, HasCpu s, HasMmu s) => Instruction s -> m ()
execute = \case
  Nop -> mcycle 1

  Ld lhs rhs -> do
    mcycle 1
    mcycle (argCost 0 1 lhs)
    mcycle (argCost 0 1 rhs)

    write8 lhs <~ use (read8 rhs)

  Ld16 lhs rhs -> do
    mcycle 2
    mcycle (arg16Cost 1 3 lhs)

    write16 lhs <~ use (read16 rhs)

  Store16 r v -> mcycle 3 >> cloneLens r .= v

  StackStore v -> do
    mcycle 3
    stackStore v

  Xor arg -> do
    mcycle (argCost 1 2 arg)
    xor =<< use (read8 arg)

  Or arg -> do
    mcycle (argCost 1 2 arg)
    Instr.or =<< use (read8 arg)

  Cpl -> mcycle 1 >> cpl
  Scf -> mcycle 1 >> scf
  Ccf -> mcycle 1 >> ccf

  And arg -> do
    mcycle (argCost 1 2 arg)
    Instr.and =<< use (read8 arg)

  Inc arg -> do
    mcycle (argCost 1 3 arg)
    write8 arg <~ (inc =<< use (read8 arg))

  Dec arg -> do
    mcycle (argCost 1 3 arg)
    write8 arg <~ (dec =<< use (read8 arg))

  Dec16 r -> do
    mcycle 2
    register.cloneLens r -= 1

  DAA -> mcycle 1 >> daa

  Add arg -> do
    mcycle (argCost 1 2 arg)
    add =<< use (read8 arg)

  Adc arg -> do
    mcycle (argCost 1 2 arg)
    adc =<< use (read8 arg)

  Add16 v -> do
    mcycle 2
    add16 =<< use (cloneLens $ register.v)

  Sub arg -> do
    mcycle (argCost 1 2 arg)
    sub =<< use (read8 arg)

  Sbc arg -> do
    mcycle (argCost 1 2 arg)
    sbc =<< use (read8 arg)

  Swap arg -> do
    mcycle (argCost 2 4 arg)
    write8 arg <~ (swap =<< use (read8 arg))

  Bit n arg -> do
    mcycle (argCost 2 3 arg)
    Instr.bit n =<< use (read8 arg)

  RocA b -> mcycle 1 >> rocA b
  RotA b -> mcycle 1 >> rotA b

  Res n arg -> do
    mcycle (argCost 2 4 arg)
    (write8 arg .=) <$> res n =<< use (read8 arg)

  Set n arg -> do
    mcycle (argCost 2 4 arg)
    write8 arg.bit n .= True

  Inc16 r -> do
    mcycle 2
    register.cloneLens r += 1

  Cmp arg -> do
    mcycle (argCost 1 2 arg)
    cmp =<< use (read8 arg)

  Jr v -> jr v
  Jmp v -> pc .= v

  JmpC k w -> do
    mcycle 3
    use (register.k) >>= flip when (mcycle 1 >> jmp w)

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

  CallC c a -> use (register.c) >>= (`callC` a)
  Call v -> call v

  Rst v -> do
    mcycle 4
    pushStack =<< use pc
    jmp v

  Ret mk -> mcycle 2 >> case mk of
    Just k -> do
      mcycle 3
      use (register.cloneLens k) >>= flip when ret

    Nothing -> mcycle 2 >> ret

  RetI -> do
    jmp =<< popStack
    interruptEnable .= True
    mcycle 1

  EnableInterrupt -> mcycle 1 >> interruptEnable .= True
  DisableInterrupt -> mcycle 1 >> interruptEnable .= False

liftRd :: (MonadState s m) => Reader s a -> m a
liftRd = gets . runReader

getInstruction :: (MonadState s m, HasRegisters s, HasMmu s, HasCpu s) => m (Instruction s)
getInstruction = consumeByte >>= \case
  0x00 -> pure Nop

  0x01 -> Store16 bc <$> consumeWord

  0x02 -> do
    nn <- use bc
    pure $ Ld (Address nn) (Register $ af.upperByte)

  0x03 -> pure (Inc16 bc)

  i | instrMid i == 0x04 -> Inc <$> liftRd (toArgument 3 i)
  i | instrMid i == 0x05 -> Dec <$> liftRd (toArgument 3 i)

  i | instrMid i == 0x06 -> do
    v <- pc <<+= 1
    Ld <$> liftRd (toArgument 3 i) ?? Address v

  0x08 -> (`Ld16` Register16 sp) . Address16 <$> consumeWord

  -- At the moment stop will act as a nop instruction.
  0x10 -> pure Nop

  0x09 -> pure $ Add16 bc
  0x0B -> pure $ Dec16 bc
  0x13 -> pure $ Inc16 de
  0x19 -> pure $ Add16 de
  0x1B -> pure $ Dec16 de
  0x2B -> pure $ Dec16 hl
  0x23 -> pure $ Inc16 hl
  0x29 -> pure $ Add16 hl
  0x33 -> pure $ Inc16 sp
  0x39 -> pure $ Add16 sp
  0x3B -> pure $ Dec16 sp

  i | instrEnd i == 0x40 -> Ld (Register $ bc.upperByte) <$> liftRd (toArgument 0 i)
  i | instrEnd i == 0x48 -> Ld (Register $ bc.lowerByte) <$> liftRd (toArgument 0 i)
  i | instrEnd i == 0x50 -> Ld (Register $ de.upperByte) <$> liftRd (toArgument 0 i)
  i | instrEnd i == 0x58 -> Ld (Register $ de.lowerByte) <$> liftRd (toArgument 0 i)
  i | instrEnd i == 0x60 -> Ld (Register $ hl.upperByte) <$> liftRd (toArgument 0 i)
  i | instrEnd i == 0x68 -> Ld (Register $ hl.lowerByte) <$> liftRd (toArgument 0 i)

  i | instrEnd i == 0x70 -> do
     v <- use hl
     Ld (Address v) <$> liftRd (toArgument 0 i)

  i | instrEnd i == 0x78 -> Ld (Register $ af.upperByte) <$> liftRd (toArgument 0 i)

  i | instrEnd i == 0x90 -> Sub <$> liftRd (toArgument 0 i)
  i | instrEnd i == 0x98 -> Sbc <$> liftRd (toArgument 0 i)
  i | instrEnd i == 0xA8 -> Xor <$> liftRd (toArgument 0 i)

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

  0x27 -> pure DAA

  i | instrEnd i == 0x80 -> Add <$> liftRd (toArgument 0 i)
  i | instrEnd i == 0x88 -> Adc <$> liftRd (toArgument 0 i)

  0x18 -> pure (Jr True)
  0x20 -> Jr . not <$> use zero
  0x28 -> Jr <$> use zero

  0x11 -> Store16 de <$> consumeWord
  0x21 -> Store16 hl <$> consumeWord

  0x2F -> pure Cpl
  0x37 -> pure Scf
  0x3F -> pure Ccf

  0x30 -> Jr . not <$> use carry
  0x38 -> Jr <$> use carry

  0x77 -> do
    nn <- use hl
    pure $ Ld (Address nn) (Register $ af.upperByte)

  i | instrEnd i == 0xA0 -> And <$> liftRd (toArgument 0 i)
  i | instrEnd i == 0xB0 -> Or  <$> liftRd (toArgument 0 i)
  i | instrEnd i == 0xB8 -> Cmp <$> liftRd (toArgument 0 i)

  0xC0 -> pure $ Ret . Just $ zero.lens not (const not)
  0xC8 -> pure $ Ret (Just zero)

  0xC1 -> pure (Pop bc)
  0xD1 -> pure (Pop de)

  0xC2 -> JmpC (zero.to not) <$> consumeWord
  0xD2 -> JmpC (carry.to not) <$> consumeWord
  0xC3 -> mcycle 4 >> Jmp <$> consumeWord

  0xC5 -> Push <$> use bc

  0xC6 -> Add . Address <$> (pc <<+= 1)
  0xCE -> Adc . Address <$> (pc <<+= 1)

  0xC9 -> do
    tclock += 16
    pure (Ret Nothing)

  0xCA -> JmpC zero  <$> consumeWord
  0xDA -> JmpC carry <$> consumeWord

  0xCB -> consumeByte >>= \case
    i | instrEnd i == 0x30 -> Swap <$> liftRd (toArgument 0 i)

    i | instrPrefix i == 0x40 -> do
          arg <- liftRd (toArgument 0 i)
          pure $ Bit (fromIntegral $ shiftR i 3 .&. 7) arg

    i | instrPrefix i == 0x80 -> do
          arg <- liftRd (toArgument 0 i)
          pure $ Res (fromIntegral $ shiftR i 3 .&. 7) arg

    i | instrPrefix i == 0xC0 -> do
          arg <- liftRd (toArgument 0 i)
          pure $ Set (fromIntegral $ shiftR i 3 .&. 7) arg

    arg -> error $ "Invalid CB argument: " ++ showHex arg ""

  0x07 -> pure $ RocA False
  0x0F -> pure $ RocA True
  0x17 -> pure $ RotA False
  0x1F -> pure $ RotA True

  0xC4 -> CallC (zero.to not) <$> consumeWord
  0xCC -> CallC zero  <$> consumeWord
  0xDC -> CallC carry <$> consumeWord
  0xCD -> mcycle 6 >> Call <$> consumeWord
  0xD4 -> CallC (carry.to not) <$> consumeWord

  0xC7 -> pure $ Rst 0x00
  0xCF -> pure $ Rst 0x08
  0xD7 -> pure $ Rst 0x10
  0xEF -> pure $ Rst 0x28
  0xDF -> pure $ Rst 0x18
  0xFF -> pure $ Rst 0x38

  0xD0 -> pure $ Ret . Just $ carry.lens not (const not)

  0xD5 -> Push <$> use de
  0xD6 -> do
    v <- pc <<+= 1
    pure $ Sub (Address v)

  0xD8 -> pure $ Ret $ Just carry
  0xD9 -> pure RetI

  0xDE -> do
    v <- pc <<+= 1
    pure $ Sbc (Address v)

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

  instr -> error $ "Unimplemented instruction: 0x" ++ showHex instr ""

           -- Utility functions to get instruction information from bytes.
instrEnd, instrMid, instrPrefix :: (Bits a, Num a) => a -> a
instrEnd = (.&. 0xF8)
instrMid = (.&. 0xC7)
instrPrefix = instrMid . instrEnd

toArgument :: (MonadReader s m, HasRegisters s) => Int -> Word8 -> m (Argument s)
toArgument i n = case shiftR n i .&. 7 of
  0 -> pure $ Register (bc.upperByte)
  1 -> pure $ Register (bc.lowerByte)
  2 -> pure $ Register (de.upperByte)
  3 -> pure $ Register (de.lowerByte)
  4 -> pure $ Register (hl.upperByte)
  5 -> pure $ Register (hl.lowerByte)
  6 -> Address <$> view hl
  7 -> pure $ Register (af.upperByte)
  _ -> error "Invalid instructionn argument"

argCost :: Integer -> Integer -> Argument s -> Integer
argCost rc _ (Register _) = rc
argCost _ ac (Address  _) = ac

arg16Cost :: Integer -> Integer -> Argument16 s -> Integer
arg16Cost rc _ (Register16 _) = rc
arg16Cost _ ac (Address16  _) = ac
