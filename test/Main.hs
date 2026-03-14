module Main where

import Control.Lens
import Control.Monad.State.Strict

import HaskBoy.Cpu qualified as C
import HaskBoy.Cpu.Execution

import HaskBoy.Emulator
import HaskBoy.Mmu

import HaskBoy.Ppu as Ppu
import HaskBoy.Ppu.Execution as Ppu.Exec

import Test.Hspec
import Test.QuickCheck
import Data.Bits (complement, (.|.), shiftL)

import Data.Aeson (FromJSON, decode)
import GHC.Generics
import Data.ByteString.Lazy qualified as BL

import Data.Word (Word8, Word16)
import Data.Function (on)
import Control.Monad (forM, forM_)
import Data.Maybe (fromJust)
import Foreign.Marshal.Utils (toBool, fromBool)
import Numeric (showHex)

data SM83Test = SM83Test
 { name    :: String
 , initial :: CpuTestState
 , final   :: CpuTestState
 } deriving (Show, Generic)

instance FromJSON SM83Test

data CpuTestState = CpuTestState
  { a :: Word8, f :: Word8, b :: Word8, c :: Word8
  , d :: Word8, e :: Word8, h :: Word8, l :: Word8
  , pc :: Word16, sp :: Word16
  , ram :: [[Word16]]
  , ime :: Int
  } deriving (Show, Generic, Eq)

instance FromJSON CpuTestState

main :: IO ()
main = hspec spec

runTest :: (MonadState s m, HasMmu s, C.HasCpu s, C.HasRegisters s) => CpuTestState -> m CpuTestState
runTest test = do
  C.af .= joinBytes a f
  C.bc .= joinBytes b c
  C.de .= joinBytes d e
  C.hl .= joinBytes h l

  C.pc .= pc test
  C.sp .= sp test
  C.ime .= toBool (ime test)

  forM_ (ram test) \(a:v:_) -> writeM a .= fromIntegral v
  cycleCpu

  ram' <- forM (ram test) \(a:_) ->
    (a:) . pure . fromIntegral <$> use (readM a)

  cpu <- use C.cpu

  pure $ CpuTestState
    { a = cpu^.(C.af.C.upperByte), f = cpu^.(C.af.C.lowerByte)
    , b = cpu^.(C.bc.C.upperByte), c = cpu^.(C.bc.C.lowerByte)
    , d = cpu^.(C.de.C.upperByte), e = cpu^.(C.de.C.lowerByte)
    , h = cpu^.(C.hl.C.upperByte), l = cpu^.(C.hl.C.lowerByte)
    , pc = cpu^.C.pc, sp = cpu^.C.sp

    , ram = ram'
    , ime = fromBool $ cpu^.C.ime
    }

  where joinBytes = (flip . (flip .)) (liftA2 (((.|.) . (`shiftL` 8)) `on` fromIntegral)) test

spec = describe "SM83 instruction tests." do
  let initialNames = map (("0" <>) . (`showHex` "")) [0..0xF] -- Hack for filenames < 16, where '01' would be '1'.

  let invInstr = [ "cb", "d3", "db", "dd", "e3", "e4"
                 , "eb", "ec", "ed", "f4", "fc", "fd"
                 ]

  let cbInstr = map ("cb " <>) $ genNames 0x10 0x37

  let instrNames = filter (not . flip elem invInstr)
        $ initialNames <> genNames 0x10 0xFF <> cbInstr

  forM_ instrNames \ns -> do
    it ("tests the cpu instruction: " <> ns) do
      ts <- fromJust . decode <$> BL.readFile ("test/sm83/v1/" ++ ns ++ ".json")
      forM_ (ts :: [SM83Test]) testInstruction

  where genNames :: Word16 -> Word16 -> [String]
        genNames f b = map (`showHex` "") [f..b]

testInstruction t = do
  let emptyMemory = fromJust $ toMemory (replicate 0x8000 0) RawAccess
  let finalState = evalState (runTest $ initial t) (initialEmulator emptyMemory)

  finalState `shouldBe` final t

