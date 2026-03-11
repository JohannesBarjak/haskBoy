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
    }

  where joinBytes = (flip . (flip .)) (liftA2 (((.|.) . (`shiftL` 8)) `on` fromIntegral)) test

spec = describe "SM83 instruction tests." do
  forM_ (genNames "0" [0..6] <> genNames "4" [0..7]) \ns -> do
    it ("tests the cpu instruction: " <> ns) do
      content <- BL.readFile $ "test/sm83/v1/" ++ ns ++ ".json"
      let Just ts = decode content :: Maybe [SM83Test]

      forM_ ts \t -> do
        let (finalState, _) = runState
                                (runTest $ initial t)
                                (initialEmulator (fromJust . (`toMemory` RawAccess) $ replicate 0x8000 0))

        finalState `shouldBe` final t
  where genNames = map . ((. show) . (<>))

