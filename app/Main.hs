{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import SDL qualified
import SDL.Raw qualified
import SDL (($=))

import Data.Word (Word8)
import System.Environment (getArgs)
import Data.ByteString qualified as BS

import HaskBoy.Emulator

import HaskBoy.Mmu

import HaskBoy.Cpu
import HaskBoy.Cpu.Instructions
import HaskBoy.Cpu.Execution

import HaskBoy.Ppu
import HaskBoy.Ppu.Execution

import Control.Monad.State.Strict
import Control.Lens

import Data.Vector qualified as V
import Data.Vector (Vector)
import Foreign (castPtr, Storable (pokeElemOff), Ptr)
import Data.Bits

import Control.Monad (when, forM_)

import Debug.Trace (traceM)
import Numeric (showHex)

import Data.List (intercalate)

hzps, fps, hzpf :: Integer
hzps = 4194304
fps  = 60
hzpf = 69905

frameTime :: Double
frameTime = 1000 / fromIntegral fps

main :: IO ()
main = do
    SDL.initializeAll

    let windowConfig = SDL.defaultWindow 
            { SDL.windowInitialSize = SDL.V2 256 256
            , SDL.windowResizable = True
            }

    window <- SDL.createWindow "GameBoy" windowConfig
    renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer

    SDL.rendererLogicalSize renderer $= Just (SDL.V2 256 256)

    filename <- head <$> getArgs
    (Just rom) <- toMemory <$> loadRom filename

    emulatorLoop renderer (initialEmulator rom)

    SDL.destroyRenderer renderer
    SDL.destroyWindow window

loadRom :: FilePath -> IO [Word8]
loadRom f = do
    rom <- BS.readFile f
    pure $ BS.unpack rom

cycleCpu :: Integer -> State Emulator ()
cycleCpu cycles
    = when (cycles > 0) $ do
        instr <- consumeByte
        execute =<< toInstruction instr

        printCpuDbgInfo instr

        instrCost <- use (cpu.tclock)
        runlyTiming (cycles - instrCost)
        cpu.tclock .= 0

        cycleCpu (cycles - instrCost)

runlyTiming :: Integer -> State Emulator ()
runlyTiming cycles = do
    mmu.ly .= fromIntegral (((hzpf - cycles) `quot` 456) `rem` 154)

printCpuDbgInfo :: Word8 -> State Emulator ()
printCpuDbgInfo instr = do
    address <- ("$"  ++) . flip showHex "" <$> use (cpu.register.pc)
    afreg <- ("af: " ++) . flip showHex "" <$> use (cpu.register.af)
    bcreg <- ("bc: " ++) . flip showHex "" <$> use (cpu.register.bc)
    dereg <- ("de: " ++) . flip showHex "" <$> use (cpu.register.de)
    hlreg <- ("hl: " ++) . flip showHex "" <$> use (cpu.register.hl)
    spreg <- ("sp: " ++) . flip showHex "" <$> use (cpu.register.sp)
    lyreg <- ("ly: " ++) . flip showHex "" <$> use (mmu.ly)
    let instruction = "instr: 0x" ++ showHex instr ""

    traceM $
        intercalate " | " [address, afreg, bcreg, dereg, hlreg, spreg, lyreg, instruction]

emulatorLoop :: SDL.Renderer -> Emulator -> IO ()
emulatorLoop renderer emulator = do
    start <- SDL.Raw.getTicks

    let (rawdp, emulator') = runState (cycleCpu hzpf *> drawTiles *> rawDisplay) emulator

    text <- gbTexture renderer
    (pixelPtr, _) <- SDL.lockTexture text Nothing

    let (pixels :: Ptr Word8) = castPtr pixelPtr

    forM_ [0..(256 * 256) - 1] $ \i -> do
        forM_ [0..2] $ \j -> do
            pokeElemOff pixels ((i * 3) + j) (rawdp V.! i)

    SDL.unlockTexture text

    SDL.copy renderer text Nothing Nothing
    SDL.present renderer

    end <- SDL.Raw.getTicks

    let time = fromIntegral $ end - start

    when (time < frameTime) $ do
        SDL.delay $ round (frameTime - time)

    emulatorLoop renderer emulator'

rawDisplay :: State Emulator (Vector Word8)
rawDisplay = mapM colorIndexToPixel =<< use (ppu.display)

colorIndexToPixel :: Pixel -> State Emulator Word8
colorIndexToPixel ci = do
    palette <- use (mmu.raw 0xFF47)
    let color = fromIntegral (palette `shiftR` (fromEnum ci * 2)) .&. 3
    pure $ ciToPixel (toEnum color)
    where ciToPixel White     = 255
          ciToPixel LightGray = 170
          ciToPixel DarkGray  = 85
          ciToPixel Black     = 0

gbTexture :: SDL.Renderer -> IO SDL.Texture
gbTexture renderer = SDL.createTexture
    renderer SDL.RGB24 SDL.TextureAccessStreaming (SDL.V2 256 256)
