{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LAnGUAGE BangPatterns        #-}

module Main where

import SDL qualified
import SDL.Raw qualified
import SDL (($=))

import Data.Word (Word8)
import System.Environment (getArgs)
import Data.ByteString qualified as BS

import Emulator

import Mmu

import Cpu
import Cpu.Instructions
import Cpu.Execution

import Ppu
import Ppu.Execution

import Debug.Trace

import Numeric (showHex)
import Control.Monad.State.Strict
import Control.Lens

import Data.Vector qualified as V
import Data.Vector (Vector)
import Data.Foldable (for_)

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
            { SDL.windowInitialSize = SDL.V2 160 144
            , SDL.windowResizable = True
            }

    window <- SDL.createWindow "GameBoy" windowConfig
    renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer

    SDL.rendererLogicalSize renderer $= Just (SDL.V2 160 144)

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
        pc' <- use (cpu.register.pc)
        instr <- consumeByte
        b' <- use (cpu.register.b)
        traceM $ "$" ++ showHex pc' "" ++ " b=" ++ show b' ++ "  instr: 0x" ++ showHex instr ""
        execute instr
        drawTiles
--        grabSprite 0x104 >>= liftIO . print
        instrCost <- use (cpu.tclock)
        cpu.tclock .= 0
        cycleCpu (cycles - instrCost)

emulatorLoop :: SDL.Renderer -> Emulator -> IO ()
emulatorLoop renderer emulator = do
    events <- SDL.pollEvents
    start <- SDL.Raw.getTicks

    let eventIsQPress event =
            case SDL.eventPayload event of
                SDL.KeyboardEvent keyboardEvent ->
                    SDL.keyboardEventKeyMotion keyboardEvent == SDL.Pressed &&
                    SDL.keysymKeycode (SDL.keyboardEventKeysym keyboardEvent) == SDL.KeycodeQ
                _ -> False
        qPressed = any eventIsQPress events

    let (_, !emulator') = runState (cycleCpu hzpf) emulator

    print $! emulator^.cpu.register.pc

    end <- SDL.Raw.getTicks

    let time = fromIntegral $ end - start

    when (time < frameTime) $ do
        SDL.delay $ round (frameTime - time)

    SDL.rendererDrawColor renderer $= SDL.V4 26 26 26 26
    SDL.clear renderer
    tex <- SDL.createTextureFromSurface renderer =<< gbSurface
    SDL.copy renderer tex Nothing Nothing

    SDL.rendererDrawColor renderer $= SDL.V4 255 255 255 255
    SDL.fillRect renderer (Just $ SDL.Rectangle (SDL.P $ SDL.V2 0 0) (SDL.V2 160 144))

    SDL.present renderer

    unless qPressed (emulatorLoop renderer emulator')

renderDisplay :: SDL.Texture -> Vector (Vector Color) -> IO ()
renderDisplay text dis = for_ (V.indexed dis) $ \(j, row) -> do
    for_ (V.indexed row) $ \(i, v) -> do
        -- SDL.drawPoint surf (SDL.P $ SDL.V2 j i)
        pure ()

gbTexture :: SDL.Renderer -> IO SDL.Texture
gbTexture renderer = SDL.createTexture
    renderer SDL.RGB888 SDL.TextureAccessStreaming (SDL.V2 256 256)

gbSurface :: IO SDL.Surface
gbSurface = SDL.createRGBSurface (SDL.V2 256 256) SDL.RGBA8888
