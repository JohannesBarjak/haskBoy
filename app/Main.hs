{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Lens
import Control.Monad (join, void)
import Control.Monad.State.Strict

import Data.Bits
import Data.ByteString qualified as BS

import Data.Sequence as Seq
import Data.Word (Word8)
import Foreign (castPtr, pokeElemOff)

import HaskBoy.Emulator
import HaskBoy.Emulator.Execution
import HaskBoy.Mmu
import HaskBoy.Ppu

import SDL (($=))
import SDL qualified
import SDL.Raw qualified

import System.Environment (getArgs)

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
            { SDL.windowInitialSize = SDL.V2 320 288
            , SDL.windowResizable = True
            }

    window <- SDL.createWindow "GameBoy" windowConfig
    renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer

    SDL.rendererLogicalSize renderer $= Just (SDL.V2 160 144)

    filename <- head <$> getArgs
    (Just rom) <- toMemory <$> loadRom filename

    emulatorLoop renderer (initialEmulator rom) 0

    SDL.destroyRenderer renderer
    SDL.destroyWindow window

loadRom :: FilePath -> IO [Word8]
loadRom f = BS.unpack <$> BS.readFile f

emulatorLoop :: SDL.Renderer -> Emulator -> Integer -> IO ()
emulatorLoop renderer prevState cycles = do
    start <- SDL.time
    void . mapM handleEvent =<< SDL.pollEvents

    let (dp, nextState) = runState (cycleCpu cycles >> rawDisplay) prevState
    renderGbDisplay dp renderer
    end <- SDL.time

    let newCycles = round (fromIntegral hzps * (end - start) :: Double)
    emulatorLoop renderer nextState newCycles

handleEvent :: SDL.Event -> IO ()
handleEvent event = case SDL.eventPayload event of
    SDL.WindowClosedEvent _ -> error "Closed window :D"
    _ -> pure ()

renderGbDisplay :: Seq Word8 -> SDL.Renderer -> IO ()
renderGbDisplay dp renderer = do
    text <- gbTexture renderer

    pixels <- castPtr . fst <$> SDL.lockTexture text Nothing
    mapM_ (pokeElemOff pixels <*> (Seq.index dp . (`div` 3))) $ init [0..160 * 144 * 3]

    SDL.unlockTexture text

    SDL.clear renderer
    SDL.copy renderer text Nothing Nothing
    SDL.present renderer

    SDL.destroyTexture text

rawDisplay :: State Emulator (Seq Word8)
rawDisplay = mapM pixelToColor . join =<< use (ppu.display)

pixelToColor :: Pixel -> State Emulator Word8
pixelToColor p = do
    palette <- use (cloneLens $ mmu.addr 0xFF47)
    pure $ [255, 170, 85, 0] !! color palette

    where color palette = fromIntegral (palette `shiftR` (fromEnum p * 2)) .&. 3

gbTexture :: SDL.Renderer -> IO SDL.Texture
gbTexture renderer = SDL.createTexture renderer SDL.RGB24 SDL.TextureAccessStreaming (SDL.V2 160 144)
