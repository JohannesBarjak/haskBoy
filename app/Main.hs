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
  texture <- gbTexture renderer

  filename <- head <$> getArgs
  parsedRom <- toMemory <$> loadRom filename

  case parsedRom of
    Just mem -> emulatorLoop (initialEmulator mem) 0 renderer texture
    Nothing  -> putStrLn "Couldn't parse file as a gameboy cartridge"

  SDL.destroyTexture texture
  SDL.destroyRenderer renderer
  SDL.destroyWindow window

loadRom :: FilePath -> IO [Word8]
loadRom f = BS.unpack <$> BS.readFile f

emulatorLoop :: Emulator -> Integer -> SDL.Renderer -> SDL.Texture -> IO ()
emulatorLoop prevState cycles renderer texture = do
  let (dp, nextState) = runState (cycleEmulator cycles >> rawDisplay) prevState

  start <- SDL.time
  void . mapM handleEvent =<< SDL.pollEvents
  renderGbDisplay dp renderer texture
  end <- SDL.time

  let newCycles = round (fromIntegral hzps * (end - start) :: Double)
  emulatorLoop nextState newCycles renderer texture

handleEvent :: SDL.Event -> IO ()
handleEvent event = case SDL.eventPayload event of
  SDL.WindowClosedEvent _ -> error "Closed window :D"
  _ -> pure ()

-- Render gameboy display onto an sdl texture.
renderGbDisplay :: Seq Word8 -> SDL.Renderer -> SDL.Texture -> IO ()
renderGbDisplay dp renderer texture = do
  pixels <- castPtr . fst <$> SDL.lockTexture texture Nothing
  mapM_ (pokeElemOff pixels <*> (Seq.index dp . (`div` 3))) $ init [0..160 * 144 * 3]

  SDL.unlockTexture texture

  SDL.clear renderer
  SDL.copy renderer texture Nothing Nothing
  SDL.present renderer

rawDisplay :: State Emulator (Seq Word8)
rawDisplay = do
  mem <- use mmu
  fmap (pixelToColor mem) . join <$> use (ppu.display)

-- Convert pixels to a grayscale colour.
pixelToColor :: Mmu -> Pixel -> Word8
pixelToColor mem p = [255, 170, 85, 0] !! color
  where color = fromIntegral (palette `shiftR` (fromEnum p * 2)) .&. 3
        palette = mem^.readM 0xFF47

gbTexture :: SDL.Renderer -> IO SDL.Texture
gbTexture renderer = SDL.createTexture renderer SDL.RGB24 SDL.TextureAccessStreaming (SDL.V2 160 144)
