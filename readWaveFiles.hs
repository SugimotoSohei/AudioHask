module Main where

import WaveHask
import System.Environment(getArgs)

main = do
  a <- getArgs
  f <- readWaveFiles a

  fileWrite "map_write.wav" $ newWaveData f