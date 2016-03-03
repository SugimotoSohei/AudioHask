module Main where

import WaveHask
import SoundHask

main = do
  let sound = defaultSound{rate = 44100,soundD = zip (repeat 32767) [261.625,293.664,329.627,0]}
  fileWrite "new_write.wav" $ newWaveData defaultWave{dat = (concat . take 2 . repeat) (sounds sound),helz = fromIntegral $ rate sound}
