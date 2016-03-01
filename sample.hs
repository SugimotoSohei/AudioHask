module Main where

import WaveHask
import SoundHask
import System.Environment(getArgs)


main = do
  --a <- getArgs
  --f <- fileRead (head a)

  let sound = defaultSound{rate = 44000}
  fileWrite "new_write.wav" $ newWavData defaultWave{dat = sounds sound,helz = fromIntegral $ rate sound}

  print "OK"