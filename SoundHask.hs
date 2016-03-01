module SoundHask (
    --Sound,
    Sound(..),
    defaultSound,
    sounds
  )
  where

import Data.Int

data Sound = Sound {
  rate :: Int   ,
  second :: Int ,
  soundD :: [(Int,Float)]
}

defaultSound :: Sound
defaultSound = Sound {
  rate = 8000  ,
  second = 1    ,
  soundD = zip (repeat 32767) [261.625,293.664,329.627,349.228,391.995,440,493.883,523.251]
}

sounds :: Sound -> [Int16]
sounds sou = map (fromIntegral . fromEnum) . concat . map (\(x,y) -> (func3 (rate sou) (nn) (second sou) . func2 x ) $ func1 (rate sou) y ) $ (soundD sou)
  where nn = length (soundD sou)

-- 離散値?を求める
func1 :: Int -> Float -> [Float]
func1 r d = [0,(d / fromIntegral r)..1.0-(d / fromIntegral r)]

-- 波形?を求める
func2 :: Int -> [Float] -> [Float]
func2 i = map ((fromIntegral i *) . sin)

-- 繰り返しを生成する
func3 :: Int -> Int -> Int -> [Float] -> [Float]
func3 r l s = take (r `div` (l `div` s)) . cycle