module WaveHask where

import qualified Data.ByteString.Lazy as Bl
import qualified Data.ByteString.Builder as Bb

-- 型のインポート
import Data.Word
import Data.Int

data Wave = Wave {
  f_id :: Word16 , -- フォーマットID
  ch   :: Word16 , -- 1:モノラル,2:ステレオ
  helz :: Word32 , -- サンプリングレート[Hz]
  brok :: Word16 , -- ブロック(データのbyte数)
  extend :: [Word8] , -- 拡張部分
  dat :: [Int16]   -- 波形データ
  --etc :: [Word8]   -- その他
  } deriving (Show)

defaultWave :: Wave
defaultWave = Wave{
  f_id = 1, -- 今はPCMのみなので固定
  ch   = 1, -- 今はモノラル固定
  helz = 8000 ,
  brok = 2 ,-- 今は16bitのみなので固定
  extend = [] , -- 今は使用不可.PCMでは存在しない
  dat = [0x7fff,-0x7fff] -- 今は16bitのみ
  --etc = [] -- 他のチャンクを追加するために使用予定
}

newWavData :: Wave -> Bl.ByteString
newWavData wav = Bl.concat [riffC,fmtC,dtC]
  where
    rawD = Bl.concat (map convI16LE (dat wav))
    raw_fmt = Bl.concat [convW16LE (f_id wav),convW16LE (ch wav),convW32LE (helz wav),convW32LE (fromIntegral (ch wav) * helz wav * fromIntegral (brok wav)),convW16LE (brok wav),convW16LE (8 * brok wav)]
    fmtC = Bl.concat [convW32BE 0x666d7420,convW32LE ((fromIntegral . Bl.length) raw_fmt),raw_fmt]
    dtC = Bl.concat [convW32BE 0x64617461,convW32LE ((fromIntegral . Bl.length) rawD ),rawD]
    riffC = Bl.concat [convW32BE 0x52494646,convW32LE (4 + (fromIntegral . Bl.length) (Bl.concat [fmtC,dtC]) ),convW32BE 0x57415645]

readWavData :: Bl.ByteString -> Wave
readWavData bs = Wave{
  f_id = lEndian $ Bl.take 2 fmtc,
  ch   = lEndian $ Bl.take 2 $ Bl.drop 2  fmtc,
  helz = lEndian $ Bl.take 4 $ Bl.drop 4  fmtc,
  brok = lEndian $ Bl.take 2 $ Bl.drop 12 fmtc,
  extend = Bl.unpack $ Bl.drop 16 fmtc ,
  dat  = (map lEndian . twobyte) datc
  }
    where aa = wavSplit bs
          fmtc = searchChank (convW32BE 0x666d7420) aa
          datc = searchChank (convW32BE 0x64617461) aa

searchChank :: Bl.ByteString -> [(Bl.ByteString,Bl.ByteString)] -> Bl.ByteString
searchChank a b
  | fil /= [] = snd (head fil)
  | otherwise = Bl.empty
    where fil = filter (\x -> (a == fst x))  b

wavSplit :: Bl.ByteString -> [(Bl.ByteString,Bl.ByteString)]
wavSplit bs = Bl.splitAt 4 (fst asd) : spl (snd asd)
  where asd = Bl.splitAt 12 bs
        spl t
          | t == Bl.empty = []
          | otherwise = (fst b,fst d) : spl (snd d)
            where b = Bl.splitAt 4 t
                  c = Bl.splitAt 4 (snd b)
                  n = lEndian $ fst c
                  d = Bl.splitAt n (snd c)

twobyte :: Bl.ByteString -> [Bl.ByteString]
twobyte d
  | d == Bl.empty = []
  | otherwise = fst sp : twobyte (snd sp)
  where sp = Bl.splitAt 2 d

lEndian :: Num a => Bl.ByteString -> a
lEndian = (le 0) . Bl.unpack
  where le :: Num a => Int -> [Word8] -> a
        le _ [] = 0
        le n (x:xs) = (fromIntegral x) * (256 ^ n) + le (n+1) xs

-- ラッパ関数
fileRead = Bl.readFile
fileWrite = Bl.writeFile
droptake d t = (Bl.take t) . (Bl.drop d)
convI16LE = Bb.toLazyByteString . Bb.int16LE
convW16LE = Bb.toLazyByteString . Bb.word16LE
convW32BE = Bb.toLazyByteString . Bb.word32BE
convW32LE = Bb.toLazyByteString . Bb.word32LE
