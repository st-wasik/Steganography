module Main where

import Data.Char

import Data.Bits
import Graphics.Image.IO
import Graphics.Image

encodePicture :: IO ()
encodePicture = do
    image <- readImageRGB VU "land.jpg"
    text <- readFile "t2.txt"
    --writeImage "out.png" . toDoubleI $ greyScale image
    --return ()
    let word8Image = toWord8I image
    let lists = toLists word8Image
    let pixelsLength = length $ head lists
    let msg = fmap fromIntegral $ createMessageBitList text
    let pixels = Prelude.concat lists
    let encoded = encodeS pixels msg
    let encodedLists = takeE pixelsLength encoded
    let image = Graphics.Image.fromLists encodedLists :: Image VU RGB Word8
    writeImage "out.png" $ toDoubleI image
    return ()


decodePicture :: IO ()
decodePicture = do
    image <- readImageRGB VU "out.png"
    putStrLn . show . decodeS . Prelude.concat . toLists $ toWord8I image
    return ()

greyScale :: Image VU RGB Double -> Image VU RGB Word8
greyScale image = fromLists . fmap (fmap (\px -> pixel px)) . toLists $ toWord8I image  

pixel :: Pixel RGB Word8 -> Pixel RGB Word8
pixel (PixelRGB a b c) = p
    where aa = fromIntegral a :: Double
          bb = fromIntegral b :: Double
          cc = fromIntegral c :: Double
          xx = round ( (1.0*aa) + (0.2*bb) + (1.0*cc) )
          x =  quot ( xx ) 3 :: Word8
          p = let y = if 80 > xx then 255 else 0 in PixelRGB y y y

main :: IO ()
main = encodePicture

createMessageBitList = Prelude.map (flip (-) 48 . ord) . Prelude.concat . Prelude.map (asBin . ord) 

(r,g,b) = (7,4,6)
               
encodeS :: [Pixel RGB Word8] -> [Word8] -> [Pixel RGB Word8]
encodeS [] _ = []
encodeS image [] = image
encodeS (x:xs) list = (modifyPixel x $ take (r+g+b) list) : (encodeS xs newlist)
    where newlist = drop (r+g+b) list

takeE _ [] = []
takeE e list = take e list : (takeE e $ drop e list)

modifyValue val 0 _ = val
modifyValue val n list = modifyValue' val 0 n list

modifyValue' val _ _ [] = val
modifyValue' val start end list@(x:xs)
    | start >= end = val
    | otherwise = let val' = setVal start val x in modifyValue' val' (start+1) end xs

setVal bit int value
    | value == 0 = clearBit int bit
    | otherwise = setBit int bit
   
asBin n = let x = asBin' n in reverse . take 8 $ (reverse x) ++ repeat '0'
asBin' :: Int -> String
asBin' 0 = "0"
asBin' 1 = "1"
asBin' n =  (asBin $ n `quot` 2) Prelude.++ [chr ((+) 48 $ n `mod` 2)]

modifyPixel :: Pixel RGB Word8 -> [Word8] -> Pixel RGB Word8
modifyPixel (PixelRGB x y z) list = PixelRGB x1 y1 z1
    where x1 = modifyValue x r $ take r list
          y1 = modifyValue y g $ take g $ drop r list
          z1 = modifyValue z b $ take b $ drop (r+g) list

toChar :: [Word8] -> Char
toChar = chr . fromIntegral . fromBinList
charToBitList = fmap (flip (-) 48 . ord ) . asBin . ord
intToBitList = fmap (flip (-) 48 . ord ) . asBin . fromIntegral

fromBinList :: [Word8] -> Word8
fromBinList = foldl1 (\acc a -> if a == 0 then acc*2 else 1 + acc*2)

takeFromPixel :: Pixel RGB Word8 -> [Word8]
takeFromPixel (PixelRGB x y z) = fmap fromIntegral (xList ++ yList ++ zList)
    where xList = take r . reverse $ intToBitList x
          yList = take g . reverse $ intToBitList y
          zList = take b . reverse $ intToBitList z

decodeS :: [Pixel RGB Word8] -> String
decodeS list = fmap (chr . fromIntegral . fromBinList) . takeE 8 . Prelude.concat $ fmap (takeFromPixel) list

replaceWith :: Char -> Char -> String -> String
replaceWith _ _ [] = []
replaceWith fst snd (x:xs)
    | x == fst = snd : replaceWith fst snd xs
    | otherwise = x : replaceWith fst snd xs
    