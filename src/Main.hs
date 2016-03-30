module Main where

import Codec.Picture (writePng, generateImage)
import Codec.Picture.Types (PixelRGB8(..))
import Data.Bits ((.|.))
import Data.Fixed (mod')
import Data.List (splitAt, length)
import Data.Vector ((!), Vector, fromList)
import Numeric (readHex)
import Options.Applicative
import System.Environment (getArgs)


data Options = Options
  { hash :: String
  , size :: Int
  , output :: String
  }


background :: PixelRGB8
background =
  PixelRGB8 (fromIntegral 240) (fromIntegral 240) (fromIntegral 240)


main :: IO ()
main =
  execParser opts >>= generator
  where
    opts = info (helper <*> options)
      ( fullDesc
        <> progDesc "Generate identicons"
        <> header "hidenticon - an identicon generator" )


options :: Parser Options
options = Options
  <$> strOption
        ( long "hash"
          <> metavar "HASH"
          <> help "Hash to generate identicon" )
  <*> option auto
    ( long "size"
      <> short 's'
      <> help "Size of the identicon" )
  <*> strOption
        ( long "output"
          <> metavar "OUTPUT"
          <> short 'o'
          <> help "Name of the output file" )


generator :: Options -> IO ()
generator (Options hash size output) =
  let
    decodedHash =
      fromList $ map (\c -> hexToInt [c]) hash

    hueValue =
      let
        x = hexToInt $ snd $ splitAt (length hash - 7) hash
        max = hexToInt "fffffff"
      in
        fromIntegral x / fromIntegral max

    baseMargin =
      floor $ fromIntegral size * 0.08

    cell =
      floor $ fromIntegral (size - baseMargin * 2) / 5


    margin =
      floor $ fromIntegral (size - cell * 5) / 2
  in
    imageCreator decodedHash hueValue cell margin size output


imageCreator :: Vector Int -> Float -> Int -> Int -> Int -> String -> IO ()
imageCreator hash hue cell margin size path =
  writePng path $
    generateImage
      (pixelRenderer hash hue cell margin)
      size size


pixelRenderer :: Vector Int -> Float -> Int -> Int -> Int -> Int -> PixelRGB8
pixelRenderer hash hue cell margin x y
  | x < margin = background
  | x > margin + 5 * cell = background
  | y < margin = background
  | y > margin + 5 * cell = background
  | otherwise = (quadrantColor hash hue) $ quadrant cell margin (x, y)


hsl2rgb :: Float -> PixelRGB8
hsl2rgb h =
  let
    s = 0.5
    b = 0.7
    h' :: Float
    h' =  h * 6
    b2 = if (b < 0.5) then b else 1.0 - b
    s' = s * b2
    b' = b + s'
    s'' = s' * 2
    b'' = b' - s''
    vec =
      Data.Vector.fromList
        [ b'
        , b' - h' `mod'` 1 * s' * 2
        , b''
        , b''
        , b'' + h' `mod'` 1 * s''
        , b'' + s''
        ]
    h'' = toInteger $ floor h'
    cr = fromIntegral $ h'' `mod` 6
    cg = fromIntegral $ (h'' .|. 16) `mod` 6
    cb = fromIntegral $ (h'' .|. 8) `mod` 6
  in
    PixelRGB8
      (fromIntegral $ floor ((vec ! cr) * 255))
      (fromIntegral $ floor ((vec ! cg) * 255))
      (fromIntegral $ floor ((vec ! cb) * 255))


hexToInt :: String -> Int
hexToInt ss =
  case readHex ss of
    [] -> 0
    [(x, _)] -> x


quadrant :: Int -> Int -> (Int, Int) -> (Int, Int)
quadrant cell margin (x, y) =
  let
    f x' = floor $ fromIntegral (x' - x' `rem` cell) / (fromIntegral cell)
    normalize x' =
      case x' of
        3 -> 1
        4 -> 0
        _ -> x'
  in
    ( normalize (f (x - margin - 1))
    , (f (y - margin - 1))
    )


quadrantColor :: Vector Int -> Float -> (Int, Int) -> PixelRGB8
quadrantColor hash hue (x, y) =
  let
    place x y =
      (10 - 5 * x) + y
    color =
      case (hash ! (place x y)) `mod` 2 of
        0 -> hsl2rgb hue
        _ -> background
  in
    color
