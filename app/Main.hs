module Main (main) where

import Codec.Picture hiding (imageHeight, imageWidth)
import Codec.Picture.Types (createMutableImage, unsafeFreezeImage)
import Control.Monad.ST (runST)
import Data.Rexparse (Cell (..), Layer (..), XpFile (..), parseXPFile, traverseXp_)
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

cellScale :: Int
cellScale = 12

cellColor :: Cell -> Maybe PixelRGB8
cellColor c
    | bgRed c == 255 && bgGreen c == 0 && bgBlue c == 255 = Nothing
    | asciiCode c == 0 || asciiCode c == 32 = Just (PixelRGB8 (bgRed c) (bgGreen c) (bgBlue c))
    | otherwise = Just (PixelRGB8 (fgRed c) (fgGreen c) (fgBlue c))

renderXp :: XpFile -> Image PixelRGB8
renderXp xp = runST $ do
    let (w, h) = case layers xp of
            (l : _) -> (fromIntegral (imageWidth l), fromIntegral (imageHeight l))
            [] -> (0, 0)
    img <- createMutableImage (w * cellScale) (h * cellScale) (PixelRGB8 0 0 0)
    traverseXp_
        ( \_ (x, y) c -> case cellColor c of
            Nothing -> pure ()
            Just p ->
                sequence_
                    [ writePixel img (x * cellScale + dx) (y * cellScale + dy) p
                    | dx <- [0 .. cellScale - 1]
                    , dy <- [0 .. cellScale - 1]
                    ]
        )
        xp
    unsafeFreezeImage img

main :: IO ()
main = do
    args <- getArgs
    case args of
        [input, output] -> do
            xp <- parseXPFile input
            writePng output (renderXp xp)
            putStrLn $ "Wrote " <> output
        _ -> do
            prog <- getProgName
            hPutStrLn stderr $ "usage: " <> prog <> " <input.xp> <output.png>"
            exitFailure
