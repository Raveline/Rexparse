module Main (main) where

import Data.Rexparse
import Test.Hspec

-- | Ascii code, foreground rgb, background rgb.
fixtureCells :: [Cell]
fixtureCells =
    [ Cell 65 255 0 0 0 0 0 -- 'A', red foreground
    , Cell 66 0 255 0 0 0 0 -- 'B', green foreground
    , Cell 67 0 0 255 0 0 0 -- 'C', blue foreground
    , Cell 68 0 0 0 0 0 0 -- 'D', black foreground
    ]

-- | One cell, four 1x1 layers.
fixture :: XpFile
fixture =
    mkXpFile
        (RexpaintVersionNumber (-1))
        [mkLayer 1 1 [c] | c <- fixtureCells]

bumpGlyph :: Cell -> Cell
bumpGlyph c = c{asciiCode = asciiCode c + 1}

main :: IO ()
main = hspec $ do
    let xp = decodeXPFile (encodeXPFile fixture)
        ls = layers xp

    describe "encodeXPFile / decodeXPFile" $
        it "round-trips the fixture" $
            xp `shouldBe` fixture

    describe "parsed structure" $ do
        it "keeps the layer count" $
            numberOfLayers xp `shouldBe` NumberOfLayers 4
        it "keeps every layer 1x1" $
            map (\l -> (imageWidth l, imageHeight l)) ls `shouldBe` replicate 4 (1, 1)

    describe "xpCells" $
        it "yields every cell, bottom layer first" $
            xpCells xp `shouldBe` zipWith (\i c -> (i, (0, 0), c)) [0 ..] fixtureCells

    describe "cellsAt" $
        it "returns the stack bottom-to-top" $
            cellsAt xp (0, 0) `shouldBe` fixtureCells

    describe "getRexpaintCell" $ do
        it "hits the requested layer" $
            getRexpaintCell ls 2 (0, 0) `shouldBe` Just (fixtureCells !! 2)
        it "rejects out-of-range coordinates" $
            getRexpaintCell ls 0 (1, 0) `shouldBe` Nothing
        it "rejects an unknown layer" $
            getRexpaintCell ls 4 (0, 0) `shouldBe` Nothing

    describe "mapXp" $
        it "rewrites every cell" $
            map (\(_, _, c) -> c) (xpCells (mapXp (\_ _ -> bumpGlyph) xp))
                `shouldBe` map bumpGlyph fixtureCells
