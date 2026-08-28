{- | Rexparse is a simple library to read the REXPaint @.xp@ format.

For efficiency, cells are stored in arrays. Read a file with 'parseXPFile';
from the resulting 'XpFile' you can either:

- look up individual cells by layer index and @(x, y)@ coordinates, with
  'getRexpaintCell' (or 'unsafeGetRexpaintCell' to skip the 'Maybe' when
  you know the indices are valid);
- or walk every cell: 'xpCells' for a plain list, 'traverseXp_' to run an
  action per cell, 'traverseXp' / 'mapXp' to rewrite cells.

Build a file with 'mkLayer' / 'mkXpFile' and write it back with 'writeXPFile'.
-}
module Data.Rexparse (
    RexpaintVersionNumber (..),
    NumberOfLayers (..),
    LayerIndex (..),
    ImageWidth (..),
    ImageHeight (..),
    Layer (..),
    Cell (..),
    XpFile (..),
    getRexpaintCell,
    unsafeGetRexpaintCell,
    parseXPFile,
    decodeXPFile,
    writeXPFile,
    encodeXPFile,
    mkLayer,
    mkXpFile,
    xpCells,
    layerCells,
    cellsAt,
    traverseXp,
    traverseXp_,
    mapXp,
)
where

import Codec.Compression.GZip (compress, decompress)
import Control.Monad (replicateM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Array qualified as A
import Data.Binary.Get
import Data.Binary.Put
import Data.ByteString.Lazy qualified as BSL
import Data.Foldable (traverse_)
import Data.Functor.Identity (Identity (..))
import Data.Int
import Data.Word
import GHC.List ((!?))

{- | Version number stored in the beginning of each xp file.
This is only used by Rexpaint.
-}
newtype RexpaintVersionNumber = RexpaintVersionNumber Int32
    deriving newtype (Show, Eq, Ord)

-- | Amount of layers stored in the picture.
newtype NumberOfLayers = NumberOfLayers Int32
    deriving newtype (Show, Eq, Ord)

-- | Index of a layer within an 'XpFile', @0@ being the bottom layer.
newtype LayerIndex = LayerIndex Int
    deriving newtype (Show, Eq, Ord, Num, Enum, Real, Integral)

-- | Width in glyphs (not in pixel !).
newtype ImageWidth = ImageWidth Int32
    deriving newtype (Show, Eq, Ord, Num, Enum, Real, Integral)

-- | Height in glyphs (not in pixels !).
newtype ImageHeight = ImageHeight Int32
    deriving newtype (Show, Eq, Ord, Num, Enum, Real, Integral)

{- | A single layer. Width and height are repeated on each layer,
as per xp file specifications. Cells are stored in a single array,
as in the xp file itself.
-}
data Layer = Layer
    { imageWidth :: ImageWidth
    , imageHeight :: ImageHeight
    , cells :: A.Array Int Cell
    }
    deriving stock (Show, Eq, Ord)

{- | A single cell, containing an asciicode and rgb data for
foreground (fg) and background (bg).
-}
data Cell = Cell
    { asciiCode :: Int32
    , fgRed :: Word8
    , fgGreen :: Word8
    , fgBlue :: Word8
    , bgRed :: Word8
    , bgGreen :: Word8
    , bgBlue :: Word8
    }
    deriving stock (Show, Eq, Ord)

{- | A full xp file.
Layers are ordered bottom first, top last.
-}
data XpFile = XpFile
    { versionNumber :: RexpaintVersionNumber
    , numberOfLayers :: NumberOfLayers
    , layers :: [Layer]
    }
    deriving stock (Show, Eq, Ord)

{- | Build a 'Layer' from its cells in the on-disk order: column @x = 0@
top to bottom, then @x = 1@, and so on. The list must hold exactly
@width * height@ cells.
-}
mkLayer :: ImageWidth -> ImageHeight -> [Cell] -> Layer
mkLayer w h cs = Layer w h (A.listArray (0, n - 1) cs)
  where
    n = fromIntegral w * fromIntegral h

{- | Assemble an 'XpFile' from its layers, bottom first. 'numberOfLayers'
is derived from the list.
-}
mkXpFile :: RexpaintVersionNumber -> [Layer] -> XpFile
mkXpFile ver ls = XpFile ver (NumberOfLayers (fromIntegral (length ls))) ls

-- Binary (de)serialisation utilities. We can't use generics here
-- because xp is little endian.

getCell :: Get Cell
getCell =
    Cell
        <$> getInt32le
        <*> getWord8
        <*> getWord8
        <*> getWord8
        <*> getWord8
        <*> getWord8
        <*> getWord8

getLayer :: Get Layer
getLayer = do
    w <- getInt32le
    h <- getInt32le
    let n = fromIntegral w * fromIntegral h
    cs <- replicateM n getCell
    pure $ Layer (ImageWidth w) (ImageHeight h) (A.listArray (0, n - 1) cs)

getXpFile :: Get XpFile
getXpFile = do
    ver <- getInt32le
    nl <- getInt32le
    ls <- replicateM (fromIntegral nl) getLayer
    pure $ XpFile (RexpaintVersionNumber ver) (NumberOfLayers nl) ls

putCell :: Cell -> Put
putCell c = do
    putInt32le (asciiCode c)
    mapM_ putWord8 [fgRed c, fgGreen c, fgBlue c, bgRed c, bgGreen c, bgBlue c]

putLayer :: Layer -> Put
putLayer l = do
    putInt32le (fromIntegral (imageWidth l))
    putInt32le (fromIntegral (imageHeight l))
    mapM_ putCell (A.elems (cells l))

putXpFile :: XpFile -> Put
putXpFile xp = do
    putInt32le ver
    putInt32le (fromIntegral (length (layers xp)))
    mapM_ putLayer (layers xp)
  where
    RexpaintVersionNumber ver = versionNumber xp

{- | Decode the gzip-compressed on-disk representation of a xp file.
Inverse of 'encodeXPFile'. Partial on malformed input, like 'parseXPFile'.
-}
decodeXPFile :: BSL.ByteString -> XpFile
decodeXPFile = runGet getXpFile . decompress

{- | Serialise an 'XpFile' to REXPaint's gzip-compressed on-disk format.
The layer count written is derived from the 'layers' list, not from the
'numberOfLayers' field.
-}
encodeXPFile :: XpFile -> BSL.ByteString
encodeXPFile = compress . runPut . putXpFile

{- | Fetch a given cell in a list of layers, given a
layer number and a set of coords (x, y).
-}
getRexpaintCell :: [Layer] -> LayerIndex -> (Int, Int) -> Maybe Cell
getRexpaintCell layers' (LayerIndex ln) (x, y) = do
    layer <- layers' !? ln
    let h = fromIntegral (imageHeight layer)
        invalidX = x < 0 || x >= fromIntegral (imageWidth layer)
        invalidY = y < 0 || y >= h
    if invalidX || invalidY
        then Nothing
        else pure $ cells layer A.! (x * h + y)

-- | Equivalent of `getRexpaintCell` but without safety.
unsafeGetRexpaintCell :: [Layer] -> LayerIndex -> (Int, Int) -> Cell
unsafeGetRexpaintCell layers' (LayerIndex ln) (x, y) =
    let layer = layers' !! ln
        h = fromIntegral (imageHeight layer)
     in cells layer A.! (x * h + y)

{- | Parse a xp file. This doesn't include proper error management,
so you might want to call this with some @try@.
-}
parseXPFile :: (MonadIO m) => FilePath -> m XpFile
parseXPFile fp = decodeXPFile <$> liftIO (BSL.readFile fp)

-- | Write an 'XpFile' to disk in REXPaint's format.
writeXPFile :: (MonadIO m) => FilePath -> XpFile -> m ()
writeXPFile fp = liftIO . BSL.writeFile fp . encodeXPFile

indexToCoords :: Int -> Int -> (Int, Int)
indexToCoords h i = i `divMod` h

{- | List all the content of a xp file: bottom layer first; per layer,
we then iterate in a column-major fashion, top to bottom then left to right.
-}
xpCells :: XpFile -> [(LayerIndex, (Int, Int), Cell)]
xpCells xp =
    [ (LayerIndex li, indexToCoords h i, c)
    | (li, layer) <- zip [0 ..] (layers xp)
    , let h = fromIntegral (imageHeight layer)
    , (i, c) <- A.assocs (cells layer)
    ]

-- | List all the cells in a given layer.
layerCells :: Layer -> [((Int, Int), Cell)]
layerCells layer =
    [(indexToCoords h i, c) | (i, c) <- A.assocs (cells layer)]
  where
    h = fromIntegral (imageHeight layer)

{- | Fetch the cell at a given set of coords in every layer that has one,
bottom layer first.
-}
cellsAt :: XpFile -> (Int, Int) -> [Cell]
cellsAt xp xy =
    [ c
    | ln <- [0 .. LayerIndex (length (layers xp) - 1)]
    , Just c <- [getRexpaintCell (layers xp) ln xy]
    ]

{- | Traversal of an xp file. Useful if you need to alter the content
of a file and need some applicative to do so.
-}
traverseXp ::
    (Applicative f) =>
    (LayerIndex -> (Int, Int) -> Cell -> f Cell) ->
    XpFile ->
    f XpFile
traverseXp f xp =
    (\ls -> xp{layers = ls})
        <$> traverse (uncurry (traverseLayer f)) (zip [0 ..] (layers xp))

traverseLayer ::
    (Applicative f) =>
    (LayerIndex -> (Int, Int) -> Cell -> f Cell) ->
    Int ->
    Layer ->
    f Layer
traverseLayer f li layer =
    (\cs -> layer{cells = A.array (A.bounds (cells layer)) cs})
        <$> traverse rebuild (A.assocs (cells layer))
  where
    h = fromIntegral (imageHeight layer)
    rebuild (i, c) = (,) i <$> f (LayerIndex li) (indexToCoords h i) c

-- | Traversal of an xp file. Typical use case would be to call a display function for each cell.
traverseXp_ ::
    (Applicative f) =>
    (LayerIndex -> (Int, Int) -> Cell -> f b) ->
    XpFile ->
    f ()
traverseXp_ f xp = traverse_ (\(li, xy, c) -> f li xy c) (xpCells xp)

{- | Map over an xpfile. Useful if you need to alter an xp file without
needing any kind of applicative in your process.
-}
mapXp :: (LayerIndex -> (Int, Int) -> Cell -> Cell) -> XpFile -> XpFile
mapXp f = runIdentity . traverseXp (\li xy c -> Identity (f li xy c))
