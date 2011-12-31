module DeepZoom
( sliceImage ) where

import Graphics.Transform.Magick.Images
import qualified Graphics.Transform.Magick.Types as Magick
import Foreign.Storable
import Foreign.ForeignPtr (withForeignPtr)
import Data.Int
import Data.Word
import Data.List
import System.FilePath


data Tile = Tile { col :: Int, row :: Int, rect :: Magick.Rectangle } deriving (Show)

-- | Borrowed from snap-photogallery
data Dimension = Dimension { width :: Int, height :: Int } deriving (Show)

-- | Returns the maximum size between the width and the edeg
getMax :: Dimension -> Int
getMax (Dimension x y) = max x y

maxLevel :: Integral b => Dimension -> b
maxLevel dimension = ceiling (logBase 2 (fromIntegral (getMax dimension)))

getDimension :: Magick.HImage -> IO Dimension
getDimension himg =
  withForeignPtr (Magick.getImage himg) $ \p -> do
    img <- peek p
    return $ Dimension (fromIntegral (Magick.columns img)) (fromIntegral (Magick.rows img))

tileOffset :: Integral a => a -> a -> a -> a
tileOffset tileSize overlap position
  | position == 0 = 0
  | otherwise     = (position * tileSize) - overlap

tileDimension :: Integral a => a -> a -> a -> a
tileDimension tileSize overlap position
  | position ==  0 = tileSize + overlap
  | otherwise      = tileSize + 2 * overlap

makeRectangle :: Integral a => a -> a -> a -> a -> Magick.Rectangle
makeRectangle width height x y = 
  Magick.Rectangle (fromIntegral width) (fromIntegral height)
                   (fromIntegral x) (fromIntegral y)

-- calcTiles :: Int -> Int -> Int -> Dimension -> [Tile]
calcTiles tileSize overlap (Dimension width height) = 
  [(Tile x y (makeRectangle (dimensionFor x) (dimensionFor y) 
                            (offsetFor x) (offsetFor y))) | x <- [0..cols], y <- [0..rows]]
    where offsetFor = tileOffset tileSize overlap
          dimensionFor = fromIntegral . (tileDimension tileSize overlap)
          rows = fromIntegral $ div height tileSize
          cols = fromIntegral $ div width tileSize

tileFileName :: Tile -> String
tileFileName tile = (intercalate "_" $ map show [(col tile), (row tile)]) ++ ".jpg"

tilePathName :: String -> Int -> Tile -> String
tilePathName baseDir level tile = joinPath [baseDir, show level, tileFileName tile]

-- doSliceImage :: tileSize overlap image baseDir level
--doSliceImage _ _ _ _ 1 = 

sliceReduce :: Int -> Int -> String -> IO Magick.HImage -> t -> IO Magick.HImage
sliceReduce tileSize overlap baseDir ioImage level = do
  image <- ioImage
  imageDimensions <- getDimension image
  let tiles = calcTiles tileSize overlap imageDimensions
  let level = maxLevel imageDimensions
  (putStrLn . show) $ map (tilePathName baseDir level) tiles
  return (scaleImage 50 50 image)


--sliceImage :: FilePath -> IO ()
sliceImage imagePath = do
  initializeMagick
  image <- readImage imagePath
  imageDimensions <- getDimension image
  let levels = maxLevel imageDimensions

  let baseDir = joinPath [takeDirectory imagePath, takeBaseName imagePath ++ "_files"]
  (putStrLn . show . fromIntegral)  levels
  --let tiles = calcTiles 256 4 imageDimensions
  --(putStrLn . show) $ map (tilePathName baseDir 0) tiles
  sliceReduce 254 4 baseDir image levels
  (putStrLn . show . fromIntegral)  levels
