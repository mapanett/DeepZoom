module Graphics.Transform.DeepZoom.Slice
( sliceImage ) where

import Graphics.Transform.Magick.Images
import qualified Graphics.Transform.Magick.Types as Magick
import Foreign.Storable
import Foreign.ForeignPtr (withForeignPtr)
import Data.Int
import Data.Word
import Data.List
import Data.Foldable (foldlM)
import System.FilePath
import System.Directory

data Tile = Tile { col :: Int, row :: Int, rect :: Magick.Rectangle } deriving (Show)

-- | Borrowed from snap-photogallery
data Dimension = Dimension { width :: Int, height :: Int } deriving (Show)

makeDimension :: Integral a => a -> a -> Dimension
makeDimension width height = Dimension (fromIntegral width) (fromIntegral height)

-- | Returns the maximum size between the width and the edeg
getMax :: Dimension -> Int
getMax (Dimension x y) = max x y

reduce :: Dimension -> Dimension
reduce (Dimension width height) = 
  makeDimension (halfOrOne width) (halfOrOne height)
  where halfOrOne x = max 1 (ceiling ((fromIntegral x) / 2))

maxLevel :: Integral b => Dimension -> b
maxLevel dimension = ceiling (logBase 2 (fromIntegral (getMax dimension)))

getDimension :: Magick.HImage -> IO Dimension
getDimension himg =
  withForeignPtr (Magick.getImage himg) $ \p -> do
    img <- peek p
    return $ Dimension (fromIntegral (Magick.columns img)) (fromIntegral (Magick.rows img))

scaleImageToDimension :: Dimension -> Magick.HImage -> Magick.HImage
scaleImageToDimension(Dimension width height) image = 
  scaleImage (fromIntegral width) (fromIntegral height) image

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

calcTiles :: Int -> Int -> Dimension -> [Tile]
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

sliceTile :: String -> Int -> Magick.HImage -> Tile -> IO ()
sliceTile baseDir level image tile = 
  writeImage (tilePathName baseDir level tile) (cropImage (rect tile) image)

sliceReduce :: Int -> Int -> String -> Magick.HImage -> Int -> IO Magick.HImage
sliceReduce tileSize overlap baseDir image level = do
  imageDimensions <- getDimension image
  mapM_ (sliceTile baseDir level image) (calcTiles tileSize overlap imageDimensions)
  return $ scaleImageToDimension (reduce imageDimensions) image

appendPath :: FilePath -> FilePath -> FilePath
appendPath path newPart = joinPath (path : newPart : [])

deepZoomPath :: FilePath -> FilePath
deepZoomPath imagePath = joinPath [takeDirectory imagePath, takeBaseName imagePath ++ "_files"]

deepZoomXML :: Int -> Int -> Dimension -> String
deepZoomXML tileSize overlap (Dimension width height) = 
  "<?xml version='1.0' encoding='UTF-8'?>" ++
  "<Image TileSize='" ++ (show tileSize) ++ "' Overlap='" ++ (show overlap)  ++ "' " ++ 
  "Format='jpg' xmlns='http://schemas.microsoft.com/deepzoom/2008'>" ++ 
  "<Size Width='" ++ (show width) ++ "' Height='" ++ (show height) ++ "'/></Image>"

writeDeepZoomXML :: Int -> Int -> Dimension -> FilePath -> IO ()
writeDeepZoomXML tileSize overlap dimensions imagePath = 
  writeFile (joinPath [takeDirectory imagePath, takeBaseName imagePath ++ ".xml"]) 
    (deepZoomXML tileSize overlap dimensions)

sliceImage :: FilePath -> IO ()
sliceImage imagePath = do
  initializeMagick
  image <- readImage imagePath
  imageDimensions <- getDimension image
  let levels = maxLevel imageDimensions
  let baseDir = deepZoomPath imagePath

  mapM_ ((createDirectoryIfMissing True) . (appendPath baseDir) . show) [0..levels]
  foldlM (sliceReduce 256 4 baseDir) image [levels, (levels-1)..0]
  writeDeepZoomXML 256 4 imageDimensions imagePath

