{-# LANGUAGE BangPatterns#-}


-------------------------------------------------------------------------------
-- Terrain
-------------------------------------------------------------------------------

module Terrain where

import IO
import System.IO
import Directory
import System.IO
import Codec.Compression.GZip

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT

import Data.Word
import Data.List
import Data.Either
import Data.Array
import qualified Data.Array.Storable as DAS
--import Codec.Image.PNG
import Data.IORef
import qualified Data.ByteString.Lazy as LB

import Foreign
import Coord
import Utils

import qualified Data.Map as DM

import Graphics
import Control.Concurrent

type TerrainData = (Ptr Word8)
type TerrainCache = (DM.Map Int (TerrainData, TextureObject))

data TerrainDB = TerrainDB {

    dbFileName :: String,
    cache :: IORef TerrainCache

}

rowSize = 32768::Int
tileSize=256::Int
latSpacing = (deg2rad $ 180*(fromIntegral(tileSize)/16384))
lonSpacing = latSpacing

numLatTiles = rowSize `div` (tileSize*2)  -- 64
numLonTiles = rowSize `div` (tileSize)    -- 128

-------------------------------------------------------------------------------
-- exported functions
--

terrainTextureLookup :: TerrainDB -> GraphicsTextureReference -> IO GraphicsTexture
terrainTextureLookup terrainDB (GraphicsTextureReference ref) = do
    debug $ "terrainTextureLookup "++show ref
    hFlush stdout
    tile <- readCompressedTile (dbFileName terrainDB) ref
    tbytes <- terrainToTextureBytes tile
    free tile
    debug $ "terrainTextureLookup success"
    hFlush stdout
    return (GraphicsTexture (tileSize,tileSize, tbytes))

openDB :: String -> IO TerrainDB
openDB fname = do
    tc <- newIORef DM.empty
    return (TerrainDB fname tc)

terrainGraphics :: BoundCoords -> XFormFunction -> [ GraphicsCmd ]
terrainGraphics (Coord lat1 lon1, Coord lat2 lon2) xform = 
    -- TODO: Find out why this color setting is needed for OpenGL
    --, color $ Color3 (1.0::GLfloat) 1.0 1.0
    map (drawTile xform) grid

    where
        gridLatTopLeft = truncAwayFromZero (lat1/latSpacing)
        gridLonTopLeft = truncAwayFromZero (lon1/lonSpacing)
        gridLatBottomRight = truncAwayFromZero (lat2/latSpacing)
        gridLonBottomRight = truncAwayFromZero (lon2/lonSpacing)

        grid = [((truncate lat,truncate lon),Coord (latSpacing*lat) (lonSpacing*lon), Coord (latSpacing*(lat+1)) (lonSpacing*(lon+1)))
                  | lat <- [gridLatBottomRight..gridLatTopLeft], lon <- [gridLonTopLeft..gridLonBottomRight] ]


-- calculate blended palette value in the range (from..to)
--
blendPalette :: Palette -> Palette -> Int -> [Palette]
blendPalette from@(Palette fr fg fb) to@(Palette tr tg tb) steps = 
    [ Palette (blend fr tr steps x)     
        (blend fg tg steps x)
        (blend fb tb steps x)
     | x <- [0..steps-1]]

    where
        blend :: Word8 -> Word8 -> Int -> Int -> Word8
        blend from to count x = truncate $ from' + ((to' - from') / count')*x'
            where from' = fromIntegral from
                  to' = fromIntegral to
                  count' = fromIntegral count-1
                  x' = fromIntegral x


-- convert from altitude (in feet) to index value, as used in terrain DB altIdx :: Int -> Int
altIdx a = (a+1500) `div` 125

-- convert a range of tiles (lat/lon) from the master source database into
-- compressed tiles in the target database
convertFromBigTerrain source target latrng lonrng = do
    mapM_ (\(lat,lon) -> 
        readFromMasterTerrain source lat lon >>= \tile ->
        writeCompressedTile tile target lat lon >>
        free tile
      ) [(lat,lon) | lat <- latrng, lon <- lonrng]

-------------------------------------------------------------------------------
-- private functions & data
--

drawTile :: XFormFunction -> ((Int,Int),Coord, Coord) -> GraphicsCmd
drawTile xform ((lat,lon),Coord c1lat c1lon, Coord c2lat c2lon) =
    Graphics.Polygon (GraphicsColour3 1 1 1) 1 Nothing $ TexturedPolygon texref (zip pts texpts)
    --Graphics.Polygon (GraphicsColour3 1 1 0) 1 Nothing $ UntexturedPolygon pts
    --Graphics.Polyline (GraphicsColour3 0 0 0) 1 Nothing $ pts

    where
        pts = map (pointToPlane (-100)) [ xform $ Coord c1lat c1lon
          , xform $ Coord c1lat c2lon
          , xform $ Coord c2lat c2lon
          , xform $ Coord c2lat c1lon ]
        texref = GraphicsTextureReference $ terrainKey lat lon
        texpts = [ (0,1), (1,1), (1,0), (0,0) ]

    --renderPrimitive Quads $ mapM_ 


data Palette = Palette { palRed :: Word8, palGrn :: Word8, palBlu :: Word8 } deriving (Show)

paletteMapper :: Ptr Word8 -> Ptr Word8 -> Int -> IO ()
paletteMapper tb rgb idx = do
    v <- peekElemOff tb idx
    pokeByteOff rgb (idx*3) (palRed (palette!v))
    pokeByteOff rgb (idx*3+1) (palGrn (palette!v))
    pokeByteOff rgb (idx*3+2) (palBlu (palette!v))

{-
palette = listArray (0,255) ((replicate 12 $ Palette 255 255 255) ++ 
               [Palette 0 128 255] ++
               [ Palette (255-x) (255-x) (255-x) | x <- [0,8..255] ]++
               (replicate (255-44) $ Palette 0 0 0))
-}

palette = listArray (0,255) ((replicate 12 $ Palette 255 255 255) ++
                [Palette 102 204 255] ++
                blendPalette (Palette 255 255 255) (Palette 219 131 64) 16 ++
                blendPalette (Palette 219 131 64) (Palette 68 5 0) 64 ++
                blendPalette (Palette 68 5 0) (Palette 0 0 0) (211-48))

-------------------------------------------------------------------------------
-- convert 8bit texture reference altitudes to RGB data, ready for use in
-- an OpenGL texture
--
terrainToTextureBytes :: TerrainData -> IO (Ptr Word8)
terrainToTextureBytes tb = do

    -- implementation #1
    rgbBytes <- mallocArray (3*(tileSize^2)) :: IO (Ptr Word8)
    mapM_ (\x -> do 
        v <- peekElemOff tb x
        pokeByteOff rgbBytes (x*3) (palRed (palette!v))
        pokeByteOff rgbBytes (x*3+1) (palGrn (palette!v))
        pokeByteOff rgbBytes (x*3+2) (palBlu (palette!v))
        
        ) [0..tileSize^2-1]
    
    {-
    -- implementation #3
    rlist <- peekArray (tileSize^2) terrainBytes
    rgbBytes <- newArray $ concat [ [ palRed (palette ! r), palGrn (palette!r), palBlu (palette!r) ] | r <- rlist ]
    -}
    --free terrainBytes
 
    return rgbBytes

terrainKey :: Int -> Int -> Int
terrainKey lat lon = (lat'*1000+lon') where (lat', lon') = latLonNormalise lat lon

compressedTileFileName :: String -> Int -> Int -> String
compressedTileFileName fname lat lon = (fname ++ show (terrainKey lat lon))

-------------------------------------------------------------------------------
-- Convert the 8bit texture reference data into an OpenGL texture, load the 
-- texture into OpenGL. Return a reference to the texture
--

makeGLTexture :: TerrainData -> IO TextureObject
makeGLTexture td = do
    rawTxt <- terrainToTextureBytes td
    [t] <- genObjectNames 1
    textureBinding Texture2D $= Just t
    let tileSize' = fromIntegral tileSize
    texImage2D Nothing NoProxy 0 RGB8 (TextureSize2D tileSize' tileSize') 0 
        (PixelData RGB UnsignedByte rawTxt)
    free rawTxt
    return t

    -- 8 bit colour
    --texImage2D Nothing NoProxy 0 R3G3B2 (TextureSize2D tileSize tileSize) 0 
        --(PixelData RGB UnsignedByte332 txt)
    -- free txt

readCompressedTile :: String -> Int -> IO TerrainData
readCompressedTile fname texref = do
    fe <- doesFileExist tileFileName
    if fe then do
        bytes <- ((LB.unpack).decompress) `fmap` LB.readFile tileFileName
        newArray bytes
      else do
        -- file is missing, assume a purely water tile
        newArray (replicate (tileSize^2) (12::Word8))

    where tileFileName = fname ++ (show texref)

writeCompressedTile :: TerrainData -> String -> Int -> Int -> IO ()
writeCompressedTile tdata fnameBase lat lon = do
    bstr <- peekArray (tileSize^2) tdata :: IO [Word8]
    if nub bstr /= [12] then
        LB.writeFile (compressedTileFileName fnameBase lat lon) (compress $ LB.pack bstr)
      else
        -- the tile is exclusively water, don't write out anything
        return ()
    return ()

-- convert from lat (31..-32) and lon (-64..63), to lat (0..63) lon (0..127)
latLonNormalise :: Int -> Int -> (Int,Int)
latLonNormalise lat lon = ( ((numLatTiles `div` 2-1)-lat), (numLonTiles `div` 2)+lon)

--
-- reads a tile of data from a full (world) terrain database
--
readFromMasterTerrain :: String -> Int -> Int -> IO TerrainData
readFromMasterTerrain fname lat lon = do
    terrainBytes <- mallocArray (tileSize^2) :: IO (TerrainData)

    -- forkIO is more light-weight, but forkOS doesn't expose msvcrt.dll bugs on Windows
    -- where dead threads are left lying around
    --forkOS (readTerrainLines fname terrainBytes)

    readTerrainLines fname terrainBytes
    return terrainBytes

    where readTerrainLines fname terrainBytes = do
            h <- openBinaryFile fname ReadMode
            mapM_ (\y -> do 
                let fileRow = (lat'*tileSize+y)*rowSize
                    fileCol = lon'*tileSize
                    (lat',lon') = latLonNormalise lat lon
                hSeek h AbsoluteSeek (fromIntegral (fileRow+fileCol))
                hGetBuf h (plusPtr terrainBytes (y*tileSize)) tileSize
                ) [0..tileSize-1]
            hClose h
            --debug $ "Thread complete for " ++ show terrainBytes

analyseTile :: String -> Int -> Int -> ([Word8] -> Word8) -> IO Word8
analyseTile fname lat lon !func=
    readFromMasterTerrain fname lat lon >>= \p ->
    (peekArray (tileSize^2) p :: IO [Word8]) >>= \b ->
    --let b=replicate 256 12 in
    free p >>
    return (func b)

    --mapM (\(lat,lon) -> length `fmap` analyseTile "r:/world-terrain" lat lon) [(lat,lon) | lat <- [-31..31], lon <- [-63..63]]
    --mapM (\(lat,lon) -> length `fmap` analyseTile "r:/world-terrain" lat lon) [(lat,lon) | lat <- [0], lon <- [-63..63]]

thspan lst = (last n - head n)
 where n = (sort.nub)lst
