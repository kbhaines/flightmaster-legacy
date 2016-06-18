--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- An attempt to manage textures using classes. It lead to writing lots of seemingly
-- pointless boilerplate, which suggests I didn't have the correct abstractions or
-- types.

{-
Texture management implementation will use a Data.Map to map from 
'GraphicsTextureReference a' to texture bytes. It will invoke 
loadTexture if the texture is not in the mapping.

Actually- I'm not sure if the caching of raw texture bytes is necessary. It may
be enough to simply map 'a' -> 'GLTextureName' as the data will have been
passed to OpenGL already. There seems little need to keep the loaded bytes
around if OpenGL has them. i.e. we will let OpenGL manage the caching issues for
us.

May need to add type parameter to GraphicsCmd type

data (GraphicsTextureReference a) => GraphicsCmd a
then Polygon has 'cmdTexture :: Maybe Texture a' field

graphicsProcess :: GraphicsTextureReference a => blah...
-}
-- instances of GraphicsTextureReference must provide a texture-loading
-- function that returns the dimensions and raw texture bytes in RGB format

class (Show a, Ord a) => GraphicsTextureReference a where
    -- loadTexture returns x and y dimensions and texture bytes
    loadTexture :: a -> IO (Int, Int, TextureBytes)
    loadTexture = undefined

type TextureBytes = (Ptr Word8)

-- a texture manager is needed to locate textures by their reference,
-- during the rendering process

newTextureManager :: (GraphicsTextureReference a) => IORef (TextureManager a)
newTextureManager = undefined

newtype GraphicsTextureReference a => TextureManager a = 
    TextureManager (DM.Map a TextureBytes) deriving (Eq, Ord,Show)

-- disposes of the texture when finished 
freeTexture :: GraphicsTextureReference a => a -> IO ()
freeTexture = undefined

-- Sample implementation for Filenames (Strings). Using newtype to work
-- around the limitations of instance declarations not being allowed
-- to use type aliases (String or [Char] don't work)

newtype TextureFilename = TextureFilename String deriving (Ord, Show, Eq)
instance GraphicsTextureReference TextureFilename where

newtype TexturePNG = TexturePNG String deriving (Ord, Show, Eq)
instance GraphicsTextureReference TexturePNG where

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


{-
 
 PNG texture

setTexture :: GLsizei -> GLsizei -> Ptr Word8 -> IO Word8
setTexture x y raw = do
    texImage2D Nothing NoProxy 0 RGB8 (TextureSize2D x y) 0 (PixelData RGB UnsignedByte raw)
    return 0

drawPng :: String -> XFormFunction -> (Coord, Coord) -> IO ()
drawPng fname xform (Coord c1lat c1lon, Coord c2lat c2lon) = do

    img1 <- loadPNGFile fname
    let img = case img1 of
                Right d -> d
                otherwise -> undefined

    let (x,y) = dimensions img
    DAS.withStorableArray (imageData img) (setTexture (fromIntegral x) (fromIntegral y))

    --yy <- newArray $ concat (replicate 4000 [128::Word8,64,0,0,0,0])
    --texImage2D Nothing NoProxy 0 RGB8 (TextureSize2D 64 64) 0 (PixelData RGB UnsignedByte yy)

    -- This texture code is cribbed from:
    -- http://www.haskell.org/~pairwise/HOpenGL/HOpenGL.html#texLoad
    textureWrapMode Texture2D S $= (Repeated, Repeat) 
    textureWrapMode Texture2D T $= (Repeated, Repeat) 
    textureFilter Texture2D $= ((Nearest, Nothing), Nearest)
    -- end

    texture Texture2D $= Enabled

    renderPrimitive Quads $ mapM_ 
       (\((x, y), tx, ty) -> do
            texCoord $ TexCoord2 tx ty
            vertex $ Vertex3 x y (-10.0)
        )
        --[ ((0::GLfloat,0),0::GLfloat,1),
          --((0::GLfloat,100),1,1),
          --((100::GLfloat,100),1,0),
          --((100::GLfloat,0),0,0)]

        [ (xform $ Coord c1lat c1lon,(0.0::GLfloat),0.0),
          (xform $ Coord c1lat c2lon,(1.0::GLfloat),0.0),
          (xform $ Coord c2lat c2lon,1.0,1.0),
          (xform $ Coord c2lat c1lon,0.0,1.0)]

    texture Texture2D $= Disabled
-}
