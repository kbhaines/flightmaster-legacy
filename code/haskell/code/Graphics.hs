-------------------------------------------------------------------------------
-- Graphics
-------------------------------------------------------------------------------

module Graphics where

import IO
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT hiding (Polygon)
import Data.Maybe
import Foreign
import qualified Data.Map as DM
import Data.IORef
import Data.List
import Utils

-- primitive graphics types
type GraphicsFloat = GLfloat
type GraphicsInt   = GLint
type GraphicsUInt  = GLuint
type Graphics1d    = GLdouble
type GraphicsAngle = GLdouble
type Graphics2d    = (Graphics1d, Graphics1d)
type Graphics3d    = (Graphics1d, Graphics1d, Graphics1d)
type GraphicsStipple = (Maybe (GLint, GLushort))

pointToPlane :: Graphics1d -> Graphics2d -> Graphics3d
pointToPlane z (x,y) = (x,y,z)

data GraphicsColour = 
    GraphicsColour3 GraphicsFloat GraphicsFloat GraphicsFloat
  | GraphicsColour4 GraphicsFloat GraphicsFloat GraphicsFloat GraphicsFloat 
  deriving (Show, Read)

newtype GraphicsTextureReference = GraphicsTextureReference Int deriving (Ord, Eq, Show, Read)
newtype GraphicsTexture = GraphicsTexture (Int,Int, Ptr Word8)
type GraphicsTextureLoader = (GraphicsTextureReference -> IO GraphicsTexture)

newtype GraphicsPosition = GraphicsPosition (GraphicsInt, GraphicsInt) deriving (Eq,Read, Show)
newtype GraphicsSize     = GraphicsSize (GraphicsInt, GraphicsInt) deriving (Eq, Read, Show)

newtype GraphicsName = GraphicsName GraphicsUInt deriving (Eq, Read, Show)

newtype GraphicsHit = GraphicsHit [GraphicsName] deriving (Eq, Read, Show)

-- |Instances of 'GraphicsNameable' are able to provide a unique naming scheme
-- for themselves, to help with graphics picking
class GraphicsNameable a where

    -- |Minimum needed to be an instance of this class:-  to be able to convert
    -- instance to a GraphicsName
    toGraphicsName :: a -> GraphicsName

    -- |Not all elements can be reverse-lookup from graphics name to source
    fromGraphicsName :: GraphicsName -> a
    fromGraphicsName = undefined

    namedGraphics :: a -> [GraphicsCmd] -> GraphicsCmd
    namedGraphics n cmds = NamedCmd (toGraphicsName n) cmds

-- |Names for Integer-based identification
instance GraphicsNameable Int where
    toGraphicsName x = GraphicsName (fromIntegral x)

-- |Instances of 'GraphicsAble' are able to draw/render themselves
class GraphicsAble a where
    toGraphics :: a -> [GraphicsCmd]

-- |Make a 'GraphicsName' from an integral value
mkGraphicsName :: (Integral a) => a -> GraphicsName
mkGraphicsName gn = GraphicsName (fromIntegral gn)

-- |Convert from a 'GraphicsName' to an integral
unGraphicsName :: (Integral a) => GraphicsName -> a
unGraphicsName (GraphicsName gn) = fromIntegral gn

-- |Make a 'GraphicsName' from an Enum value
mkEnumGraphicsName :: (Enum a) => a -> GraphicsName 
mkEnumGraphicsName e = mkGraphicsName $ fromEnum e

-- |Get an Enum from a 'GraphicsName'
unEnumGraphicsName :: (Enum a) => GraphicsName -> a
unEnumGraphicsName e = toEnum $ unGraphicsName e

-- |Make an enumeration from a GraphicsHit
unEnumGraphicsHit :: (Enum a) => GraphicsHit -> a
unEnumGraphicsHit (GraphicsHit (e:_)) = toEnum $ unGraphicsName e

-- |Make an enumeration from a GraphicsHit, return enum and rest of GraphicsNames
hitToEnum :: (Enum a) => GraphicsHit -> (a, [GraphicsName])
hitToEnum (GraphicsHit (gn:gns)) = (toEnum $ unGraphicsName gn, gns)

namedCmd :: (GraphicsNameable a) => a -> [GraphicsCmd] -> GraphicsCmd
namedCmd n cmds = NamedCmd (toGraphicsName n) cmds

data GraphicsCmd = 
    NullOp
  -- TODO: Extract primitives, allow fine control to user
  | InitDrawing Graphics3d -- ProjectionParams
  | InitPicking Graphics3d GraphicsPosition 
  | ProjectionParams {
      cmdPP :: Graphics3d
    }
  | Polygon {
        cmdPGColour :: !GraphicsColour
      , cmdPGWidth  :: !GraphicsFloat
      , cmdPGStipple:: !(Maybe (GLint, GLushort))
      , cmdPGPoints :: !GraphicsPolygonDefinition 
    }
  | Polyline {
        cmdPLColour :: !GraphicsColour
      , cmdPLWidth  :: !GraphicsFloat
      , cmdPLStipple:: !GraphicsStipple
      , cmdPLPoints :: ![Graphics3d]
    }
  | NamedCmd {
      cmdName     :: !GraphicsName
    , cmdNamedCmds:: ![GraphicsCmd]
    }
  | Circle !Graphics3d !Graphics1d !GraphicsColour -- (x y z) radius
  | Square !Graphics3d !Graphics1d !GraphicsColour -- (x y z) width
  | RasterText !Int !Graphics3d !GraphicsColour !String -- font (xyz) colour text
  | Translate !Graphics3d ![GraphicsCmd] -- (xyz)
  | RelTranslate !Graphics3d ![GraphicsCmd] -- (xyz)
  | TransRotate Bool !Graphics3d !GraphicsAngle ![GraphicsCmd] -- resetIdentity (xyz) rotation 
  -- TODO: RelTransRotate
  | Rotate !GraphicsAngle ![GraphicsCmd]
  | Scale !Graphics1d !Graphics1d !Graphics1d ![GraphicsCmd]  -- (sx sy sz model)
  | EscapeIO !(IO [GraphicsCmd])
  | FinishedDrawing
  deriving (Show, Read)

instance Show (IO a) where
    show _ = "An IO thing"

instance Read (IO a) where
    readsPrec _ = undefined

data GraphicsPolygonDefinition =
    UntexturedPolygon [Graphics3d]
  | TexturedPolygon GraphicsTextureReference [(Graphics3d,Graphics2d)]
  deriving (Show, Read)

-- exported functions
--

data GraphicsTextureManager = GraphicsTextureManager {

    textureLoader :: GraphicsTextureLoader
  , textureMap :: IORef (DM.Map GraphicsTextureReference TextureObject)

}

newTextureManager :: GraphicsTextureLoader -> IO GraphicsTextureManager
newTextureManager luf = 
    newIORef DM.empty >>= \m ->
    return (GraphicsTextureManager luf m)

mkgc :: GraphicsColour -> IO ()
mkgc (GraphicsColour3 r g b) = color $ Color3 r g b
mkgc (GraphicsColour4 r g b a) = color $ Color4 r g b a

-- |Run the supplied list of 'GraphicsCmd' through OpenGL's hit processor and
-- return a list of hit records

graphicsHits :: GraphicsTextureManager -> [GraphicsCmd] -> IO [GraphicsHit]
graphicsHits texmgr cmds = do

    -- NB Projection matrix already set up by InitPicking command in cmds
 
    (_,hits) <- getHitRecords 512 (graphicsProcess texmgr cmds)
    debug $ show hits
    return $ case hits of
               Nothing -> []
               _ -> filter (\(GraphicsHit lst) -> (not.null) lst) $ (map hitRecord2GraphicsHit (sort $ fromJust hits))

    where
        hitRecord2GraphicsHit :: HitRecord -> GraphicsHit
        hitRecord2GraphicsHit (HitRecord _ _ as) = GraphicsHit $ map (\(Name x) -> GraphicsName x) as

graphicsProcess :: GraphicsTextureManager -> [GraphicsCmd] -> IO ()
graphicsProcess texmgr cmds = do
 --debug1 "graphicsProcess.." cmds
 mapM_ conv cmds

 where
  conv (Graphics.Polygon col wid stip (UntexturedPolygon pts)) = 
        mkgc col >> lineWidth $= wid >> lineStipple $= stip >>
        renderPrimitive Graphics.Rendering.OpenGL.Polygon (mapM_ (\(x,y,z)->vertex $ Vertex3 x y z) pts) >>
        lineStipple $= Nothing

  -- TODO: Add polyline if wid>0
  -- TODO: Add stippling support
  conv (Graphics.Polygon col _ _ (TexturedPolygon ref txpts)) = 
        mkgc col >> 
        lookupTexture texmgr ref >>= \tobj ->
        textureBinding Texture2D $= (Just tobj) >>

        -- This texture code is cribbed from:
        -- http://www.haskell.org/~pairwise/HOpenGL/HOpenGL.html#texLoad
        textureWrapMode Texture2D S $= (Repeated, ClampToEdge) >>
        textureWrapMode Texture2D T $= (Repeated, ClampToEdge) >>
        textureFilter Texture2D $= ((Linear', Nothing), Linear') >>

        texture Texture2D $= Enabled >>
        renderPrimitive Graphics.Rendering.OpenGL.Quads
            (mapM_ ( \((x,y,z),(tx,ty)) -> do 
                texCoord $ TexCoord2 tx ty
                vertex $ Vertex3 x y z) txpts ) >>
             -- NOTE: zip problem encountered here: zip pts txpts
             -- This causes an exception, so the zip operation was moved out, to Terrain.drawTile
             --
             -- using either of pts or txpts (or both) will cause the exception. It's fine to use
             -- constants.
            --([ ((0,0),(0::GraphicsFloat,0)), ((1,1),(1,1)), ((2,2),(2,2)), ((3,3),(3,3)) ]) )>>

        texture Texture2D $= Disabled
        --lineStipple $= Nothing

  conv gc = case gc of
    (InitDrawing (xp,yp,zp))-> do
        clearColor $= Color4 (1.0::GLclampf) 1.0 1.0 (0.0::GLclampf)
        clear [ColorBuffer, DepthBuffer]
        matrixMode $= Projection
        loadIdentity
        ortho (-xp) xp (-yp) yp (-zp) (zp)
        depthFunc $= Just Less
        matrixMode $= (Modelview 0)
        loadIdentity
        blend $= Enabled
        blendFunc $= (SrcAlpha, OneMinusSrcAlpha)

    (InitPicking (xp,yp,zp) (GraphicsPosition (x,y)) ) -> do
        matrixMode $= Projection
        loadIdentity
        clear [DepthBuffer]
        depthFunc $= Just Less
        vport@(Position _ vy,Size _ vh) <- get viewport
        -- KB: pickMatrix has to come before the ortho/translate functions are
        -- called, otherwise seems not to work. Not sure why!
        pickMatrix (fromIntegral x, fromIntegral (vh - y + vy)) (10,10) vport
        ortho (-xp) xp (-yp) yp (-zp) zp
        depthFunc $= Just Less
        matrixMode $= (Modelview 0)
        loadIdentity

    ProjectionParams (x,y,z) -> do
        matrixMode $= Projection
        loadIdentity
        ortho (-x) x (-y) y (-z) z
        depthFunc $= Just Less
        matrixMode $= (Modelview 0)
        loadIdentity

    FinishedDrawing -> do
        loadIdentity
        translate $ Vector3 (0::Graphics1d) 0 (8)
        swapBuffers
        
    (NamedCmd (GraphicsName name) subcmds) -> withName (Name name) $ (graphicsProcess texmgr subcmds)

    (Polyline col wid stip pts) -> 
        -- lineSmooth $= Enabled >>
        mkgc col >> lineWidth $= wid >> lineStipple $= Nothing >>
        renderPrimitive LineStrip (mapM_ (\(x,y,z)->vertex $ Vertex3 x y z) pts) >>
        lineStipple $= Nothing 
        -- -->> lineSmooth $= Disabled

    (Circle (x,y,z) r colour) -> 
            mkgc colour >>
            renderPrimitive Graphics.Rendering.OpenGL.Polygon (mapM_ (\(x', y')->vertex$Vertex3 x' y' z) (circlePoints (x,y) r))

    (Square (x,y,z) r colour) -> preservingMatrix $ do
            translate $ Vector3 x y z
            mkgc colour 
            square r

    (RasterText size (x,y,z) colour txt) -> preservingMatrix $ do
        translate $ Vector3 x y z 
        mkgc colour 
        currentRasterPosition $= (Vertex4 0 0 0 20)
        renderString (localFont size) txt

    (Translate (x,y,z) model) -> preservingMatrix $ do
        loadIdentity
        translate $ Vector3 x y z
        graphicsProcess texmgr model

    (RelTranslate (x,y,z) model) -> preservingMatrix $ do
        translate $ Vector3 x y z
        graphicsProcess texmgr model

    (TransRotate reset (x,y,z) rot model ) -> preservingMatrix $ do
        if reset then loadIdentity else return ()
        translate $ Vector3 x y z
        Graphics.UI.GLUT.rotate (rot) $ Vector3 (0::Graphics1d) 0 1
        graphicsProcess texmgr model

    (Rotate rot model) -> preservingMatrix $ do 
        loadIdentity
        Graphics.UI.GLUT.rotate (rot) $ Vector3 (0::Graphics1d) 0 1
        graphicsProcess texmgr model

    (Scale sx sy sz model) -> preservingMatrix $ do
        scale sx sy sz
        graphicsProcess texmgr model

    (EscapeIO act) -> act >>= \as -> mapM_ conv as

    (NullOp) -> return ()

    otherwise -> error "Unsupported Graphics Operation"

graphicsGetViewport :: IO GraphicsSize
graphicsGetViewport =
    get viewport >>= \(_,Size x y) -> 
    return (GraphicsSize (x,y))

graphicsSetViewport :: GraphicsPosition -> GraphicsSize -> IO ()
graphicsSetViewport (GraphicsPosition (px,py)) (GraphicsSize (sx,sy)) = do
    viewport $= (Position px py,Size sx sy)

localFont size = case size of 
        1 -> Helvetica10
        2 -> Helvetica12
        otherwise -> Helvetica18

-- TODO: unloadTexture :: GraphicsTextureReference -> IO ()

-------------------------------------------------------------------------------
-- private functions & data
--

loadOpenGLTexture :: Int -> Int -> Ptr Word8 -> IO TextureObject
loadOpenGLTexture x y rawData =
    genObjectNames 1 >>= \[t] ->
    textureBinding Texture2D $= Just t >>
    texImage2D Nothing NoProxy 0 RGB8 (TextureSize2D x' y') 0 (PixelData RGB UnsignedByte rawData) >>
    return t
    where (x',y') = (fromIntegral x, fromIntegral y)

lookupTexture :: GraphicsTextureManager -> GraphicsTextureReference -> IO TextureObject
lookupTexture mgr ref = do
    hFlush stdout
    tc <- get (textureMap mgr)
    case DM.lookup ref tc of
        -- cache hit
        (Just tobj) -> return tobj

        -- cache miss, load it
        Nothing -> do
            hFlush stdout
            GraphicsTexture (x,y,rawData) <- (textureLoader mgr) ref
            txtObj <- loadOpenGLTexture x y rawData
            textureMap mgr $= (DM.insert ref txtObj tc)
            free rawData
            return txtObj

-------------------------------------------------------------------------------
-- Graphics rendering primitives
--

radiusThreshold = 1

maxArcPoints :: Graphics1d -> Graphics1d
maxArcPoints r = (max 64 (r/10))

circle :: Graphics1d -> IO ()
circle r = renderPrimitive Graphics.Rendering.OpenGL.Polygon (mapM_ (\(x, y)->vertex$Vertex3 x y 0.0) (circlePoints (0,0::Graphics1d) r))

square :: Graphics1d -> IO ()
square sz' = do
    let sz = realToFrac sz'
    renderPrimitive LineLoop $ do
        vertex $ Vertex3 (-sz) sz (0.0::Graphics1d)
        vertex $ Vertex3 sz sz (0.0::Graphics1d)
        vertex $ Vertex3 sz (-sz) (0.0::Graphics1d)
        vertex $ Vertex3 (-sz) (-sz) (0.0::Graphics1d)

circlePoints :: Graphics2d-> Graphics1d -> [Graphics2d]
circlePoints (xoff, yoff) r
    | r > radiusThreshold = 
        let n = (maxArcPoints r) in map (\k -> let t = 2*pi*k/n in 
               (xoff+(sin(t)*r),yoff+(cos(t)*r)))  [1..n]

    | otherwise = []

arcPoints :: Graphics2d -> Graphics2d -> Graphics2d -> Graphics1d -> [Graphics2d]
arcPoints (startx,starty) (endx,endy) ctr@(centrex,centrey) rad = arc2 ctr theta1 theta2 rad
  where
    theta1 = atan2 (startx-centrex) (starty-centrey)
    theta2 = atan2 (endx-centrex) (endy-centrey)

    -- generates the points for a right-hand (clockwise) arc
    --
    arc2 :: Graphics2d -> Graphics1d -> Graphics1d -> Graphics1d -> [Graphics2d]
    arc2 (xoff, yoff) th1 th2 r
        | r > radiusThreshold =
          map (\k -> let t = normal(th2-th1)*k/rez in 
              (xoff+(sin(t+th1)*r),yoff+(cos(t+th1)*r)))  [0..rez]

        | otherwise = []

          where normal n = if n < 0 then (2*pi+n) else n
                rez = maxArcPoints r

dbl2glf :: Double -> Graphics1d
dbl2glf d = d

