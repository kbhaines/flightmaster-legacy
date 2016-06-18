module Runway where

import qualified Data.ByteString.Char8 as C
import Data.Binary
import List
import Maybe
--import Graphics.Rendering.OpenGL hiding (get)
--import Graphics.UI.GLUT hiding (get)

import Text.Printf
import Coord
import Graphics
import Utils

type RunwayIdent = SString

-------------------------------------------------- 
-- need to change to field-format {} as for waypoint, above
-- Runway: RunwayIdent, Length (feet)
data Runway = Runway {
    rwIdent :: RunwayIdent,
    rwCoord :: Coord,
    rwLength :: Int,
    rwWidth :: Int,
    rwElevation :: Int
} deriving (Show, Read, Eq, Ord)

-- function returning list of runway Idents
runwayIdents :: [Runway] -> [RunwayIdent]
runwayIdents r = map rwIdent r

-- determine if runway is landable
runwayLandable :: Int -> Runway -> Bool
runwayLandable len rw = rwLength rw >= len

-- returns runways longer than specification
runwayFilter :: Int -> [Runway] -> [Runway]
runwayFilter len rw = filter (\x -> runwayLandable len x) rw

runwayFlipIdent :: RunwayIdent -> RunwayIdent
runwayFlipIdent id = mkSString (printf "%02d%s" flipHdg flipLRC) 
  where
    flipHdg = if hdg < 19 then hdg+18 else hdg-18
    hdg =  case C.readInt $ C.take 2 id of
            Just (x,_) -> x
            Nothing -> error "Invalid runway Ident"
    flipLRC = if C.length id == 3 then
                case (id `C.index` 2) of
                    'L' -> "R"
                    'R' -> "L"
                    'C' -> "C"
                    otherwise -> error "unrecognised runway LRC"
                else
                    ""

runwayOtherEnd:: [Runway] -> RunwayIdent -> Runway
runwayOtherEnd rwys id = 
    case find (\x -> id == runwayFlipIdent (rwIdent x)) rwys of
        Just a -> a
        Nothing -> error "Other end of runway not found"


-------------------------------------------------------------------------------
-- draw all the runways, using the specified transformation function
-- to map from world to display coordinates
--
runwayActions :: [Runway] -> XFormFunction -> [GraphicsCmd]
runwayActions rwys xform = do
    map (\rwy  -> drawOneRunway rwy (runwayOtherEnd rwys (rwIdent rwy)) xform)
        $ filter (\r -> coordLat (rwCoord r) /= 0) rwys

-------------------------------------------------------------------------------
--draw a single runway, for which we need to know the runway at the other
--end (i.e. if drawing 09, we also need the record for rwyEnd=27)

drawOneRunway ::  Runway -> Runway -> XFormFunction -> GraphicsCmd
drawOneRunway (Runway id pos len width elev) rwyEnd xform = TransRotate False (x,y,z) (-rwyHdg) prims
    where
        (x,y) = xform pos
        z = (50.0::Graphics1d)
        (x',y') = xform $ rwCoord rwyEnd
        rwyHdg = angleOfLine x y x' y'

    -- length and width could be better handled, perhaps by some
    -- call to the xform function to return the unit vector for feet per 
    -- display unit
        rwyViewLength = sqrt ((x'-x)^2 + (y'-y)^2) / 2
        rwyViewWidth = max (fromIntegral width * (rwyViewLength / fromIntegral len)) 2

        prims = [ Polygon (GraphicsColour3 0 0 0) 1 Nothing 
                    (UntexturedPolygon [ ( -rwyViewWidth,0,z), (-rwyViewWidth, rwyViewLength,z),
                      ( rwyViewWidth, rwyViewLength,z), (rwyViewWidth,0,z) ])
                ] 

{-

    -- label
    translate $ Vector3 x y z
    color $ Color3 (0.5::Graphics1d) 0.5 0.5
    translate $ Vector3 (-rwyViewWidth*2) (0::Graphics1d) 3 
    scale 0.12 0.12 (0.12::Graphics1d)
    renderString Roman (unSString id)

-}

-------------------------------------------------------------------------------
-- calculates angle of line in degrees
--
angleOfLine :: Graphics1d -> Graphics1d -> Graphics1d -> Graphics1d -> Graphics1d
angleOfLine x1 y1 x2 y2 = 
    if tancalc < 0 then 
        360+realToFrac (rad2deg tancalc)
      else
          realToFrac (rad2deg tancalc)

    where
        tancalc = atan2 (realToFrac(x2-x1)) (realToFrac(y2-y1))


-------------------------------------------------------------------------------
-- Binary import and export
--
-- We may want to consider automatic derivation using the Scrap Your
-- Boilerplate generics module.
--

instance Binary Runway where
    put (Runway ident coord length width elev) = do
        put ident
        put coord
        put length
        put width
        put elev

    get = do
        ident <- get
        coord <- get
        length <- get
        width <- get
        elev <- get
        return (Runway ident coord length width elev)
