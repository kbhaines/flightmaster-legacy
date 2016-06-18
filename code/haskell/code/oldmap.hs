------------------------------------------------------------------------------
-- Map
-------------------------------------------------------------------------------

module Map where

import Coord
import Graphics
import Airspace
import Waypoint as WP
import Terrain
import Data.Maybe
import qualified Data.Set as DS

import qualified Window as Win
import WindowMessage

import qualified GPS 
import UI
import FlightPlan

-------------------------------------------------------------------------------
--
-- Terminology:
--
-------------------------------------------------------------------------------

data MapState = MapState {

    -- this is used if GPS position is unavailable, or if the GPS position has been
    -- lost, or if the user has selected a different map centre (dragged, waypoint selected)
 
    mapDragged :: Maybe Coord  -- Just p, if map has been dragged

  , mapScale :: Double
  , mapTrackUp :: Bool
  , mapUserHeading :: Double
  , mapAirspaceClasses :: DS.Set Class
  , mapSelection :: Maybe WP.Waypoint
  --, mapPickFunction :: GraphicsName -> Maybe WP.WaypointID

  , mapWaypointDB :: WP.WaypointDB
  , mapAirspaceDB :: AirspaceDB
  , mapPositionFunc :: IO (Maybe (Bool, GPS.GPSData))
  , mapFlightPlan :: FlightPlan


} 

-------------------------------------------------------------------------------
-- exported functions
--

-------------------------------------------------------------------------------

-- returns a transformation function, lat/lon bounding box and latitudeScaling
-- factor for the given centre, height (radians) and current viewport
-- dimensions

getMappingParameters :: Coord -> Double -> Graphics2d -> (XFormFunction,BoundCoords, Double)
getMappingParameters mapCentre mapHeight (vieww,viewh) = 
 
    -- map scaling factors:
    -- x*latScale converts radians (including latitude) into viewport
    -- x*lonScale converts lon-radians into viewport
    --
    -- : minimum bounding rectangle

    let latScale = viewh'/mapHeight
        vieww' = vieww
        viewh' = viewh
        aspectRatio = vieww' / viewh'
        mapWidth = mapHeight * aspectRatio
        diagonal = sqrt(mapHeight^2 + mapWidth^2) / 2
        mapWidthLon = diagonal / cos (coordLat mapCentre)

        lonScale = latScale * cos (coordLat mapCentre)
        mapBounds = (Coord (coordLat mapCentre + diagonal) (coordLon mapCentre - mapWidthLon),
            Coord (coordLat mapCentre - diagonal) (coordLon mapCentre + mapWidthLon)) in

    (mapCoord mapCentre latScale lonScale, mapBounds, latScale)

-------------------------------------------------------------------------------

drawCentre :: MapState -> Maybe (Bool, GPS.GPSData) -> Coord
drawCentre mapState aircraftPos = 
    if isNothing (mapDragged mapState) && aircraftPos /= Nothing then 
        fromJust.GPS.position.snd.fromJust $ aircraftPos 
      else
        (fromJust $ mapDragged mapState)

-------------------------------------------------------------------------------

mapDraw :: MapState -> Maybe (Bool, GPS.GPSData) -> Win.Window -> [GraphicsCmd]
mapDraw mapState gps win =
    --[EscapeIO $ (mapPositionFunc mapState) >>= \pos -> return $ mapDraw' mapState pos (Win.dimensions wrect) mapState
    ----, EscapeIO $ (putStrLn $ show (windowDimensions wrect))>> return []
    --]
    mapDraw' mapState gps (Win.dimensions win) mapState

mapDraw' :: MapState -> Maybe (Bool, GPS.GPSData) -> Graphics2d -> MapState -> [GraphicsCmd]
mapDraw' mapState aircraftPos winDimensions@(winx,winy) mparams@(MapState dragged scale trackUp usrHdg asClasses mapsel wpdb asdb _ plan) =

  -- Sink this drawing below the other interface/screen elements
  mapRotation aircraftPos mparams [RelTranslate (0,0,-150) $ concat [asActs, wpActs, terActs, planActs, aircraft, selectActs]]

  where 
    (mfunc, mapBounds, latScale) = getMappingParameters (drawCentre mapState aircraftPos) (nm2rad scale) winDimensions

    aircraft = if aircraftPos /= Nothing then
                    let (isFixOK, gps) = fromJust aircraftPos
                        colour = if isFixOK then (GraphicsColour3 0 0.8 0) else (GraphicsColour3 0.8 0 0)
                      in
                        drawAircraft mfunc (fromJust.GPS.position $ gps) (-GPS.track gps) 10 colour
                  else
                    [NullOp]
    wpActs = concatMap (WP.waypointActions mfunc latScale) (WP.inBounds wpdb mapBounds)
    asActs = concatMap (Airspace.airspaceActions mfunc latScale) (Airspace.inBoundsAndFilter asdb mapBounds (classFilter asClasses))
    terActs = Terrain.terrainActions mapBounds mfunc
    planActs = concatMap (planActions mfunc latScale) (planLegs plan)
    selectActs = drawSelection mfunc latScale (mapSelection mapState)

    ---------------------------------------------------------------------------

    mapRotation :: Maybe (Bool, GPS.GPSData) -> MapState -> [GraphicsCmd] -> [ GraphicsCmd ]
    mapRotation (Just (_,gps)) (MapState Nothing _ True _ _ _ _ _ _ _) gcmds = [Rotate (GPS.track gps) gcmds]
    mapRotation Nothing (MapState Nothing _ _ usrHdg _ _ _ _ _ _) gcmds = [Rotate usrHdg gcmds]
    mapRotation _ (MapState _ _ _ _ _ _ _ _ _ _) gcmds = [Rotate 0 gcmds]

    ---------------------------------------------------------------------------

    classFilter ::  DS.Set Class -> Airspace -> Bool
    classFilter set as = (DS.member (asClass as) set)

    ---------------------------------------------------------------------------

    drawAircraft :: XFormFunction -> Coord -> Double -> Graphics1d -> GraphicsColour -> [GraphicsCmd]
    drawAircraft xform posn rotation width col =
        let (aircraftX, aircraftY) = xform posn in
        [TransRotate False (aircraftX, aircraftY,15) rotation 
            [ Graphics.Polygon col 1 Nothing (UntexturedPolygon [ (0,width*2,0), (width,-width*2,0), (-width,-width*2,0) ])]]

    ---------------------------------------------------------------------------

    drawSelection :: XFormFunction -> Double -> Maybe WP.Waypoint -> [GraphicsCmd]
    drawSelection _ _ Nothing = []
    drawSelection xform _ (Just wp) = 
        let (x,y)= xform $ WP.coord wp in
        [Translate (x,y,0) 
            [Graphics.Polyline (GraphicsColour3 0 0 0) 3 Nothing 
             [ (-15,-15,0),(15,-15,0),(15,15,0),(-15,15,0),(-15,-15,0) ]]]

    ---------------------------------------------------------------------------

    planActions :: XFormFunction -> Double -> Leg -> [GraphicsCmd]
    planActions mfunc _ leg = [Polyline (GraphicsColour3 0 0.5 0) 4.0 Nothing legPts]
      where
        legPts = map (pointToPlane 2.0 . mfunc) (legSegmentate leg (pi/180))

-------------------------------------------------------------------------------

mapHandleEvent :: MapState -> Win.Window -> Win.MessageBody -> Win.QueryFunction -> Win.Response
mapHandleEvent mapState win Win.Draw queryFunc = Win.DrawCmds $ mapDraw mapState' gps win
  where 
    mapState' = mapState { 
         mapAirspaceClasses =  DS.fromList $ map (\(box,cls) -> cls) 
            $ filter (\(box,_) -> queryFunc box 0 == BoolValue True) boxToClass
       , mapTrackUp = trackUp
    }
       
    (BoolValue trackUp) = queryFunc trackUpCheckboxID 0
    (SVGPSData gps) = queryFunc (Win.ID 0) globalGPSPosition
    boxToClass = [
        (suasCheckbox, SUAS)
      , (classACheckbox, ClassA)
      , (classBCheckbox, ClassB)
      , (classCCheckbox, ClassC)
      , (classDCheckbox, ClassD)
      , (classECheckbox, ClassE)
      , (classFCheckbox, ClassF)
      ]


mapHandleEvent _ win (Win.AppWindowResize newsize) _ = Win.Update (Win.resize win newsize) Nothing

mapHandleEvent mapState win (Win.StateQuery qry) queryFunc
  | qry == mapWaypointSelected = Win.Response $ BoolValue $ not (isNothing (mapSelection mapState))
  | qry == mapWaypointSelection = Win.Response $ SVWaypoint $ fromJust (mapSelection mapState)
  | qry == mapVarPosition = Win.Response $ SVCoord $ (drawCentre mapState gps)
  | qry == globalFlightPlan = Win.Response $ SVFlightPlan $ mapFlightPlan mapState
  where
    (SVGPSData gps) = queryFunc (Win.ID 0) globalGPSPosition

mapHandleEvent mapState win (Win.StateUpdateNotify _ (Just (Win.SyncVar (varID, (SVFlightPlan newFp))))) _ 
 | varID == globalFlightPlan = 
    Win.UpdatePrivate $ win { Win.handleMessage = mapHandleEvent (mapState { mapFlightPlan = newFp }) }

-- Determine view state of mapResetViewButton based on current GPS data
mapHandleEvent mapState win (Win.StateUpdateNotify _ (Just (Win.SyncVar (varID, (SVGPSData gps))))) _ 
 | varID == globalGPSPosition = 
    Win.UpdatePrivate $ win { Win.children = newChildren }
    where
        newChildren     -- : use hidden/visible attributes!!
         | isNothing gps || isNothing (mapDragged mapState) = filter (\w -> Win.ident w /= mapResetViewButtonID) (Win.children win)
         | mapResetViewButton `notElem` (Win.children win) = mapResetViewButton : (Win.children win)
         | otherwise = Win.children win


mapHandleEvent mapState win (Win.LeftButtonDown _ gids) queryFunc 
  | gids /= [] = 
    let newStateSelected = mapState { mapSelection = Just selectedWaypoint }
        selectedWaypoint = findByID (mapWaypointDB mapState) (fromGraphicsName (head gids))
      in
    Win.Update win { Win.handleMessage = mapHandleEvent newStateSelected } 
        (Just $ Win.SyncVar (globalSelection, SVWaypoint selectedWaypoint))

  | otherwise = 
    let newStateUnselected = mapState { mapSelection = Nothing } 
        in
    Win.Update win { Win.handleMessage = mapHandleEvent newStateUnselected } 
        (Just $ Win.SyncVar (globalSelection, BoolValue False))

-- handle drag event. 
mapHandleEvent mapState win (Win.LeftDrag (GraphicsPosition (dx,dy)) _) queryFunc
 | mapDragged mapState == Nothing = 
    -- starting off a new drag. Make note of the current centre of the map
    Win.Update win {

        Win.handleMessage = mapHandleEvent (mapState {mapDragged = Just newCentre} )
      , Win.children = mapResetViewButton : (Win.children win)
        
    } Nothing

 | otherwise = 
    -- continuing an existing drag operations. Calculate the new offset based on the
    -- last drawn position
    Win.Update win {
        Win.handleMessage = mapHandleEvent (mapState {mapDragged = Just newCentre} )
    } Nothing
    where
        currentCentre@(Coord oldLat oldLon) = drawCentre mapState gps
        newCentre = Coord newLat newLon
        newLat = oldLat + ((fromIntegral dy)/latScale)
        newLon = oldLon - ((fromIntegral dx)/(latScale*cos oldLat))
        (_, _, latScale) = getMappingParameters currentCentre (nm2rad $ mapScale mapState) winDimensions
        winDimensions = Win.dimensions win
        (SVGPSData gps) = queryFunc (Win.ID 0) globalGPSPosition

mapHandleEvent mapState win (Win.StateUpdateNotify buttonID (Just Win.Clicked)) _
  | buttonID == mapResetViewButtonID = Win.Update (win { 
        Win.handleMessage = mapHandleEvent (mapState { mapDragged = Nothing})
      , Win.children = filter (\w -> Win.ident w /= mapResetViewButtonID) (Win.children win)
      }) Nothing
  | buttonID == zoomOutButtonID = Win.Update ( win {
        Win.handleMessage = mapHandleEvent (mapState { mapScale = min ((mapScale mapState)*1.1) 150.0 })
      }) Nothing
  | buttonID == zoomInButtonID = Win.Update ( win {
        Win.handleMessage = mapHandleEvent (mapState { mapScale = max ((mapScale mapState)*0.9) 0.1 })
      }) Nothing

{-
mapHandleEvent mapState win (Win.StateUpdateNotify buttonId (Just Win.Clicked)) _ 
  | buttonId == planButtonID = Win.Update win { Win.handleMessage = mapHandleEvent mapState' } syncv
  | otherwise = Win.NoResponse

  where
    mapState' = mapState { mapFlightPlan = newFp }
    newFp = appendWaypoint (mapFlightPlan mapState) newWp
    newWp = LegWaypoint (findByID wpdb (fromJust $ mapSelection mapState)) 0.0
    wpdb = mapWaypointDB mapState
    syncv = Just $ Win.SyncVar (globalFlightPlan, SVFlightPlan newFp)
-}

mapHandleEvent mp win (Win.Key chr) _
    |isNothing newmp = Win.NoResponse
    |otherwise = Win.Update (win {Win.handleMessage = mapHandleEvent (fromJust newmp) }) Nothing
    where 
        newmp = modifyMapParams 
        modifyMapParams :: Maybe MapState
        modifyMapParams = case chr of
            ('-') -> Just mp { mapScale = min ((mapScale mp)*1.1) 150.0 }
            ('=') -> Just mp { mapScale = max ((mapScale mp)*0.9) 0.1 }
            (' ') -> Just mp { mapTrackUp = not (mapTrackUp mp) }
            ('[') -> Just mp { mapUserHeading = mapUserHeading mp + 3.0 }
            (']') -> Just mp { mapUserHeading = mapUserHeading mp - 3.0 }
            ('a') -> Just mp { mapAirspaceClasses = classToggle ClassA (mapAirspaceClasses mp)}
            ('b') -> Just mp { mapAirspaceClasses = classToggle ClassB (mapAirspaceClasses mp)}
            ('c') -> Just mp { mapAirspaceClasses = classToggle ClassC (mapAirspaceClasses mp)}
            ('d') -> Just mp { mapAirspaceClasses = classToggle ClassD (mapAirspaceClasses mp)}
            ('e') -> Just mp { mapAirspaceClasses = classToggle ClassE (mapAirspaceClasses mp)}
            ('f') -> Just mp { mapAirspaceClasses = classToggle ClassF (mapAirspaceClasses mp)}
            ('s') -> Just mp { mapAirspaceClasses = classToggle SUAS (mapAirspaceClasses mp)}
            ('u') -> Just mp { mapAirspaceClasses = classToggle ClassU (mapAirspaceClasses mp)}
            otherwise -> Nothing

        classToggle cls set = 
            if DS.member cls set then 
                DS.delete cls set
              else
                DS.insert cls set

mapHandleEvent _ _ _ _ = Win.NoResponse

{-
mapKeyboard :: MapState -> Win.Key -> KeyState -> Modifiers -> Position -> MapState
mapKeyboard mp key Down _ _ = case key of
    (Char '-') -> mp { mapScale = min ((mapScale mp)*1.1) 150.0 }
    (Char '=') -> mp { mapScale = max ((mapScale mp)*0.9) 0.1 }
    (MouseButton WheelUp) -> mp { mapScale = min ((mapScale mp)*1.1) 150.0 }
    (MouseButton WheelDown) -> mp { mapScale = max ((mapScale mp)*0.9) 0.1 }
    (Char ' ') -> mp { mapTrackUp = not (mapTrackUp mp) }
    (Char '[') -> mp { mapUserHeading = mapUserHeading mp + 3.0 }
    (Char ']') -> mp { mapUserHeading = mapUserHeading mp - 3.0 }
    (Char 'a') -> mp { mapAirspaceClasses = classToggle ClassA (mapAirspaceClasses mp)}
    (Char 'b') -> mp { mapAirspaceClasses = classToggle ClassB (mapAirspaceClasses mp)}
    (Char 'c') -> mp { mapAirspaceClasses = classToggle ClassC (mapAirspaceClasses mp)}
    (Char 'd') -> mp { mapAirspaceClasses = classToggle ClassD (mapAirspaceClasses mp)}
    (Char 'e') -> mp { mapAirspaceClasses = classToggle ClassE (mapAirspaceClasses mp)}
    (Char 'f') -> mp { mapAirspaceClasses = classToggle ClassF (mapAirspaceClasses mp)}
    (Char 's') -> mp { mapAirspaceClasses = classToggle SUAS (mapAirspaceClasses mp)}
    (Char 'u') -> mp { mapAirspaceClasses = classToggle ClassU (mapAirspaceClasses mp)}
    otherwise -> mp

    where
        classToggle cls set = 
            if DS.member cls set then 
                DS.delete cls set
              else
                DS.insert cls set

mapKeyboard mp _ _ _ _ = mp

mapSelector :: WaypointDB -> AirspaceDB -> GraphicsName -> Maybe WaypointID
mapSelector wpdb _ (GraphicsName id) = Just (WaypointID (id', findByID wpdb (id')))
    where id' = fromIntegral id

-}

-------------------------------------------------------------------------------
-- private functions & data
--


-------------------------------------------------------------------------------
-- map from coordinate to display coordinates, using the specified scaling
-- factors and reference position
-- 
mapCoord :: Coord -> Double -> Double -> Coord -> Graphics2d
mapCoord ref latscale lonscale c1 = (x,y)
  where
    x = realToFrac ((coordLon c1 - coordLon ref)*lonscale)
    y = realToFrac ((coordLat c1 - coordLat ref)*latscale)

reverseMapCoord :: Coord -> Double -> Double -> Graphics2d -> Coord
reverseMapCoord reference latscale lonscale (x,y) = Coord lat lon
  where
    lat = (y' / lonscale) + (coordLat reference)
    lon = (x' / latscale) + (coordLon reference)
    x' = realToFrac x
    y' = realToFrac y

