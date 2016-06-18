{-# LANGUAGE NamedFieldPuns #-}

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


import Control.Monad.State

import Utils
import qualified Window as Win

import MapParameters as Mp
import Navigation as Nav
import ObjectDB as DB

import qualified Selection as Sel
import MessageProcessor
import PlanEditor as PE

import UID

import MapNames

-- |A map environment is needed in order for the map to respond to window
-- events

data MapEnvironment = MapEnvironment {
    
    dbMP :: ObjectDBMP
  , editMP :: EditMP
  , mapMP :: MapParametersMP
  , navMP :: NavigationMP
  , selectionMP :: Sel.SelectionMP

} 


-------------------------------------------------------------------------------

-- |returns a transformation function, lat/lon bounding box and latitudeScaling
-- factor for the given centre, height (radians) and current viewport
-- dimensions

getMappingParameters :: Coord -> Double -> Graphics2d -> (XFormFunction,ReverseXFormFunction, BoundCoords, Double)
getMappingParameters mapCentre mapHeight (vieww,viewh) = 
 
    -- map scaling factors:
    -- x*latScale converts radians (including latitude) into viewport
    -- x*lonScale converts lon-radians into viewport
    --
    -- TODO: minimum bounding rectangle

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

    (mapCoord mapCentre latScale lonScale, reverseMapCoord mapCentre latScale lonScale, mapBounds, latScale)

-------------------------------------------------------------------------------

drawCentre :: Mp.Parameters -> Coord
drawCentre (Mp.Parameters Off (ACOrigin org) _ _ _ _) = org
drawCentre (Mp.Parameters _ (CoordOrigin org) _ _ _ _) = org

-------------------------------------------------------------------------------

mapDraw :: ObjectDBMP -> MapParametersMP -> NavigationMP -> Sel.SelectionMP -> Graphics2d -> [GraphicsCmd]
mapDraw dbMP mapParamsMP navMP selectionMP winDimensions@(winx,winy) = [ EscapeIO acts ] where 
 acts = do
    (DB db) <- dbMP GetDB
    (Sel.CheckpointResponse selCheckpoint) <- selectionMP Sel.GetAsCheckpoint
    (Sel.SelectionResponse selection) <- selectionMP Sel.Get
    (OriginResponse mapOrigin) <- mapParamsMP GetOriginCoord
    (PositionResponse aircraftPos) <- mapParamsMP GetACPosition
    (Nav.PlanResponse plan) <- navMP ReportPlan

    (Mp.Response mapState@(Mp.Parameters pos org scale orient filters _)) <- mapParamsMP GetParameters

    let (mfunc, _, mapBounds, latScale) = getMappingParameters mapOrigin (nm2rad scale) winDimensions

        aircraft = if aircraftPos /= Nothing then
                        let (fixAge, posn, track, speed, alt) = fromJust aircraftPos
                            trackDeg = rad2deg track
                            colour = if fixAge < 3 then (GraphicsColour3 0 0.8 0) else (GraphicsColour3 0.8 0 0)
                          in
                            drawAircraft mfunc (posn) (-trackDeg) 10 colour
                      else
                        [NullOp]

        wpdb = waypointDB db
        asdb = airspaceDB db

        wpActs = namedGraphics MapWaypointName $ concatMap (WP.waypointGraphics mfunc latScale) (WP.inBounds wpdb mapBounds)
        asActs = namedGraphics MapAirspaceName $ concatMap (Airspace.airspaceGraphics mfunc latScale) (Airspace.inBoundsAndFilter asdb mapBounds (classFilter DS.empty))
        terActs = namedGraphics MapTerrainName $ Terrain.terrainGraphics mapBounds mfunc
        route = namedGraphics MapRouteName $ concatMap (
            \(legNo,leg) -> drawRoute mfunc latScale leg legNo (legNo == selectedLeg && (to leg) == selectedLegChkpt)
            )   (zip [1..] $ toLegs plan)
        selectActs = drawSelection mfunc latScale selCheckpoint
        (selectedLeg,selectedLegChkpt) = case selection of
                                            (Sel.Leg legNum chk) -> (legNum,chk)
                                            _ -> (0, toCheckpoint (Coord 0 0))

        classFilter ::  DS.Set Class -> Airspace -> Bool
        --classFilter set as = (DS.member (asClass as) set)
        classFilter _ _ = True

    return [Rotate 0 [RelTranslate (0,0,-150) $ [asActs, wpActs, terActs, route] ++ aircraft ++ selectActs]]

          -- Sink this drawing below the other interface/screen elements


            ---------------------------------------------------------------------------

            --mapRotation :: Maybe (Bool, GPS.GPSData) -> MapState -> [GraphicsCmd] -> [ GraphicsCmd ]
            --mapRotation (Just (_,gps)) (MapState Nothing _ True _ _ _ _ _ _ _) gcmds = [Rotate (GPS.track gps) gcmds]
            --mapRotation Nothing (MapState Nothing _ _ usrHdg _ _ _ _ _ _) gcmds = [Rotate usrHdg gcmds]
            --mapRotation _ (MapState _ _ _ _ _ _ _ _ _ _) gcmds = [Rotate 0 gcmds]

            ---------------------------------------------------------------------------


    where 
    ---------------------------------------------------------------------------

    drawAircraft :: XFormFunction -> Coord -> Double -> Graphics1d -> GraphicsColour -> [GraphicsCmd]
    drawAircraft xform posn rotation width col =
        let (aircraftX, aircraftY) = xform posn in
        [TransRotate False (aircraftX, aircraftY,15) rotation 
            [ Graphics.Polygon col 1 Nothing (UntexturedPolygon [ (0,width*2,0), (width,-width*2,0), (-width,-width*2,0) ])]]

    ---------------------------------------------------------------------------

    drawSelection :: XFormFunction -> Double -> Maybe Checkpoint -> [GraphicsCmd]
    drawSelection _ _ Nothing = []
    drawSelection xform _ (Just chk) = 
        let (x,y)= xform $ checkpointCoord chk in
        [Translate (x,y,0) 
            [Graphics.Polyline (GraphicsColour3 0 0 0) 3 Nothing 
             [ (-15,-15,0),(15,-15,0),(15,15,0),(-15,15,0),(-15,-15,0) ]]]

    ---------------------------------------------------------------------------

    drawRoute :: XFormFunction -> Double -> Leg -> CheckpointNumber -> Bool -> [GraphicsCmd]
    drawRoute mfunc _ leg legNo isSelected = [namedGraphics legNo [Polyline colour thick Nothing legPts]]
      where
        (colour,thick)
          | isSelected = (GraphicsColour3 0 0 0, 10.0)
          | otherwise = (GraphicsColour3 0 0.5 0, 4.0)
        legPts = map (pointToPlane 2.0 . mfunc) (legSegmentate leg (pi/180))


-- |Split the given leg into segments of maximum length 'limit'. If limit is
-- 50, and the leg is 60 long, then the two segments will be 30 long
legSegmentate :: Leg -> Range -> [Coord]
legSegmentate (Leg fromWp toWp _ dist _) limit = p1 : segments ++ [p2]
  where
    (p1,p2) = (checkpointCoord fromWp, checkpointCoord toWp)
    segments 
      | dist < limit = []
      | otherwise = take n $ map (intermediatePointsGC p1 p2 dist) [interval,(2*interval)..1.0]
    interval = 1 / (fromIntegral n + 1)
    n = truncate (dist/limit)

-- |Calculate a point that is a fraction of the way 'f' along the great
-- circle between the two coordinates
intermediatePointsGC :: Coord -> Coord -> Double -> Double -> Coord
intermediatePointsGC (Coord lat1 lon1) (Coord lat2 lon2) dist f = Coord lat lon
  where
    d = dist
    a = sin ((1-f)*d) / sin d
    b = sin (f*d) / sin d
    x = a*cos lat1*cos lon1 + b*cos lat2 * cos lon2
    y = a*cos lat1*sin lon1 + b*cos lat2 * sin lon2
    z = a*sin lat1 + b * sin lat2
    lat = atan2 z (sqrt (x^2+y^2))
    lon = atan2 y x


-- |Makes sure that a consistent state is maintained across Selection, Edit and
-- Window MessageProcessors. Should be called after the SelectionMP is updated
-- by a user selecting a new object.
updateSelection :: Sel.SelectionMP -> EditMP -> NavigationMP -> Win.WindowMP -> IO ()
updateSelection selection edit navigation window = do

    (Sel.SelectionResponse curSelection) <- selection $ Sel.Get

    case curSelection of
        (Sel.DatabaseObject _ chkpt) -> handleCheckpointSelection chkpt
        (Sel.Freepoint chkpt) -> handleCheckpointSelection chkpt
        (Sel.Leg legNum chkpt) -> handleLegSelection legNum chkpt
        _ -> undefined

  where
    handleCheckpointSelection :: Checkpoint -> IO ()
    handleCheckpointSelection checkpoint= do
        (Nav.PlanResponse basePlan) <- navigation ReportPlan
        (Nav.PlanListResponse toPlans) <- navigation $ SearchHistory checkpoint

        -- Query the planning MP to see what editing operations are allowed on that waypoint,
        -- and set up the buttons accordingly
        editOps <- edit $ PE.SetEditCheckpoint toPlans basePlan checkpoint
        setupButtons editOps
        return ()

    handleLegSelection :: CheckpointNumber -> Checkpoint -> IO ()
    handleLegSelection legNum chkpt = do
        (Nav.PlanResponse basePlan) <- navigation ReportPlan
        editOps <- edit $ PE.SetEditLeg basePlan legNum
        setupButtons editOps
        return ()
    
    setupButtons :: PE.Response -> IO ()
    setupButtons (SupportedOps (addAllowed, deleteAllowed,toAllowed)) = do
        let [addButtonState, deleteButtonState,toButtonState] = map buttonState [addAllowed, deleteAllowed, toAllowed]
        _<- window (Win.SetState UID.addButtonID addButtonState)
        _<- window (Win.SetState UID.deleteButtonID deleteButtonState)
        _<- window (Win.SetState UID.toButtonID toButtonState)
        return ()

    buttonState True = Win.Enabled
    buttonState False = Win.Hidden

-- |Make sure that the selection is consistent with the edits that the user
-- has just made to the plan. Should be called after every edit operation
validateSelection :: Sel.SelectionMP -> EditMP -> NavigationMP -> Win.WindowMP -> IO () 
validateSelection selection edit navigation window = do
    (Sel.SelectionResponse selType) <- selection $ Sel.Get

    case selType of
        (Sel.Leg {}) -> undefined
        _ -> undefined

    return ()


{-
To be called after edits to flight plan (to, add, delete buttons etc).
Validate leg selection
Update buttons in winMP according to edit state
-}

--------------------------------------------------------------------------------
-- |Map event handler
mapHandleEvent :: MapEnvironment -> Win.MessageHandler
mapHandleEvent (MapEnvironment {dbMP = db, mapMP = params, navMP=nav, selectionMP = selection}) win (Win.Draw id) = 
    return $ Win.DrawCmds $ mapDraw db params nav selection (Win.dimensions win)

--mapHandleEvent _ win (Win.AppWindowResize newsize) _ = Win.Update (Win.resize win newsize) Nothing

mapHandleEvent (MapEnvironment {mapMP=params, dbMP=db,editMP=edit, navMP=nav,selectionMP=selection}) win (Win.LeftButtonUp id pos gids)
  | gids /= [] = return $ Win.IORequest Win.OK (setSelection $ unEnumGraphicsHit $ head gids)
  | otherwise = do
    forM_ [addButtonID, deleteButtonID, toButtonID] (\w->Win.applyWindowM w Win.hide)
    return $ Win.IORequest Win.OK setToFreepoint 

   where 
    setSelection :: MapElement -> Win.WindowMP -> IO ()
    setSelection MapWaypointName win = do
        
        -- Handle case of object from database being selected
        
        -- TODO: This needs re-writing, also bug may ensue if waypoint element isn't named,
        -- such as the Label part of it, in which case gids has only one element.
        let obid = hitToOBID $ head gids
        (DB.ObjectResponse obj) <- db $ GetObject (fromJust obid)
        if (not $ isNothing obj) then do
              let (WaypointObject (_,wp)) = fromJust obj
                  checkpoint=toCheckpoint wp
              selection $ Sel.SetByObjectReference (fromJust obid) checkpoint
              debug $ show checkpoint
              updateSelection selection edit nav win
              return ()
          else
              return ()

    setSelection MapRouteName win = do

        -- Handle case of leg being selected
        (Nav.PlanResponse plan) <- nav ReportPlan
        let legId = hitToLeg $ head gids
            legChkpt = to $ toLegs plan !! (fromJust legId - 1)
        selection $ Sel.SetLeg (fromJust legId) legChkpt
        updateSelection selection edit nav win

        return ()

    setSelection MapAirspaceName win = do

        -- Handle case of airspace being selected
        setSelection MapTerrainName win
        putStrLn "Airspace!"
        return ()

    setSelection MapTerrainName win = do

        -- Handle case of free-point being selected
        crd <- reverseMap
        selection $ Sel.SetByCoord crd
        updateSelection selection edit nav win
        return ()

    setSelection _ win = do

          -- Handle case of un-handled object being selected (for now)
          _ <- selection $ Sel.Clear
          _  <- edit PE.ClearCheckpoint
          forM_ [addButtonID, deleteButtonID, toButtonID] (\w -> win $ Win.SetState w Win.Hidden)
          return ()
            
    buttonState True = Win.Enabled
    buttonState False = Win.Hidden
    objectToCheckpoint (WaypointObject (_, Waypoint {identifier=id, coord=crd})) = Nav.Checkpoint crd 0.0  (AMSL 0) (Just $ unSString id)
            

    -- |Sets up selection to an arbitrary point on the screen, i.e. not anything under it
    -- such as a waypoint, airspace etc.
    setToFreepoint :: Win.WindowMP -> IO ()
    setToFreepoint _ = do
      selection $ Sel.Clear
      edit $ PE.ClearCheckpoint
      (Coord lat lon) <- reverseMap
      putStrLn $ show (rad2deg lat, rad2deg lon)
      selection $ Sel.SetByCoord $ Coord lat lon
      return ()

    -- | Convert from a graphics hit name to a possible object ID
    hitToOBID :: GraphicsHit -> Maybe OBID
    hitToOBID gh = case hitType of
                    MapWaypointName -> Just $ WaypointOBID fid
                    MapAirspaceName -> Just $ AirspaceOBID fid
                    _ -> Nothing
      where (hitType, gns) = hitToEnum gh
            fid = unGraphicsName $ head gns

    -- |Convert from a graphics hit name to a possible Leg ID
    hitToLeg :: GraphicsHit -> Maybe CheckpointNumber
    hitToLeg gh = case hitType of
                    MapRouteName -> Just $ fid
                    _ -> Nothing
      where (hitType, gns) = hitToEnum gh
            fid = unGraphicsName $ head gns

    -- |Map from screen-to-world coordinates, for the current map parameters
    -- and the click position
    reverseMap :: IO Coord
    reverseMap = do
      (Response mapparams) <- params GetParameters
      let (wx,wy)= Win.dimensions win
          (_,reverseMap,_,_) = getMappingParameters (drawCentre mapparams) (nm2rad $ zoom mapparams) (Win.dimensions win)
          (px,py) = pos
      return $ reverseMap (px-wx/2, wy/2-py)


mapHandleEvent (MapEnvironment {mapMP=params} ) win (Win.LeftDrag id dragVector currentPos) 
 = return $ Win.IORequest Win.OK (\_ -> do
    (OriginResponse currentOrigin@(Coord oldLat oldLon)) <- params GetOriginCoord
    (Response mapParams) <- params GetParameters

    let (dx,dy) = dragVector
        newVector =Coord latVec lonVec
        latVec = (dy/latScale)
        lonVec = -(dx/(latScale*cos oldLat))
        (_, _, _, latScale) = getMappingParameters currentOrigin (nm2rad mapScale) winDimensions
        mapScale = zoom mapParams
        winDimensions = Win.dimensions win

    _ <- params $ SetOffsetVector (Just $ Coord latVec lonVec)
    (Response mapParams) <- params GetParameters
    (OriginResponse origin) <- params GetOriginCoord
    putStrLn $ show (mapParams,origin)
    return ()
    )

mapHandleEvent (MapEnvironment {mapMP=params}) win (Win.LeftDragStop id) = return $ Win.IORequest Win.OK $ (\_ -> do
    putStrLn "Stop"
    _ <- params $ SetOffsetVector Nothing
    return ()
    )

mapHandleEvent (MapEnvironment {mapMP} ) win (Win.LeftDragCompleted id _) = do
    --(Just reset) <- Win.findWindowM resetViewButtonID
    --Win.replaceWindow resetViewButtonID (reset { Win.displayState = Win.Enabled } )
    return $ Win.IORequest Win.OK (dragSet mapMP)

mapHandleEvent _ w _ = return $ Win.NoResponse

--------------------------------------------------------------------------------
-- |Drag setup, invoked when the user finishes dragging the map

dragSet :: MapParametersMP -> Win.WindowMP -> IO ()
dragSet mparams win = do
    putStrLn "Completed"
    _ <- mparams ApplyOffsetVector
    (Mp.Response (Mp.Parameters {origin}) ) <- mparams GetParameters
    case origin of
        (ACOrigin _) -> return ()
        (CoordOrigin _) -> do 
            _<- win (Win.SetState UID.resetViewButtonID Win.Enabled)
            return ()

    return ()


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
    lat = (y' / latscale) + (coordLat reference)
    lon = (x' / lonscale) + (coordLon reference)
    x' = realToFrac x
    y' = realToFrac y

