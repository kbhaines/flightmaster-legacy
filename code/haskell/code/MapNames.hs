-- |Module information

module MapNames where


import Graphics

-- |Type identification for the different interactive elements on the map
data MapElement = 
    MapWaypointName
  | MapAirspaceName
  | MapCheckpointName
  | MapRouteName   
  | MapTerrainName
    deriving (Eq, Read, Show, Enum)

instance GraphicsNameable MapElement where
    toGraphicsName me = mkEnumGraphicsName me
    fromGraphicsName g = unEnumGraphicsName g
