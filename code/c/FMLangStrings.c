#include "Platform.h"

/*
 * general-purpose strings
 *
 */

const char *OKString = "OK";
const char *CancelString = "Cancel";

const char *MessageFlagString = "MSG";

/*
 * Time labels
 *
 */

const char *TimeModeStr[] = { "ETE","ETA","LOC","UTC","ETV", "ETT" };

/*
 * GPS events
 *
 */

const char *GPSFixLostString = "GPS Fix Lost";
const char *GPSFix2dString = "GPS Fix 2D";
const char *GPSFix3dString = "GPS Fix 3D";

/*
 * Turn & VNAV events
 *
 */

const char *TurnIn2MinutesString = "Approaching Turn";
const char *TurnAnticipateOffOption="Off";
const char *VNAVIn2MinutesString = "Approaching VNAV";
const char *VNAVOptionString = "VNAV";

/*
 * OBS Input prompts
 *
 */

const char *OBSInputPrompt = "Set OBS";
const char *OBSLabelStrings[4] = { "OK", "Resume", NULL, NULL };

/*
 * Vertical Navigation (VNAV) prompts
 *
 */

const char *VNAltPrompt = "Set Target Altitude";
const char *VNAltButtons[4] = { "AMSL", "Above", "Cancel", NULL };

const char *VNRatePrompt = "Set Rate";
const char *VNRateButtons[2][4] = {
	{"ft/min","ft/Nm","Degrees","Cancel" }, 	// Nautical/statute
	{"m/min", "m/Km","Degrees", "Cancel" } };	// metric

const char *VNByPrompt = "Set distance before";
const char *VNByButtons[4] = { "OK", "Waypoint", "Cancel", NULL };

const char *VNGPSAltPrompt = "Set Barometric Altitude";
const char *VNGPSButtons[4] = { "AMSL", "Reset", "Cancel", NULL };

/*
 * Obstacle warning event
 *
 */

const char *ObstacleNearby = "Near Obstacle(s)";
const char *ObstacleMapOption = "Map";
const char *ObstacleOffOption = "Off";

/*
 * power save events
 * 
 */

const char *PowerSaveOn = "Power Saving ON";
const char *PowerSaveOff = "Power Saving OFF";

/*
 * Pan-to buttons
 *
 */

const char *PanToCursor = "Csr";
const char *PanToWaypoint= "Wpt";

/*
 * FlightPlan button
 *
 */

const char *AlternatePlan = "Load Alternate Plan";
const char *OriginalPlan = "Load Original Plan";

/*
 * Terrain Reference Keypad
 *
 */

const char *TerrainRefPrompt = "Set Terrain Reference";
const char *TerrainRefButtons[4] = { "AMSL", "GPS", "Cancel", NULL };

/*
 * Diversion Manager
 *
 */

const char *CancelDiversion = "Restore flight plan from %s? (All edits will be lost)";

/*
 * Timers
 *
 */

const char *TimerModes[3] = { "Stop", "Flight", "Run" };
const char *TimerButtons[4] = { "Once", "Recycle","Count-up","Cancel" };
const char *TimerMessage = "%s timer expired";
const char *TimerMessageButton = "Timer";



const char *Str2DOnly = "2-D ONLY";
const char *StrAltitude = "Altitude";
const char *StrAppendIdent = "APPEND %s";
const char *StrBYNOTAM = "BY NOTAM";
const char *StrBackCourse = "BC";
const char *StrCancelOBSFirst = "CANCEL OBS FIRST";
const char *StrCheckingObstacleDB = " Checking Obstacle DB %d...";
const char *StrChooseGPS = "Choose GPS";
const char *StrCreatingdatabaseStructure = "Creating database structure";
const char *StrDegrees = "degrees";
const char *StrDeleteIdent = "DELETE %s";
const char *StrErrorDetectedPurgingDatabase = "Error detected - purging database";
const char *StrFilenotfound = "File not found";
const char *StrGPS = "GPS";
const char *StrGoto = "GOTO";
const char *StrGotoIdent = "GOTO %s";
const char *StrH = "H";
const char *StrHeading = "Heading";
const char *StrPlan = "Plan";
const char *StrHz = "Hz";
const char *StrLTMap = "<map";
const char *StrLTPan = "<pan";
const char *StrLoaded = "LOADED";
const char *StrLoadingCoPilotSystemDatabase = " Loading CoPilot System Database";
const char *StrLoadingCoPilotUserDatabase = " Loading CoPilot User Database";
const char *StrMap = "Map";
const char *StrMapNum = "Map %d";
const char *StrMark = "MRK";
const char *StrNoFix = "No Fix";
const char *StrNoMemory = "No Memory";
const char *StrNoPositionFix = "NO POSITION FIX";
const char *StrOFF = "OFF";
const char *StrPan = "Pan";
const char *StrPleaseWait = "Please Wait";
const char *StrPurgingDatabase = "Purging database";
const char *StrSFC = "SFC";
const char *StrSearching = "Searching...";
const char *StrSegmentNum = "Segment: %d/%d";
const char *StrSetTimer = "Set Timer (hhmm or mm)";
const char *StrSim = "Sim";
const char *StrSpeed = "Speed";
const char *StrStartAt = "START AT %s";
const char *StrTref = "T-ref: %s%s";
const char *StrUNLTD = "UNLTD";
const char *StrV = "V";
const char *StrVP = "VP";
const char *StrWA = "WA";
const char *StrWarnGPSNotDetected = "WARN: GPS NOT DETECTED";
const char *StrWarnNoPosition = "WARN: NO POSITION";
const char *StrInitialiseMap = "TYPE A WAYPOINT ID";
 const char *StrBlankFlight = "Blank Plan";
