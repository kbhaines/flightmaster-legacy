#include "Platform.h"

/*
 * general-purpose strings
 *
 */

const char *OKString = "_OK";
const char *CancelString = "_Cancel";

const char *MessageFlagString = "_MSG";

/*
 * Time labels
 *
 */

const char *TimeModeStr[] = { "_ETE","_ETA","_LOC","_UTC","_ETV", "_ETT" };

/*
 * GPS events
 *
 */

const char *GPSFixLostString = "_GPS Fix Lost";
const char *GPSFix2dString = "_GPS Fix 2D";
const char *GPSFix3dString = "_GPS Fix 3D";

/*
 * Turn & VNAV events
 *
 */

const char *TurnIn2MinutesString = "_Approaching Turn";
const char *TurnAnticipateOffOption="_Off";
const char *VNAVIn2MinutesString = "_Approaching VNAV";
const char *VNAVOptionString = "_VNAV";

/*
 * OBS Input prompts
 *
 */

const char *OBSInputPrompt = "_Set OBS";
const char *OBSLabelStrings[4] = { "_OK", "_Resume", NULL, NULL };

/*
 * Vertical Navigation (VNAV) prompts
 *
 */

const char *VNAltPrompt = "_Set Target Altitude";
const char *VNAltButtons[4] = { "_AMSL", "_Above", "_Cancel", NULL };

const char *VNRatePrompt = "_Set Rate";
const char *VNRateButtons[2][4] = {
	{"_ft/min","_ft/Nm","_Degrees","_Cancel" }, 	// Nautical/statute
	{"_m/min", "_m/Km","_Degrees", "_Cancel" } };	// metric

const char *VNByPrompt = "_Set distance before";
const char *VNByButtons[4] = { "_OK", "_Waypoint", "_Cancel", NULL };

const char *VNGPSAltPrompt = "_Set Barometric Altitude";
const char *VNGPSButtons[4] = { "_AMSL", "_Reset", "_Cancel", NULL };

/*
 * Obstacle warning event
 *
 */

const char *ObstacleNearby = "_Near Obstacle(s)";
const char *ObstacleMapOption = "_Map";
const char *ObstacleOffOption = "_Off";

/*
 * power save events
 * 
 */

const char *PowerSaveOn = "_Power Saving ON";
const char *PowerSaveOff = "_Power Saving OFF";

/*
 * Pan-to buttons
 *
 */

const char *PanToCursor = "_Csr";
const char *PanToWaypoint= "_Wpt";

/*
 * FlightPlan button
 *
 */

const char *AlternatePlan = "_Load Alternate Plan";
const char *OriginalPlan = "_Load Original Plan";

/*
 * Terrain Reference Keypad
 *
 */

const char *TerrainRefPrompt = "_Set Terrain Reference";
const char *TerrainRefButtons[4] = { "_AMSL", "_GPS", "_Cancel", NULL };

/*
 * Diversion Manager
 *
 */

const char *CancelDiversion = "_Restore flight plan from %s? (All edits will be lost)";

/*
 * Timers
 *
 */

const char *TimerModes[3] = { "_Stop", "_Flight", "_Run" };
const char *TimerButtons[4] = { "_Once", "_Recycle","_Count-up","_Cancel" };
const char *TimerMessage = "_%s timer expired";
const char *TimerMessageButton = "_Timer";



const char *Str2DOnly = "_2-D ONLY";
const char *StrAltitude = "_Altitude";
const char *StrAppendIdent = "_APPEND %s";
const char *StrBYNOTAM = "_BY NOTAM";
const char *StrBackCourse = "_BC";
const char *StrCancelOBSFirst = "_CANCEL OBS FIRST";
const char *StrCheckingObstacleDB = "_ Checking Obstacle DB %d...";
const char *StrChooseGPS = "_Choose GPS";
const char *StrCreatingdatabaseStructure = "_Creating database structure";
const char *StrDegrees = "_degrees";
const char *StrDeleteIdent = "_DELETE %s";
const char *StrErrorDetectedPurgingDatabase = "_Error detected - purging database";
const char *StrFilenotfound = "_File not found";
const char *StrGPS = "_GPS";
const char *StrGoto = "_GOTO";
const char *StrGotoIdent = "_GOTO %s";
const char *StrH = "_H";
const char *StrHeading = "_Heading";
const char *StrPlan = "_Plan";
const char *StrHz = "_Hz";
const char *StrLTMap = "_<map";
const char *StrLTPan = "_<pan";
const char *StrLoaded = "_LOADED";
const char *StrLoadingCoPilotSystemDatabase = "_ Loading CoPilot System Database";
const char *StrLoadingCoPilotUserDatabase = "_ Loading CoPilot User Database";
const char *StrMap = "_Map";
const char *StrMapNum = "_Map %d";
const char *StrMark = "_MRK";
const char *StrNoFix = "_No Fix";
const char *StrNoMemory = "_No Memory";
const char *StrNoPositionFix = "_NO POSITION FIX";
const char *StrOFF = "_OFF";
const char *StrPan = "_Pan";
const char *StrPleaseWait = "_Please Wait";
const char *StrPurgingDatabase = "_Purging database";
const char *StrSFC = "_SFC";
const char *StrSearching = "_Searching...";
const char *StrSegmentNum = "_Segment: %d/%d";
const char *StrSetTimer = "_Set Timer (hhmm or mm)";
const char *StrSim = "_Sim";
const char *StrSpeed = "_Speed";
const char *StrStartAt = "_START AT %s";
const char *StrTref = "_T-ref: %s%s";
const char *StrUNLTD = "_UNLTD";
const char *StrV = "_V";
const char *StrVP = "_VP";
const char *StrWA = "_WA";
const char *StrWarnGPSNotDetected = "_WARN: GPS NOT DETECTED";
const char *StrWarnNoPosition = "_WARN: NO POSITION";
const char *StrInitialiseMap = "_TYPE A WAYPOINT ID";
 const char *StrBlankFlight = "_Blank Plan";
