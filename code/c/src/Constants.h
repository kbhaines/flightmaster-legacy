#ifndef CONSTANTS_H
#define CONSTANTS_H

/*
 * Creator ID can be either BHMN or GPFM. GPFM is for beta test versions.
 * Change file names for index and waypoints when the creator Id changes. 
 *
 * Don't forget to change FlightMaster.def accordingly!!!
 *
 */

#ifdef AEROPALM

#define APPNAME "APNav"
#define FILEROOT "/PALM/Programs/AeroPalm/"
#define FlightMasterCreatorId     'AP-N'
#define BetaCreatorID        'AP-N'
//#define REGCODEPRIME         103099 v1.0
//#define REGCODEPRIME         102103 v2.0
#define REGCODEPRIME         102109

#else

#define APPNAME "FlightMaster7"
#define FILEROOT "/PALM/FM/"

// change in Platform.c too
#define FlightMasterCreatorId 'BHMN'
#define BetaCreatorID        'BHMN'

#ifdef DEMO
#define REGCODEPRIME		 'CODE'
#else
//#define REGCODEPRIME         93997
#define REGCODEPRIME 	     95707
#define DEMOCODEPRIME        93007
#define REGCODESEED          0
#define YEAREPOCH            2009
#endif

#endif

#define FlightMasterDatabaseType  'WAYP'
#define AirspaceDatabaseType 'airs'
#define TFRDatabaseType      'tfrs'

#define MAIN                  __attribute__(( ))
#define SECTION2              __attribute__((section("section2")))
#define SECTION3              __attribute__((section("section3")))
#define SECTION4              __attribute__((section("section4")))
#define SECTION5              __attribute__((section("section5")))
#define SECTION6              __attribute__((section("section6")))
#define SECTION7              __attribute__((section("section7")))
#define SECTION8              __attribute__((section("section8")))
#define SECTION9              __attribute__((section("section9")))
#define SECTION10              __attribute__((section("sectio10")))

#define CPINTERFACE_SECTION   SECTION2
#define HSI_SECTION           SECTION2
#define STARTUP_SECTION       SECTION2
#define DBHANDLER_SECTION     SECTION2
#define MAPICON_SECTION       SECTION2

#define AI_SECTION            SECTION3
#define DIVERSION_SECTION     SECTION3
#define MAINFORM_SECTION      SECTION3
#define ASDB_SECTION          SECTION3
#define FLIGHTPLAN_SECTION    SECTION3

#define EDITWAYPOINT_SECTION  SECTION4
#define MENUHANDLER_SECTION   SECTION4
#define GPSFORM_SECTION       SECTION4
#define AVCALCS_SECTION       SECTION4
#define GPS_SECTION           SECTION4
#define TIMERFORM_SECTION     SECTION4

#define MAP_SECTION           SECTION5

#define UTILS_SECTION         SECTION6
#define DIVERSIONMGR_SECTION  SECTION6
#define MODALDIALOGS_SECTION  SECTION6
#define ALARM_MANAGER_SECTION SECTION6

#define ADT_SECTION           SECTION7
#define ADT2_SECTION          SECTION8
#define ADT3_SECTION          SECTION9

#define PLATFORM_SECTION      SECTION10

#endif
