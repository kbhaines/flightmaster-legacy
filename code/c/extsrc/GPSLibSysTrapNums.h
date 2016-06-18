/*********************************************************************
*
*   MODULE NAME:
*       GPSLibSysTrapNums.h - Contains the SYS_TRAP numbers for the
*           GPSLib library that can be used in both the 68K code that
*           uses the library, and the ARM shim code inside the
*           library.
*
*   Copyright 2002-2004 by Garmin Ltd. or its subsidiaries.
*
*********************************************************************/

#ifndef __GPSLIBSYSTRAPNUMS_H__
#define __GPSLIBSYSTRAPNUMS_H__

#ifndef __MC68K__
    #include <LibTraps.h>
#endif

/********************************************************************
 * Traps
 ********************************************************************/
/*enum
    {
    gpsLibTrapClose = sysLibTrapCustom,
    gpsLibTrapGetLibAPIVersion,
    gpsLibTrapGetMaxSatellites,
    gpsLibTrapGetPosition,
    gpsLibTrapGetPVT,
    gpsLibTrapGetSatellites,
    gpsLibTrapGetStatus,
    gpsLibTrapGetTime,
    gpsLibTrapGetVelocity,
    gpsLibTrapOpen
    };
    */
#define gpsLibTrapClose 0xA805
#define gpsLibTrapGetLibAPIVersion 0xa806
#define gpsLibTrapGetMaxSatellites 0xa807
#define gpsLibTrapGetPosition 0xa808
#define gpsLibTrapGetPVT 0xa809
#define gpsLibTrapGetSatellites 0xa80a
#define gpsLibTrapGetStatus 0xa80b
#define gpsLibTrapGetTime 0xa80c
#define gpsLibTrapGetVelocity 0xa80d
#define gpsLibTrapOpen 0xa80e


#endif  //__GPSLIBSYSTRAPNUMS_H__
