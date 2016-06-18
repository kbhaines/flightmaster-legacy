#ifndef GARMINGPS_H_
#define GARMINGPS_H_

#include "Platform.h"
#include "Gps.h"
#include "Constants.h"

Boolean GarminGPSOpen(void) GPS_SECTION;

Boolean GarminGPSRead(GPSType *GPS) GPS_SECTION;

void GarminGPSClose(void) GPS_SECTION;

#endif /*GARMINGPS_H_*/
