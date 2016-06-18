/*
 * TerrainType
 *
 * Abstract data type for terrain rendering
 *
 * Terrain coordinate system is based on 23 bits of accuracy - 15 bits for lat
 * / lon (-16384 < 0 < 16383) and 8 bits of fixed-point data (in bits 0-7).
 *
 * Latitude is positive for north, longitude is negative for west.
 *
 * Longitude differs from CoPilot/Mapping modules in that longitude is negative
 * in the western direction. 
 *
 * The terrain data must be stored in an external file.
 *
 */

#ifndef TERRAIN_TYPE_H_INCLUDED
#define TERRAIN_TYPE_H_INCLUDED

#include "Platform.h"
#include "Constants.h"


typedef struct TerrainTypeStruct * TerrainType;

// NB Do not use terrainSys
typedef enum { terrainSys = -1, terrainOff, terrainNormal, terrainNormalWarn, terrainWarn } TerrainPaletteType;

/********************************************************************************
 *
 * Public functions
 *
 */

/*
 * TerrainNew
 *
 * Create a new terrain object
 *
 * Cache width & height should NOT be larger than the screen/bitmap into which
 * the terrain will be rendered - terrain code can only scale UP onto the
 * screen
 *
 */

TerrainType TerrainNew(
		
	const char *filename, 		//	name of terrain file on disk

	Int32 cache_width,
	Int32 cache_height			// width & height of internal cache to use

	) ADT2_SECTION;

/*
 * convert from 32bit world coords into floating point (for debugging)
 *
 */

#define WCFLOAT(x) ((float)(180.0*(float)(x) / MAXLON))

/*
 * TerrainFree
 *
 * Delete terrain object
 *
 */

void TerrainFree(TerrainType t) ADT2_SECTION;

/*
 * TerrainSetPaletteEntry
 *
 * Set a new palette entry for the terrain
 * 
 *
 */

void TerrainSetPaletteEntry(TerrainType t, Int32 altFrom, Int32 altTo, UInt8 red, UInt8 green, UInt8 blue) ADT2_SECTION;

/*
 * TerrainLoadPalette
 *
 * Loading a palette from the specified file
 *
 */

void TerrainLoadPalette(TerrainType t, PFFileRef f, Boolean nightMode) ADT2_SECTION;

/*
 * TerrainSetAlertAlt
 *
 */

void TerrainSetAlertAlt(TerrainType t, Int32 altInFeet) ADT2_SECTION;

/*
 * TerrainSetPalette
 *
 * Set the type of display that the rendering engine will draw,
 * determined by 'type'
 *
 */

void TerrainSetPalette(TerrainType t, TerrainPaletteType type, const char *fileName, Boolean nightMode) ADT2_SECTION;

/*
 * TerrainRenderNorthUp
 *
 * Render the specified terrain onto the screen. This function is available for
 * any PDA, it does not contain an ARM core.
 *
 */

void TerrainRenderNorthUp(

	TerrainType t, 			// Terrain cache
	
	Int32 p1lat, Int32 p1lon,	// lat/lon of top left corner of screen
	Int32 p3lat, Int32 p3lon,	// lat/lon of bottom right corner of screen

	void *screen,			// pointer to top left of screen area
	Int32 width, Int32 height	// width & height of screen (pixels/bytes)

	) ADT2_SECTION;
	
/*
 * TerrainRender
 *
 * Renders the terrain from the specified cache onto the screen. Points 1-4
 * define the four corners of the screen, and they may be specified such that
 * the terrain is drawn rotated:
 *
 * P1-------P2
 * |        |
 * |        |
 * |        |
 * P4-------P3
 *
 * The function has a PNOlet core, for extra speed. This means it is
 * limited to running on a PDA with an ARM processor, or the Windoze
 * OS (as a DLL).
 *
 */

void TerrainRender(

	TerrainType t,			// Terrain cache to use

	Int32 p1lat, Int32 p1lon,
	Int32 p2lat, Int32 p2lon,
	Int32 p3lat, Int32 p3lon,
	Int32 p4lat, Int32 p4lon,	// lat/lon of four corners of screen

	void *screen,			    // pointer to top left of screen
	Int32 width, Int32 height,	// width & height of screen (pixels/bytes)
	Int32 rowWidth				// width of one row of screen (bytes)
	
	) ADT2_SECTION;

#endif
