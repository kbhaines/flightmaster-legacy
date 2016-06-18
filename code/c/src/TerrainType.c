/*
 * TerrainType
 *
 * Abstract data type for terrain rendering
 *
 *
 * Terrain coordinate system is -16384 -> +16383 to represent -180 to +180
 * degrees but with 8 bits of fixed point precision (23 bits total)
 *
 */


#include "Platform.h"
#include "TerrainType.h"
#include "TerrainCore.h"
#include "GlobalTypes.h"
#include "Utils.h"
#include "Modules.h"

#define ModuleID TerrainModuleID

#define TRUE 1
#define FALSE 0

// 2^31 - 1 
#define MAXLON 2147483647

extern AppColourPrefsType AppColourPrefs;

typedef UInt8 TCElement;

struct TerrainTypeStruct {

	PFFileRef file;					// terrain file

	/* 
	 * Top left and bottom right of terrain file
	 *
	 * Limits of terrain file are in integer terrain coordinate system based on
	 * the high-resolution dataset.
	 *
	 */

	Int32 tf_lat1, tf_lon1;
	Int32 tf_lat3, tf_lon3;		
	
	TCElement *terrain_cache;
	Int32 tc_width, tc_height;	// dimensions of cache

	Int32 tc_scale_factor;		// scaling: 1=hi-res, 2=med, 4=lo
	Int32 t1lat, t1lon;
	Int32 t3lat, t3lon;			//top left and bottom right of terrain in cache

	Int16 alertAltitudeIndex;	// If != -1, this is the index which represents
								// the altitude of the user's aircraft in the
								// palette

	Boolean terrain_invalid;	// set true if the terrain cache is invalid and
								// terrain should not be drawn from it
							
	TerrainPaletteType paletteType;

	UInt8 *sourcePalette;		// user-supplied palette
	UInt8 *displayPalette;		// palette used in the actual display

};


/*
 * sea level is represented by '12'
 *
 */

#define OCEAN 12

/*
 * Converts from 32-bit world coordinate system to terrain coordinate system,
 * which is 23 bits (comprising 15bits integer portion & 8bits fixed point)
 *
 */

#define CONV_WCTC(x) ( x >> 9 )

/*
 * convert from 23bit terrain coords into floating-point (for debugging)
 *
 */

#define TCFLOAT(x) ((float)(180.0*(float)(x) / 4194304.0))

/*******************************************************************************
 *
 * Private functions
 *
 */


#define ALT_TO_IDX(x) (((x) + 1500) / 125)

/*
 * function : terrain_cache_check
 *
 * Checks the contents of the terrain cache against the requested rectangle
 * which is in Terrain Coordinate system
 *
 */

static void terrain_cache_check(TerrainType t, Int32 p1lat, Int32 p1lon, Int32 p3lat, Int32 p3lon) ADT2_SECTION;

static void terrain_cache_check(TerrainType t, Int32 p1lat, Int32 p1lon, Int32 p3lat, Int32 p3lon) {

	Int32 file_width = (t->tf_lon3 - t->tf_lon1);
	Int32 file_height = (t->tf_lat1 - t->tf_lat3);

	Int32 mid_lat, mid_lon;
	Int32 req_height, req_width;
	Int16 force = FALSE;

	Int32 filerow, filecol;
	Int32  bytes_to_load, cache_col;
	Int32  rows_to_load, cache_row;
	Int32  master_offset;

	LOGENTRY;

	/*
	 * the cache is always loaded with data centred on the middle of the
	 * requested area.
	 *
	 */

	req_height = p1lat - p3lat;
	if (p1lon > p3lon) {

		/*
		 * wraparound 180 E/W
		 *
		 */

		p3lon = CONV_WCTC(2147483647);

	}
	req_width = p3lon - p1lon;

	mid_lat = p1lat - req_height/2;
	mid_lon = p1lon + req_width/2;

	LOGTAG("Cache request for:");
	LOGFLOAT(TCFLOAT(p1lat));
	LOGFLOAT(TCFLOAT(p1lon));
	LOGFLOAT(TCFLOAT(p3lat));
	LOGFLOAT(TCFLOAT(p3lon));
	LOGINT32(FP8INT(req_height));
	LOGINT32(FP8INT(req_width));

	LOGTAG("Current Cache Contents:");
	LOGFLOAT(TCFLOAT(t->t1lat));
	LOGFLOAT(TCFLOAT(t->t1lon));
	LOGFLOAT(TCFLOAT(t->t3lat));
	LOGFLOAT(TCFLOAT(t->t3lon));

	LOGINT16(force);

	/*
	 * When we've got one of the low-resolution terrain maps in the cache, then
	 * it's possible that the new terrain request would look better if the
	 * cache was reloaded with a high-resolution version.
	 * 
	 */

	if ((t->tc_scale_factor == 2 && FP8INT(req_height) < t->tc_height &&
			FP8INT(req_width) < t->tc_width) ||
		(t->tc_scale_factor == 4 && FP8INT(req_height) < t->tc_height*2 &&
			FP8INT(req_width) < t->tc_width*2)
			) {

		/*
		 * reload due to zooming into smaller area, need
		 * to access the higher-detail map
		 *
		 */

		LOGTAG("Zooming, forcing miss");
		force  = TRUE;

	}

	if (!t->terrain_invalid && !force && p1lat < t->t1lat && p3lat > t->t3lat && 
			p1lon > t->t1lon && p3lon < t->t3lon) {

		/*
		 * cache hit!
		 *
		 */

		LOGTAG("CACHE HIT!");
		LOGEXIT;

		return;

	}

	//printf("MISS!\n");
	
	/*
	 * no cache hit, get ready to load a new cache from disk
	 *
	 * fill the cache with ocean
	 *
	 */

	PFMemSet(t->terrain_cache, t->tc_width*t->tc_height*sizeof(TCElement), OCEAN);

	/*
	 * validate the request, to see if we can load a map which fits into the
	 * terrain cache limits. 
	 *
	 * We check the terrain caches in order, starting with the high-resolution
	 * dataset.
	 *
	 * In all cases of satisfiable requests, we adjust p1 to the origin of the
	 * terrain cache that we're about to load.
	 *
	 */

	if (FP8INT(req_height) > t->tc_height || FP8INT(req_width) > t->tc_width) {

		/*
		 * high-resolution won't fit, try one of the others
		 *
		 */

		LOGTAG("Checking alternate map");

		if (FP8INT(req_height) > t->tc_height*2 || FP8INT(req_width) > t->tc_width*2) {

			if (FP8INT(req_height) > t->tc_height*4 || FP8INT(req_width) > t->tc_width*4) {

				/*
				 * we're screwed, the cache can't hold the requested terrain
				 *
				 */

				//char err[64];

				///StrPrintF(err, "Cache request too big (%d, %d)", req_width, req_height);
				//ErrFatalDisplay(err);
				
				t->terrain_invalid = true;
				LOGEXIT;
				return;

			}

			/*
			 * low-resolution (1/4 scale), we can hold a lot of the planet here!
			 *
			 */

			LOGTAG("1/4 map");
			p1lat = mid_lat + INTFP8(t->tc_height*2);
			p1lon = (mid_lon - INTFP8(t->tc_width*2));
			t->tc_scale_factor = 4;

			/*
			 * skip over hi-res + 1/2 res datasets
			 *
			 */

			master_offset = 5*((file_height * file_width)/4);

		} else {

			/*
			 * medium-resolution (1/2 scale map), we can hold more of the
			 * planet than full scale
			 *
			 */

			LOGTAG("1/2 map");
			p1lat = mid_lat + INTFP8(t->tc_height);
			p1lon = (mid_lon - INTFP8(t->tc_width));
			t->tc_scale_factor = 2;

			/*
			 * skip over hi-res dataset
			 *
			 */

			master_offset = file_height * file_width;

		}

	} else {

		LOGTAG("1/1 Map");
		p1lat = mid_lat + INTFP8(t->tc_height/2);
		p1lon = (mid_lon - INTFP8(t->tc_width/2));
		t->tc_scale_factor = 1;

		master_offset = 0;

	}

	/*
	 * skip over the file header
	 *
	 */

 	master_offset += sizeof(t->tf_lat1)*4;

	filerow = (t->tf_lat1 - FP8INT(p1lat));
	filecol = FP8INT(p1lon) - t->tf_lon1;

	//LOGINT32(master_offset);
	//LOGINT32(filerow);
	//LOGINT32(filecol);
	//LOGINT32(t->tc_scale_factor);

	/*
	 * lower resolutions mean we need to scale down the row/column
	 *
	 */

	filerow /= t->tc_scale_factor;
	filecol /= t->tc_scale_factor;
	file_height /= t->tc_scale_factor;
	file_width /= t->tc_scale_factor;

	/*
	 * check for requests left or right of the dataset contents, shorten
	 * the row load requests accordingly.
	 *
	 */

	cache_col = 0;
	bytes_to_load = t->tc_width;
	if (filecol < 0) {

		/*
		 * left of dataset edge, load fewer bytes from dataset and load into an
		 * offset in the terrain cache.
		 *
		 */

		bytes_to_load = t->tc_width + filecol;
		cache_col= -filecol;
		filecol = 0;

	} else if ( filecol + bytes_to_load > file_width) {

		/*
		 * right of dataset edge, load fewer bytes only up to the edge of the dataset
		 *
		 */

		bytes_to_load = MAX(file_width - filecol,0);

	}

	/*
	 * check for requests above or below the dataset, shorten
	 * the load requests accordingly
	 *
	 */

	cache_row = 0;
	rows_to_load = t->tc_height;
	if (filerow < 0) {

		/*
		 * top of dataset, load fewer rows from dataset and start loading lower in
		 * the cache
		 *
		 */
		
		cache_row = -filerow;
		filerow = 0;

	} else if (filerow + rows_to_load > file_height) {

		/*
		 * bottom of dataset, load fewer rows
		 *
		 */

		rows_to_load = MAX(file_height - filerow,0);

	}

	LOGINT32(cache_row);
	LOGINT32(cache_col);
	LOGINT32(filerow);
	LOGINT32(filecol);
	LOGINT32(rows_to_load);
	LOGINT32(bytes_to_load);

	/*
	 * simple approach - go from top (north) of terrain cache to the bottom,
	 * and load one row at a time.
	 *
	 */

	if (bytes_to_load > 0 && rows_to_load > 0) {

		LOGTIMESTART;

		while (cache_row < t->tc_height && filerow < file_height) {

			Int32 bytes_read;

			//if (cache_row==0) printf("row, col, w: %d %d %d\n", filerow, filecol, file_width);

			PFSeekFile(t->file, pfSeekStart, master_offset + filerow*file_width+filecol);

			bytes_read = PFReadFile(t->file, &t->terrain_cache[cache_row*t->tc_width+cache_col], bytes_to_load*sizeof(char));

			if (bytes_read != bytes_to_load) {
				
				//printf("Read error (tried %d)\n", bytes_to_load);
				ModErrThrow();

			}

			filerow++;
			cache_row++;

		}

		LOGTAG("Cache timing");
		LOGTIMESTOP;

	}

	/*
	 * set up the corner reference variables of the cache. In the case of the
	 * t1-corner, this needs to be rounded up to the next row above.
	 *
	 */

	t->t1lon = INTFP8(FP8INT(p1lon));
	if (t->tc_scale_factor > 1) {

		t->t1lat = INTFP8(FP8INT(p1lat) + (t->tc_scale_factor == 4 ? 3:2));
		t->t3lat = t->t1lat - INTFP8(t->tc_height * t->tc_scale_factor);
		t->t3lon = t->t1lon + INTFP8(t->tc_width * t->tc_scale_factor);

	} else {
			
		t->t1lat = INTFP8(FP8INT(p1lat)+1);
		t->t3lat = t->t1lat - INTFP8(t->tc_height);
		t->t3lon = t->t1lon + INTFP8(t->tc_width);

	}

	t->terrain_invalid = false;

	LOGTAG("Loaded cache with:");
	LOGFLOAT(TCFLOAT(t->t1lat));
	LOGFLOAT(TCFLOAT(t->t1lon));
	LOGFLOAT(TCFLOAT(t->t3lat));
	LOGFLOAT(TCFLOAT(t->t3lon));

	LOGEXIT;

}

/*******************************************************************************
 *
 * Public functions
 *
 */

/*
 * TerrainNew
 *
 * Create a new terrain object
 *
 */

TerrainType TerrainNew(
		
	const char *filename, 		//	name of terrain file on disk

	Int32 cache_width,
	Int32 cache_height			// width & height of internal cache to use

) {

#define NUMCOLS 18

	RGBColorType cols[NUMCOLS] = {
		{0, 10, 40, 160 },// water
		{0, 0,102,0},
		{0, 0,131,0},
		{0, 0,148,0},
		{0, 0,160,0},
		{0, 0,173,0},
		{0, 12,175,2},
		{0, 44,163,13},
		{0, 69,156,24},
		{0, 105,145,37},
		{0, 134,134,49},
		{0, 156,141,69},
		{0, 174,160,95},
		{0, 189,177,115},
		{0, 189,181,123},
		{0, 198,183,129},
		{0, 206,193,140},
		{0, 0,0,0}
	};

	UInt8 *pal = PFMalloc(256);

	TerrainType t = PFMalloc(sizeof(struct TerrainTypeStruct));

	LOGENTRY;

	ModErrThrowIf(!pal);
	ModErrThrowIf(!t);

	/*
	 * check for prescence of terrain file
	 *
	 */

	if (!filename || !(t->file = PFOpenFile(filename, pfFileReadOnly)) || !(PFFileSize(t->file) > 1024)) {
			
		PFMemFree(t);
		PFMemFree(pal);
		return NULL;

	}

	PFPaletteSet(230, 16, &cols[1]);
		
	t->paletteType   = terrainSys;
	t->sourcePalette = pal;
	t->displayPalette = PFMalloc(PFMallocSize(pal));
	ModErrThrowIf(!t->displayPalette);

	/*
	 * set dimensions of Terrain file
	 *
	 */

	t->tf_lat1 = 8191;
	t->tf_lon1 = -16384;
	t->tf_lat3 = -8192;
	t->tf_lon3 = 16383;

	t->t1lat = t->t3lat = t->t1lon = t->t3lon = 0;
	t->terrain_invalid = true;

	PFReadFile(t->file, &t->tf_lat1,sizeof(t->tf_lat1));
	PFReadFile(t->file, &t->tf_lon1, sizeof(t->tf_lon1));
	PFReadFile(t->file, &t->tf_lat3, sizeof(t->tf_lat3));
	PFReadFile(t->file, &t->tf_lon3, sizeof(t->tf_lon3));

	LOGINT32(t->tf_lat1);
	LOGINT32(t->tf_lon1);
	LOGINT32(t->tf_lat3);
	LOGINT32(t->tf_lon3);

	t->tc_width = cache_width;
	t->tc_height = cache_height;

	t->tc_scale_factor = 1;

	t->terrain_cache = (TCElement*) PFMalloc(cache_width * cache_height * sizeof(TCElement));
	ModErrThrowIf(!t->terrain_cache);

	TerrainSetPalette(t, terrainWarn, NULL, false);

	LOGEXIT;

	return t;
}

/*
 * TerrainFree
 *
 * Delete terrain object
 *
 */

void TerrainFree(TerrainType t) {

	LOGENTRY;
	PFCloseFile(t->file);

	LOGLINE;
	PFMemFree(t->terrain_cache);
	LOGLINE;
	PFMemFree(t->displayPalette);
	PFMemFree(t->sourcePalette);
	PFMemFree(t);

	LOGEXIT;

}


/*
 * TerrainSetPaletteEntry
 *
 * Set a palette entry for the terrain
 *
 */

void TerrainSetPaletteEntry(TerrainType t, Int32 altFrom, Int32 altTo, UInt8 r, UInt8 g, UInt8 b) {

	RGBColorType rgb;
	Int16 idx = MAX(0,ALT_TO_IDX(altFrom));
	Int16 to = MIN(240, ALT_TO_IDX(altTo));
	IndexedColorType c;

	ModErrThrowIf(!t);

	rgb.r = r; rgb.b = b; rgb.g = g;
	
	c = PFPaletteGetIndex(&rgb);

	while (idx <= to) t->sourcePalette[idx++] = c;

}

/*
 * TerrainLoadPalette
 *
 */

void TerrainLoadPalette(TerrainType t, PFFileRef f, Boolean nightMode) {

	char line[256];
	Int16 paletteIndex = 255-26;
	Int16 colourIndex = 0;
	RGBColorType rgbPalette[30];
	RGBColorType *rgb = rgbPalette;
	
	ModErrThrowIf(!t);

	LOGTAG("Reading palette");

	while (paletteIndex < 255 && PFReadLine(f, line)) {

		Int16 alt;
		char *p = line;

		LOGSTR(line);

		if (line[0] == '#') continue;

		alt = StrAToI(p);
		LOGINT16(alt);
		SKIP_FIELD(p);
		ModErrThrowIf(!p);

		rgb->r = StrAToI(p);
		SKIP_FIELD(p);
		ModErrThrowIf(!p);

		rgb->g = StrAToI(p);
		SKIP_FIELD(p);
		ModErrThrowIf(!p);

		rgb->b = StrAToI(p);
		SKIP_FIELD(p);
		ModErrThrowIf(!p);
			
		if (nightMode) {

			rgb->r = rgb->r * 2 / 3;
			rgb->g = rgb->g * 2 / 3;
			rgb->b = rgb->b * 2 / 3;

		}

		alt = ALT_TO_IDX(alt);

		while (colourIndex <= alt) {

			t->sourcePalette[colourIndex++] = paletteIndex;

		}

		rgb++;
		paletteIndex++;

	}

	PFPaletteSet(255-26, 26, rgbPalette);

}

/*
 * TerrainSetAlertAlt
 *
 */

void TerrainSetAlertAlt(TerrainType t, Int32 altInFeet) {

	Int16 newIndex = ALT_TO_IDX(altInFeet);

	Int16 j;
	IndexedColorType level[4];

	/*
	 * NB these are *daylight* levels, getting darker as the 
	 * ground gets higher
	 *
	 */

	const RGBColorType levelRGB[4] =  {
		{0, 255, 255, 0},
		{0, 255, 0, 255},  
		{0, 255, 0, 0},  
		{0, 255, 0, 0}
	
	};

	ModErrThrowIf(!t);

	if (t->paletteType == terrainNormal || t->paletteType == terrainOff) {

		return;

	}

	/*
	 * work out if the new altitude requires a change in the palette,
	 * if it doesn't then don't do anything
	 *
	 */

	if (newIndex == t->alertAltitudeIndex) return;
		
	for (j = 0 ; j < 4;j++) level[j] = PFPaletteGetIndex(&levelRGB[j]);

	if (newIndex < 0) ErrThrow(1234);

	PFMemMove(t->displayPalette, t->sourcePalette, PFMallocSize(t->sourcePalette));
	
	/*
	 * Bands are:
	 *
	 *  +250	 		level 3
	 *  -249 to 249		level 2
	 *  -499 to -250	level 1
	 *  -1000 to -500	level 0
	 *
	 */

	for (j=newIndex+2; j < 256; j++) t->displayPalette[j] = level[3];
	for (j=MAX(0,newIndex-2); j<newIndex+2 && j < 256; j++) t->displayPalette[j] = level[2];

	for (j=3; j< 8;j++) {

		Int16 idx = newIndex - j;
		
		if (idx > -1 && idx != OCEAN && j < 4) 
			
			t->displayPalette[idx] = level[1];

		else if (idx > -1 && idx != OCEAN ) t->displayPalette[idx] = level[0];

	}

	t->alertAltitudeIndex = newIndex;

}

/*
 * TerrainSetPalette
 *
 */

void TerrainSetPalette(TerrainType t, TerrainPaletteType type, const char *fileName, Boolean nightMode) {

	PFFileRef f;
	Int16 j;
	Int16 k;

	LOGENTRY;

	ModErrThrowIf(!t);
	ModErrThrowIf(type == terrainSys);

	if (t->paletteType != type) {

		TerrainSetPaletteEntry(t, 0, 0, 10, 40, 160);

		switch (type) {

		case terrainNormal:
		case terrainNormalWarn:

			/*
			 * check for palette file, load if available
			 *
			 */

			if (fileName && (f = PFOpenFile(fileName, pfFileReadOnly))) {

				TerrainLoadPalette(t, f, nightMode);
				PFCloseFile(f);

			} else {

				k = 255;
				for (j = OCEAN + 1; j < 250; j++) {

					RGBColorType rgb;

					rgb.r = rgb.g = rgb.b = k--;
					
					t->sourcePalette[j] = PFPaletteGetIndex(&rgb);

				}

			}
			break;

		case terrainOff:
		case terrainWarn:
			for (j=0; j < 256; j++) {

				if (j != OCEAN) t->sourcePalette[j] = t->sourcePalette[OCEAN+1];

			}
			break;

		default:
			ModErrThrow();
			break;

		}

		PFMemMove(t->displayPalette, t->sourcePalette, PFMallocSize(t->sourcePalette));
		t->paletteType = type;
		t->alertAltitudeIndex = -1;

	}

	LOGEXIT;

}


/*
 * TerrainRenderNorthUp
 *
 * Render the specified terrain onto the screen
 *
 */

void TerrainRenderNorthUp(

		TerrainType t, 			// Terrain cache
		
		Int32 p1lat, Int32 p1lon,		// lat/lon of top left corner of screen
		Int32 p3lat, Int32 p3lon,		// lat/lon of bottom right corner of screen

		void *screen,			// pointer to top left of screen area
		Int32 width, Int32 height	// width & height of screen (pixels/bytes)

		) {

	UInt8 *pos;

	Int32 src_y;
	Int32 src_h;
	Int32 inc_y;
	Int32 dir_y  = INTFP8(1);
	Int32 dest_y = 0;

	TCElement terrain_pixel;

	Int32 widthFP8 = INTFP8(width);
	Int32 heightFP8 = INTFP8(height);

	LOGENTRY;

	/*
	 * convert World Coords into Terrain Coords
	 *
	 */
	
	p1lat = CONV_WCTC(p1lat);
	p1lon = CONV_WCTC(p1lon);
	p3lat = CONV_WCTC(p3lat);
	p3lon = CONV_WCTC(p3lon);

	terrain_cache_check(t, p1lat, p1lon, p3lat, p3lon);

	if (t->terrain_invalid) {

		LOGEXIT;
		return;

	}

	src_y = (t->t1lat - p1lat) / t->tc_scale_factor;
	src_h = (p1lat - p3lat) / t->tc_scale_factor;

	/*
	 * initialise the increment control variable so that it
	 * represents a position some fraction along the source pixel
	 *
	 */

	inc_y = (FP8FRAC(src_y) * heightFP8) / FP8RADIX;

	if (src_h < 0) {

		src_h = -src_h;
		inc_y = heightFP8 - inc_y;
		dir_y = -dir_y;

	}

	LOGTIMESTART;

	pos = (UInt8 *) screen;

	while (dest_y < height) {

		Int32 src_x;
		Int32 src_w;
		Int32 inc_x;
		Int32 dest_x = 0;
		Int32 dir_x  = INTFP8(1);

		/* 
	 	 * setup inner X loop
		 *
		 */

		src_x = (p1lon - t->t1lon) / t->tc_scale_factor;
		src_w = (p3lon - p1lon) / t->tc_scale_factor;
		
		inc_x = (FP8FRAC(src_x) * widthFP8) / FP8RADIX;
		
		if (src_w < 0) {

			src_w = -src_w;
			inc_x = widthFP8 - inc_x;
			dir_x = -dir_x;

		}

		terrain_pixel = t->displayPalette[t->terrain_cache[FP8INT(src_y)*t->tc_width+FP8INT(src_x)]];
		while (dest_x < width) {
	
			*pos++ = terrain_pixel;

			inc_x += src_w;
			if (inc_x > widthFP8) {

				inc_x -= widthFP8;
				src_x += dir_x;
				terrain_pixel = t->displayPalette[t->terrain_cache[FP8INT(src_y)*t->tc_width+FP8INT(src_x)]];

			}
			dest_x ++;
			
		}

		inc_y += src_h;
		if (inc_y > heightFP8) {

			inc_y -= heightFP8;
			src_y += dir_y;

			if (FP8INT(src_y)>255) src_y = INTFP8(255);

		}

		dest_y ++;

	}

	LOGTIMESTOP;

	LOGEXIT;

}

/*
 * TerrainRender
 *
 * Renders the terrain from the specified cache onto the screen. Points 1-4
 * define the four corners of the screen, and they may be specified such that
 * the terrain is drawn rotated.
 *
 * Set TARG_PACE to run the function in PACE mode
 *
 * (NB can also be set by "makepp PACE=y")
 */


//#define TARG_PACE 1

#ifdef TARG_PACE
#include "TerrainCore.c"
#endif

struct TerrainCoreParamType tcParam;

void TerrainRender(

	TerrainType t,			// Terrain cache to use

	Int32 p1lat, Int32 p1lon,
	Int32 p2lat, Int32 p2lon,
	Int32 p3lat, Int32 p3lon,
	Int32 p4lat, Int32 p4lon,	// lat/lon of four corners of screen

	void *screen,				// pointer to top left of screen
	Int32 width, Int32 height,	// width & height of screen (pixels/bytes)
	Int32 rowWidth				// width of entire screen row
	) {
	
	Int32 maxlat, minlat, maxlon, minlon;

	LOGENTRY;

	ModErrThrowIf(!t);

	tcParam.width = width;
	tcParam.height = height;
	tcParam.rowWidth = rowWidth;
	tcParam.screen = screen;
	tcParam.palette= t->displayPalette;
	tcParam.cache = t->terrain_cache;
	tcParam.tc_width = t->tc_width;

	tcParam.widthFP8 = INTFP8(width);
	tcParam.heightFP8 = INTFP8(height);
	tcParam.widthOverHeightFP8 = tcParam.widthFP8 / tcParam.height;

	tcParam.col_dir_x  = INTFP8(1);
	tcParam.col_dir_y  = INTFP8(1);
	tcParam.row_dir_x  = INTFP8(1);
	tcParam.row_dir_y  = INTFP8(1);

	LOGINT32(p1lat); LOGINT32(p1lon);
	LOGINT32(p2lat); LOGINT32(p2lon);
	LOGINT32(p3lat); LOGINT32(p3lon);
	LOGINT32(p4lat); LOGINT32(p4lon);

	/*
	 * convert World Coords into Terrain Coords within the terrain cache
	 * limits.
	 *
	 */
	
	p1lat = CONV_WCTC(p1lat);
	p1lon = CONV_WCTC(p1lon);
	p2lat = CONV_WCTC(p2lat);
	p2lon = CONV_WCTC(p2lon); 
	p3lat = CONV_WCTC(p3lat);
	p3lon = CONV_WCTC(p3lon);
	p4lat = CONV_WCTC(p4lat);
	p4lon = CONV_WCTC(p4lon);

	/*
	 * determine the bounding rectangle around this area
	 *
	 */

	maxlat = p1lat > p2lat ? p1lat : p2lat;
	maxlat = p3lat > maxlat ? p3lat : maxlat;
	maxlat = p4lat > maxlat ? p4lat : maxlat;

	minlat = p1lat < p2lat ? p1lat : p2lat;
	minlat = p3lat < minlat ? p3lat : minlat;
	minlat = p4lat < minlat ? p4lat : minlat;

	maxlon = p1lon > p2lon ? p1lon : p2lon;
	maxlon = p3lon > maxlon ? p3lon : maxlon;
	maxlon = p4lon > maxlon ? p4lon : maxlon;

	minlon = p1lon < p2lon ? p1lon : p2lon;
	minlon = p3lon < minlon ? p3lon : minlon;
	minlon = p4lon < minlon ? p4lon : minlon;

	LOGFLOAT(TCFLOAT(maxlat)); 
	LOGFLOAT(TCFLOAT(minlon));
	LOGFLOAT(TCFLOAT(minlat)); 
	LOGFLOAT(TCFLOAT(maxlon));

	terrain_cache_check(t, maxlat, minlon, minlat, maxlon);

	if (t->terrain_invalid) {

		LOGEXIT;
		return;

	}

	/*
	 * Set up column-based constants. These run down the screen from P1 to P4,
	 * but P2 to P3 gives the same gradient and width/height differences.
	 *
	 * The col_inc_* control variables are initialised to represent a position
	 * some fraction along the source pixel. If we're stepping backwards then
	 * the fraction needs to be inverted to represent the fact we're stepping
	 * in the opposite direction:
	 *
	 * Box = one source pixel, '.' = fractional position within pixel:
	 *
	 * 	+-------+
	 * 	|		|
	 * 	|-. +ve |
	 * 	|		|
	 * 	+-------+
	 *
	 * So in stepping -ve through the source pixels, we need to invert the +ve fraction
	 * we obtained ( 1 - f ).
	 *
	 * Important to note that the increments are based on screen *height* for
	 * both x and y - because these variables control x/y source pixels along
	 * the y-axis.
	 *
	 */

	tcParam.col_src_y = (t->t1lat - p1lat) / t->tc_scale_factor;
	tcParam.col_src_h = (p2lat - p3lat) / t->tc_scale_factor;

	tcParam.col_inc_y = (FP8FRAC(tcParam.col_src_y) * tcParam.heightFP8) / FP8RADIX;
	if (tcParam.col_src_h < 0) {

		tcParam.col_src_h = -tcParam.col_src_h;
		tcParam.col_inc_y = tcParam.heightFP8 - tcParam.col_inc_y;
		tcParam.col_dir_y = INTFP8(-1);

	}

	tcParam.col_src_x = (p1lon - t->t1lon) / t->tc_scale_factor;
	tcParam.col_src_w = (p3lon - p2lon) / t->tc_scale_factor;

	tcParam.col_inc_x = (FP8FRAC(tcParam.col_src_x) * tcParam.heightFP8) / FP8RADIX;
	if (tcParam.col_src_w < 0) {

		tcParam.col_src_w = -tcParam.col_src_w;
		tcParam.col_inc_x = tcParam.heightFP8 - tcParam.col_inc_x;
		tcParam.col_dir_x = INTFP8(-1);

	}

	/*
	 * set up row-based constants. These run across the screen, from P1 to P2.
	 *
	 * We don't have to set up as many constants as for col_* variables,
	 * because values for row_src_* and row_inc_* are set within the inner
	 * control loop (using the corresponding values from the outer control
	 * loop).
	 *
	 */

	tcParam.row_src_h = (p1lat - p2lat) / t->tc_scale_factor;
	if (tcParam.row_src_h < 0) {

		tcParam.row_src_h = -tcParam.row_src_h;
		tcParam.row_dir_y = INTFP8(-1);

	}

	tcParam.row_src_w = (p2lon - p1lon) / t->tc_scale_factor;
	if (tcParam.row_src_w < 0) {

		tcParam.row_src_w = -tcParam.row_src_w;
		tcParam.row_dir_x = INTFP8(-1);

	}

	LOGTIMESTART;
	
#ifdef TARG_PACE
	TerrainRenderCore(NULL, &tcParam, NULL);
#else
	PceNativeResourceCall('Tarm', 9999, "", &tcParam);
#endif
	
	LOGTIMESTOP;

	LOGEXIT;

}

