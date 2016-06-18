/*
 * TerrainCore.c
 *
 * Provides the hard-work part of the terrain rendering algortithm, adapted so
 * that it can be compiled as a PNOlet for extra speed.
 *
 * DLL for simulator?
 *
 */

#include "Platform.h"
#include <PceNativeCall.h>
#include "Utils.h"
#include "TerrainCore.h"

/*
 * set TARG_PACE in TerrainType.c if you want to run the
 * render function on the Simulator (non-DLL mode)
 *
 */

#ifdef TARG_PACE

#define SWAP16(x) ((x))
#define SWAP32(x) ((x))
unsigned int TerrainRenderCore(const void *emulStateP, void *userData, Call68KFuncType *call68kf) ADT2_SECTION;

#else

#define SWAP16(x) ( (((x) & 0xff00) >> 8) | (((x) & 0x00ff) << 8) )
#define SWAP32(x) ( (((x) & 0xff000000) >> 24) | (((x) & 0x00ff0000) >> 8) \
				|   (((x) & 0x0000ff00) << 8) | (((x) & 0x000000ff) << 24)) 

#endif

/*
 * This function is the core of the TerrainRender function, designed
 * to execute as a PNO.
 *
 * UserData points to the parameter block
 *
 */

unsigned int TerrainRenderCore(const void *emulStateP, void *userData, Call68KFuncType *call68kf) {

	Int32 dest_y;
	UInt8 *pos;

	Int32 rowPadding;
	
	struct TerrainCoreParamType *tcParamIn = userData;
	struct TerrainCoreParamType tcParam;

	/*
	 * transfer the variables from the passed-in block pointed to by
	 * userData into a local copy of the same structure, but byte-swapped
	 * from big-endian to little endian
	 *
	 */

	tcParam.col_src_x = SWAP32(tcParamIn->col_src_x);
	tcParam.col_src_y = SWAP32(tcParamIn->col_src_y);

	tcParam.col_src_h = SWAP32(tcParamIn->col_src_h);
	tcParam.col_src_w = SWAP32(tcParamIn->col_src_w);

	tcParam.col_inc_x = SWAP32(tcParamIn->col_inc_x);
	tcParam.col_inc_y = SWAP32(tcParamIn->col_inc_y);

	tcParam.col_dir_x = SWAP32(tcParamIn->col_dir_x);  
	tcParam.col_dir_y = SWAP32(tcParamIn->col_dir_y);

	tcParam.row_src_w = SWAP32(tcParamIn->row_src_w);
	tcParam.row_src_h = SWAP32(tcParamIn->row_src_h);
	tcParam.row_dir_x = SWAP32(tcParamIn->row_dir_x);
	tcParam.row_dir_y = SWAP32(tcParamIn->row_dir_y);

	tcParam.width = SWAP32(tcParamIn->width);
	tcParam.height = SWAP32(tcParamIn->height);
	tcParam.rowWidth = SWAP32(tcParamIn->rowWidth);

	rowPadding = tcParam.rowWidth - tcParam.width;
	
	tcParam.widthFP8 = SWAP32(tcParamIn->widthFP8); 
	tcParam.heightFP8 = SWAP32(tcParamIn->heightFP8);
	tcParam.widthOverHeightFP8 = SWAP32(tcParamIn->widthOverHeightFP8);

	tcParam.tc_width = SWAP32(tcParamIn->tc_width);

	tcParam.palette = (UInt8*) SWAP32((Int32)tcParamIn->palette);
	tcParam.screen = (UInt8*) SWAP32((Int32)tcParamIn->screen);
	tcParam.cache = (UInt8*) SWAP32((Int32)tcParamIn->cache);

	pos = tcParam.screen;

	dest_y = tcParam.height;
	while (dest_y) {

		Int32 row_src_x = tcParam.col_src_x;
		Int32 row_src_y = tcParam.col_src_y;

		Int32 row_inc_x; 
		Int32 row_inc_y;

		UInt8 terrain_pixel;
		Int32 dest_x;

		/*
		 * initial values for incremental variables are based on the col_*
		 * equivalents in the outer control loop - "how far through the pixel
		 * are we?". Of course, the ratio needs converting e.g.
		 *
		 * 		col_inc_x		row_inc_x
		 * 		---------	=	---------
		 * 		  height		 width
		 *
		 */

		/*
		 * We also invert the increment value if we're stepping in a negative
		 * direction.  But note that the choice to invert row_inc_* here
		 * depends on the previous inversion of the col_inc_* variable - if
		 * row_dir == col_dir then there's no need to invert (as col_inc has
		 * *already* been inverted or left alone, and we should treat row_inc
		 * the same).
		 *
		 * Basically: row_dir == col_dir : both increments moving the same direction
		 * 			  row_dir != col_dir : increments are moving in opposite directions
		 *
		 */

		row_inc_x = FP8INT(tcParam.col_inc_x * tcParam.widthOverHeightFP8);
		if (tcParam.row_dir_x != tcParam.col_dir_x ) row_inc_x = tcParam.widthFP8 - row_inc_x;

		row_inc_y = FP8INT(tcParam.col_inc_y * tcParam.widthOverHeightFP8);
		if (tcParam.row_dir_y != tcParam.col_dir_y ) row_inc_y = tcParam.widthFP8 - row_inc_y;

		terrain_pixel = tcParam.palette[tcParam.cache[FP8INT(row_src_y)*tcParam.tc_width+FP8INT(row_src_x)]];
		dest_x = tcParam.width;
		while (dest_x) {
	
			/* 
			 * inner loop, going across the screen
			 *
			 */

			*pos++ = terrain_pixel;

			row_inc_x += tcParam.row_src_w;
			if (row_inc_x > tcParam.widthFP8) {

				row_inc_x -= tcParam.widthFP8;
				row_src_x += tcParam.row_dir_x;
				terrain_pixel = tcParam.palette[tcParam.cache[FP8INT(row_src_y)*tcParam.tc_width+FP8INT(row_src_x)]];

			}

			row_inc_y += tcParam.row_src_h;
			if (row_inc_y > tcParam.widthFP8) {

				row_inc_y -= tcParam.widthFP8;
				row_src_y += tcParam.row_dir_y;
				terrain_pixel = tcParam.palette[tcParam.cache[FP8INT(row_src_y)*tcParam.tc_width+FP8INT(row_src_x)]];

			}

			dest_x--;
			
		}
		pos += rowPadding;

		tcParam.col_inc_x += tcParam.col_src_w;
		if (tcParam.col_inc_x > tcParam.heightFP8) {

			tcParam.col_inc_x -= tcParam.heightFP8;
			tcParam.col_src_x += tcParam.col_dir_x;

		}

		tcParam.col_inc_y += tcParam.col_src_h;
		if (tcParam.col_inc_y > tcParam.heightFP8) {

			tcParam.col_inc_y -= tcParam.heightFP8;
			tcParam.col_src_y += tcParam.col_dir_y;

		}

		dest_y--;

	}

}
