/*
 * TerrainCore.h
 *
 * This file defines the transfer structure used to call from
 * 68k application to the PNO function 'TerrainRenderCore'
 *
 */

#ifndef TERRAIN_CORE_INCLUDED
#define TERRAIN_CORE_INCLUDED

#include "Platform.h"
/*
 * These macros convert to/from 8 bit fixed point precision
 *
 */

#define FP8RADIX 256
#define INTFP8(x) ((x) << 8) 
#define FP8INT(x) ((x) >> 8) 
#define FP8FRAC(x) ((x) & 0xff)


struct TerrainCoreParamType {

	/*
	 * The col_* variables control the outer loop which runs down the screen
	 *
	 */

	Int32 col_src_x;
	Int32 col_src_y;		// source x & y coords as we step down the screen

	Int32 col_src_h;
	Int32 col_src_w;		// width/height of the source rectangle

	Int32 col_inc_x;
	Int32 col_inc_y;		// controls for when to step through x & y down the screen

	Int32 col_dir_x;  
	Int32 col_dir_y;

	/*
	 * The row_* variables control the inner loop which runs across the screen
	 *
	 * The variables here only need to be computed once, the inner loop defines
	 * and computes row_src_x/y and row_inc_x/y
	 *
	 */

	Int32 row_src_w;
	Int32 row_src_h;
	Int32 row_dir_x;
	Int32 row_dir_y;

	Int32 width;
	Int32 height;
	Int32 rowWidth;

	Int32 widthFP8; 
	Int32 heightFP8;
	Int32 widthOverHeightFP8;

	Int32 tc_width;

	UInt8 *palette;
	UInt8 *screen;
	UInt8 *cache;

};

#endif

