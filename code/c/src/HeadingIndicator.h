/*
 * HeadingIndicator.h
 *
 * Header file for the HeadingIndicator
 *
 * A HeadingIndicator is a graphical representation of a heading indicator 
 * gauge that one might find in a light aircraft. It is a circular gauge 
 * with compass markings inside and the top of the gauge is aligned with the 
 * aircraft heading/track. The gauge also overlays a pointer to a waypoint 
 * (similar to an ADF), and an optional label in the centre of the gadget.
 *
 * Initialise a HeadingIndicator by calling HeadingIndicatorInit. 
 *
 */
#ifndef HEADINGINDICATOR_H_INCLUDED
#define HEADINGINDICATOR_H_INCLUDED

#include "Platform.h"
#include "Constants.h"

typedef struct HSITypeStruct *HSIType;

typedef struct HSIMiniPanelTypeStruct *HSIMiniPanelType;

#define HSIStandardRadius 134

/*
 * function : HSINew
 *
 * Initialises a HeadingIndicator gauge. 
 *
 */

extern HSIType HSINew(Coord x, Coord y, Coord radius) HSI_SECTION;

/*
 * function : HeadingIndicatorDraw
 *
 * heading - the heading disc is drawn with this heading at the top
 * bearing - the bearing pointer is drawn to point at this heading
 *
 * Draws the heading indicator at the initialised location, into the
 * current draw window.
 *
 */

extern void HSIDraw(const HSIType hsi, Int16 heading, Int16 bearing, 
			Int16 course, float trkErr, float maxTrackError) HSI_SECTION;


/*
 * function : HSIFree
 *
 * Deallocate data associated with the heading indicator
 */

extern void HSIFree(HSIType hi) HSI_SECTION;

/*
 * HSIMiniPanel
 * 
 */

extern HSIMiniPanelType HSIMiniPanelNew(Coord x, Coord y, Coord width, Coord height) HSI_SECTION;

extern void HSIMiniPanelFree(HSIMiniPanelType hsimp) HSI_SECTION;

extern void HSIMiniPanelDraw(const HSIMiniPanelType hsimp) HSI_SECTION;

#endif
