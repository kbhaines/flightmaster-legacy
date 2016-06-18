/*
 * Colours.c
 *
 */

#include "Platform.h"
#include "GlobalTypes.h"
#include "Colours.h"
#include "Modules.h"

#define ModuleID ColoursModuleID

/******************************************************************************
 *
 * module variables
 *
 */

static const RGBColorType route = { 0, 0,204,0 };
static const RGBColorType track = { 0, 0,220,0 };
static const RGBColorType atz   = { 0, 240,0,240 };
static const RGBColorType label = { 0, 0,0,0 };
static const RGBColorType sky   = { 0, 85,130,224};
static const RGBColorType ground= { 0, 167,97,43 };
static const RGBColorType aaLines     = { 0, 255,255,255 };
static const RGBColorType pointer= { 0, 0, 0, 200 };
static const RGBColorType warning= { 0, 200, 0, 0};

static const RGBColorType satConstOuter = { 0, 100, 100, 200};
static const RGBColorType satConstInner = { 0, 190, 190, 220};

static const RGBColorType green   = { 0, 0, 200, 0 };
static const RGBColorType yellow  = { 0, 249, 160, 7 };
static const RGBColorType red     = { 0, 200, 0, 0 };
static const RGBColorType brightRed     = { 0, 255, 48, 48 };

static const RGBColorType black        = { 0, 0,0,0};
static const RGBColorType white        = { 0, 255,255,255};

static const RGBColorType vsiClimb = { 0, 100, 130, 224 };
static const RGBColorType vsiDescend = {0, 167, 97, 43 };

static const RGBColorType statusBar = {0, 0, 102, 204 };
static const RGBColorType statusBarLight = { 0, 149, 204, 255 };

static const RGBColorType background = { 0, 255,255,255 };
static const RGBColorType dataDisplay = { 0, 0, 0, 0 };

static const RGBColorType airway = { 0, 128, 128, 128 };
static const RGBColorType suas = { 0, 255, 0, 0 };
static const RGBColorType classOther = { 0, 0, 0, 255 };

static const RGBColorType airspace[7] = {

	{ 0, 220, 30, 0 },	// Class A
	{ 0, 0,0,200    },	// Class B
	{ 0, 0,0,200	},  // Class C
	{ 0, 200,0,200	},	// Class D
	{ 0, 200,0,200  },	// Class E
	{ 0, 0,128,255	},	// Class F
	{ 0, 0,128,255	}   // Class G

};

static const RGBColorType airspaceOutline[8] = {

	{ 0, 110, 15, 0 },	// Class A
	{ 0, 0,0,100    },	// Class B
	{ 0, 0,0,100	},  // Class C
	{ 0, 100,0,100	},	// Class D
	{ 0, 100,0,100  },	// Class E
	{ 0, 0,64,128	},	// Class F
	{ 0, 0,64,128	},  // Class G
	{ 0, 128, 0,0 	}	// SUAS

};


static const RGBColorType airfieldIcon = { 0, 200, 0, 200 };
static const RGBColorType otherIcon = { 0, 0,0, 200 };

/*****************************************************************************
 *
 * private functions
 *
 */

static void SetAppColourPreferences(AppColourPrefsType *ap) UTILS_SECTION;

static void SetAppColourPreferences(AppColourPrefsType *ap) {

	Int16 j;

	ap->route = PFPaletteGetIndex( &route );	
	ap->track = PFPaletteGetIndex( &track );	
	ap->atz = PFPaletteGetIndex( &atz );	
	ap->label = PFPaletteGetIndex( &label );	
	ap->sky = PFPaletteGetIndex( &sky );	
	ap->ground = PFPaletteGetIndex( &ground );	
	ap->aaLines = PFPaletteGetIndex( &aaLines );	
	ap->green = PFPaletteGetIndex( &green );	
	ap->yellow = PFPaletteGetIndex( &yellow );	
	ap->red = PFPaletteGetIndex( &red );	
	ap->pointer = PFPaletteGetIndex( &pointer );	
	ap->satConstOuter = PFPaletteGetIndex( &satConstOuter );	
	ap->satConstInner = PFPaletteGetIndex( &satConstInner );	
	ap->warning = PFPaletteGetIndex( &warning );	
	ap->black = PFPaletteGetIndex( &black );	
	ap->white = PFPaletteGetIndex( &white );	
	ap->vsiClimb = PFPaletteGetIndex( &vsiClimb );
	ap->vsiDescend= PFPaletteGetIndex( &vsiDescend );
	ap->statusBar = PFPaletteGetIndex( &statusBar );
	ap->statusBarLight = PFPaletteGetIndex( &statusBarLight );
	ap->background = PFPaletteGetIndex( &background );
	ap->dataDisplay = PFPaletteGetIndex( &dataDisplay );

	for (j=0;j<7;j++) ap->airspace[j] = PFPaletteGetIndex( &airspace[j] );
	for (j=0;j<8;j++) ap->airspaceOutline[j] = PFPaletteGetIndex( &airspaceOutline[j] );

	ap->airway = PFPaletteGetIndex( &airway);
	ap->suas = PFPaletteGetIndex( &suas);
	ap->classOther = PFPaletteGetIndex( &classOther);
	ap->airfieldIcon = PFPaletteGetIndex( &airfieldIcon);
	ap->otherIcon = PFPaletteGetIndex( &otherIcon);

}


/*****************************************************************************
 * 
 * public functions
 *
 */

/*
 * function : ColoursSetDay
 *
 */

void ColoursSetDay(AppColourPrefsType *ap) {
	
	PFPaletteSetDay();
	SetAppColourPreferences(ap);
	
}

/*
 * function : ColoursSetNight
 *
 */

void ColoursSetNight(AppColourPrefsType *ap) {
	
	PFPaletteSetNight();
	SetAppColourPreferences(ap);
	ap->black = PFPaletteGetIndex(&red);
	
}

