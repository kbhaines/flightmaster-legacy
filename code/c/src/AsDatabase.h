/*
 * AsDatabase.h
 *
 * Airspace database module
 *
 */

#ifndef ASDB_INCLUDED
#define ASDB_INCLUDED


#include "Platform.h"
#include <VFSMgr.h>
#include "Constants.h"

typedef UInt16 AirspaceIDType;
#define asNotFound 0

typedef enum {

	altAmsl, altAgl, altFL, altNotam

} AltReferenceType;

/*
 * master class type, select one of these bits per airspace record. However
 * airways may be Low, High or Both
 *
 */


#define asTypeSUAS       0x0010
#define asTypeClassA     0x0020
#define asTypeClassB     0x0040
#define asTypeClassC     0x0080
#define asTypeClassD     0x0100
#define asTypeClassE     0x0200
#define asTypeClassF     0x0400
#define asTypeClassG     0x0800
#define asTypeLowAirway  0x1000
#define asTypeHighAirway 0x2000
#define asTypeOther      0x4000
#define asTypeClassBG   (asTypeClassB | asTypeClassC | asTypeClassD | asTypeClassE | asTypeClassF | asTypeClassG )
#define asTypeAirspace   (asTypeClassA | asTypeClassBG | asTypeSUAS)
#define asTypeAirway     (asTypeLowAirway | asTypeHighAirway)

#define asTypeMask       0xFFF0
#define asSubTypeMask    0x000F

/*
 * subclass types. 'or' these with the master class
 * type to produce AirspaceClassType
 * 
 */

#define suasAlert   0
#define suasDanger  1
#define suasMoa     2
#define suasProhibited 3
#define suasRestricted 4
#define suasTra     5
#define suasWarning 6

typedef UInt16 AirspaceClassType;


/*
 * line segment for airspace boundary
 *
 */

typedef struct {

	/*
	 * end of line (start is end of previous segment) 
	 *
	 */

	Int32 lat, lon;

} LineSegmentType;

/*
 * arc segment for airspace boundary
 *
 */

typedef struct {

	/*
	 * centre of arc 
	 *
	 */

	Int32 lat, lon;

	/*
	 * radius in latitude radians. If -ve then arc is anticlockwise.
	 *
	 * radius = (NM radius / 10800) * 2^31
	 *
	 */

	Int32 radius;

	/*
	 *  start & end angles of arc (radians)
	 *
	 *  if start=end=0 then arc is a circle.  
	 *
	 */

	Int16 start, end;

} ArcSegmentType;



/*
 * airspace record
 *
 */

typedef struct {

	AirspaceClassType type;
	  
	/*
	 * type-specific data
	 *
	 * For airspace, this is the primary frequency of the airspace. High
	 * byte is whole MHz, low byte is decimal part. E.g. 118.75 is stored
	 * as ( 118 << 8) + 75
	 *
	 * For airways, this is the RNP of the airway expressed as NM*10
	 *
	 */

	UInt16 extra;
	
	/*
	 * altitude limits in feet, and reference points
	 *
	 */

	UInt16 lowerAlt, upperAlt;
	AltReferenceType lowerAltRef;
	AltReferenceType upperAltRef;

	/*
	 * segmentCode is a null-terminated string of 'l' (line) or 'a' (arc)
	 * characters, describing the data that follows in the segData part of
	 * the record.
	 *
	 * E.g. 'llalla' means line, line, arc, line, line,arc.
	 *
	 */

	char segmentCode[1];

	/*
	 * textual description of the airspace, lines separated by \n, null
	 * terminated. Line 1 should be the name/description of the airspace.
	 * Lines 2 onwards are freeform. 
	 *
	 * The size of description + segmentCode should be padded to an even
	 * number of bytes (including \0), by appending a '\n' character here
	 * if necessary. DO NOT USE \0 TO PAD!
	 *
	 */

	char description[1];

	/*
	 * rest of record is set of ArcSegmentType and LineSegmentType records.
	 *
	 */

	LineSegmentType segData[1];

} AirspaceType;

extern const char *suasTypes[];

/*
 * function : AsOpenDatabase
 *
 * Does what it says ;-)
 *
 * Returns true if airspace database exists and was opened successfully
 *
 */

extern Boolean AsOpenDatabase(void) ASDB_SECTION;

/*
 * function : AsGetDatabaseInfo
 *
 * Returns the database information block for the main database
 *
 */

extern void *AsGetDatabaseInfo(void) ASDB_SECTION;

/*
 * function : AsSearchDatabase
 *
 * Searches a airspace database within the limits given in 
 * (lat1,lon1) -> (lat2,lon2) where (lat1,lon1) is northwest corner.
 *
 * Retrieves a set of Airspace Record IDs, store in results.
 *
 * Returns the number of records found
 *
 */

extern Int16 AsSearchDatabase(Int16 lat1, Int16 lon1, Int16 lat2, Int16 lon2,
		AirspaceClassType filter, AirspaceIDType *results, UInt16 maxResults) 
		ASDB_SECTION;

/*
 * function : AsGetAirspace
 *
 * Returns a copy of the airspace record corresponding to the given ID. Caller
 * is responsible for freeing the memory.
 *
 */

extern AirspaceType *AsGetAirspace(AirspaceIDType id) ASDB_SECTION;

/*
 * function : AsDecodeAltitude 
 *
 * Returns a string representation of the given reference point/altitude combination in
 * the specified units.
 *
 * altMetres is the altitude encoded into metres
 *
 * Units is 0 for feet, 1 for metres. alt must always be given in feet.
 *
 */

extern void AsDecodeAltitude(/*@out@*/ char *str, /*@out@*/ Int32 *altMetres, UInt16 units, AltReferenceType ref, UInt16 alt) ASDB_SECTION;

/*
 * function : AsCloseDatabase
 *
 * Does what it says ;-)
 *
 */

extern void AsCloseDatabase(void) ASDB_SECTION;

/*
 * function : AsImport
 *
 * Import the specified database (0 = base, 1 = TFRs) from an external card.
 *
 * f is a pointer to a callback function that will get called with the status
 * of the import operation.
 *
 * Returns false if the import failed. In this case, f will have been called
 * with an error message.
 *
 */

extern Boolean AsImport(Int16 dbNumber, void (*f)(const char *)) ASDB_SECTION;

#endif
