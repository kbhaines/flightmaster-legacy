/*
 * PDBManager
 *
 * ADT which allows client to create PalmOS Database files (PDB).
 *
 */

/*
 * PalmOS datatypes
 *
 */

typedef unsigned short int	UInt16;
typedef unsigned int 	UInt32;
typedef int 		 	Int32;
typedef short int 		Int16;
typedef unsigned char   Boolean;

enum { false = 0, true = 1};


typedef struct PDBTypeStruct *PDBType;

/*
 * PDBNew
 *
 * Create and initialises a new PDB object
 *
 */

extern PDBType PDBNew(char *name, UInt32 creator, UInt32 type) ;

/*
 * PDBAddRecord
 *
 * Adds the data to the PDB. Data points to the data, size is the size of the
 * data in bytes.
 *
 * Function makes a *copy* of data, so it is safe to de-allocate *data after
 * calling this function.
 *
 */

extern void PDBAddRecord(PDBType pdb, void *data, int size, UInt32 uniqueID) ;

/*
 * PDBInsertRecord
 *
 * Inserts the record at the specified point
 *
 */

extern void PDBInsertRecord(PDBType pdb, void *data, int size, UInt32 uniqueID, UInt16 insertPos);

/*
 * PDBWrite
 *
 * Writes the PDB data to the specified file
 *
 */

extern void PDBWrite(PDBType pdb, const char *filename) ;

/*
 * PDBSetAppInfo
 *
 * ..on the tin etc...
 *
 */

extern void PDBSetAppInfo(PDBType pdb, void *appInfo, int appInfoSize);

/*
 * PDBGetCreateDate
 *
 */

extern UInt32 PDBGetCreateDate(PDBType pdb);

/*
 * PDBFree
 *
 */

extern void PDBFree(PDBType pdb) ;

/*
 * PDBDataPack
 *
 * Reverses the order of the specified data, returning a pointer to it. Handles
 * up to 16 bytes
 *
 */

extern void *PDBDataPack(void *data, int size) ;

/*
 * these helper functions are for converting to Palm-endian'd data and packing
 * into a contiguous space
 *
 * e.g. PDBPack16(ptr,d) stores 'd' in ptr and advances ptr by sizeof(Int16)
 *
 */

#define PDBPalm16(d) *((UInt16*)(PDBDataPack((void*)&d, sizeof(UInt16))))
#define PDBPalm32(d) *((UInt32*)(PDBDataPack((void*)&d, sizeof(UInt32))))
#define PDBPalmFloat(d) *((float*)(PDBDataPack((void*)&d, sizeof(float))))
#define PDBPalmDouble(d) *((double*)(PDBDataPack((void*)&d, sizeof(double))))

#define PDBPack16(ptr,d) *((UInt16*)ptr++) = PDBPalm16(d)
#define PDBPack32(ptr,d) *((UInt32*)ptr++) = PDBPalm32(d)
#define PDBPackFloat(ptr,d) *((float*)ptr++) = PDBPalmFloat(d)
#define PDBPackDouble(ptr,d) *((double*)ptr++) = PDBPalmDouble(d)

#define PDBPackString(ptr,s) strcpy(ptr,s);ptr+=strlen(s)+1;

