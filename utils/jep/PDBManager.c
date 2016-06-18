/*
 * PDBManager
 *
 * Implementation of a PDB management ADT, allowing a client
 * program to write PDB files (PalmOS Data Base)
 *
 */

#include <stdio.h>
#include <time.h>
#include "PDBManager.h"

/*
 * PDB
 *
 */

#define MAXRECORDS 65500

struct PDBTypeStruct {

	char name[32];
	UInt16 numRecords;
	UInt32 creator;
	UInt32 type;
	UInt32 createDate;
	void *appInfo;
	int   appInfoSize;

	int   recSize[MAXRECORDS];
	UInt32 uniqueID[MAXRECORDS];
	void *record[MAXRECORDS];

};


/*
 * structures for creating the PDB file
 *
 */

#define dmDBNameLength    32 /* 31 chars + 1 null terminator */
struct PDBHeader { 

	char	 name[ dmDBNameLength ];
	UInt16	 attributes;
	UInt16	 version;
	UInt32	 create_time;
	UInt32	 modify_time;
	UInt32	 backup_time;
	UInt32	 modificationNumber;

	UInt32	 appInfoID;
	UInt32	 sortInfoID;
	UInt32   type;
	UInt32	 creator;

	UInt32	 id_seed;
	UInt32	 nextRecordList;
	UInt16	 numRecords;

	/*
	 * 'attribute packed' is required on 32 bit platforms, otherwise structure
	 * is 80 bytes when it should be 78. This won't port to C++, but
	 * will be OK for now
	 *
	 */

} __attribute__ ((packed));

/*
 * each record in a PDB has one of these entries
 *
 */

struct PDBRecHeader {   /* 8 bytes total	*/

	UInt32	 offset;	// from start of file
	//struct {
	 //int delete    : 1;
	 //int dirty     : 1;
	 //int busy      : 1;
	 //int secret    : 1;
	 //int category  : 4;
	//}	 attributes;

	char     attributes;
	char	 uniqueID[3];

} __attribute__ ((packed));

/*******************************************************************************
 *
 * Public Functions
 *
 */

/*
 * PDBNew
 *
 * Create and initialises a new PDB object
 *
 */

PDBType PDBNew(char *name, UInt32 creator, UInt32 type) {

	PDBType pdb = malloc(sizeof(struct PDBTypeStruct));
	time_t secs;

	strncpy(pdb->name, name,sizeof(pdb->name));

	pdb->creator = creator;
	pdb->type = type;
	pdb->appInfo = 0;
	pdb->appInfoSize = 0;

	pdb->numRecords = 0;

	secs = time(NULL);

	pdb->createDate = (UInt32)(secs+2082844800);

	return pdb;

}

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

void PDBAddRecord(PDBType pdb, void *data, int size, UInt32 uniqueID) {

	int recnum = pdb->numRecords++;

	if (recnum >= MAXRECORDS) {

		printf("MAXRECORDS exceeded\n");
		exit(1);

	}

	pdb->record[recnum] = malloc(size);

	memcpy(pdb->record[recnum], data,size);

	pdb->recSize[recnum] = size;
	pdb->uniqueID[recnum] = uniqueID;

}

/*
 * PDBInsertRecord
 *
 */

void PDBInsertRecord(PDBType pdb, void *data, int size, UInt32 uniqueID, UInt16 insertPos) {

	int j;

	if (insertPos > pdb->numRecords) 
		insertPos = pdb->numRecords;

	for (j=pdb->numRecords; j > insertPos; j--) {

		pdb->recSize[j] = pdb->recSize[j-1];
		pdb->uniqueID[j] = pdb->uniqueID[j-1];
		pdb->record[j] = pdb->record[j-1];

	}

	pdb->record[insertPos] = malloc(size);

	memcpy(pdb->record[insertPos], data,size);

	pdb->recSize[insertPos] = size;
	pdb->uniqueID[insertPos] = uniqueID;

	pdb->numRecords++;

}

/*
 * PDBSetAppInfo
 *
 * ..on the tin etc...
 *
 */

void PDBSetAppInfo(PDBType pdb, void *appInfo, int appInfoSize) {

	if (pdb->appInfo) free(pdb->appInfo);

	pdb->appInfo = malloc(appInfoSize);
	pdb->appInfoSize = appInfoSize;

	memcpy(pdb->appInfo, appInfo, appInfoSize);

}


/*
 * PDBWrite
 *
 * Writes the PDB data to the specified file
 *
 */

void PDBWrite(PDBType pdb, const char *filename) {

	struct PDBHeader hdr;
	FILE *of;
	int j;
	UInt32 bytesFromStart;

	const Int16 attrs = 8;
	const Int16 version=4;
	UInt32 appInfoID = 0;

	if (pdb->appInfo) {

		 appInfoID = sizeof(hdr) + 2 + pdb->numRecords * sizeof(struct PDBRecHeader); 

	}

	strncpy(hdr.name, pdb->name, sizeof(hdr.name));
	hdr.attributes = PDBPalm16(attrs);
	hdr.version    = PDBPalm16(version);
	hdr.create_time= PDBPalm32(pdb->createDate);
	hdr.modify_time= hdr.create_time;
	hdr.backup_time= hdr.create_time;
	hdr.modificationNumber = 0;

	hdr.appInfoID  = PDBPalm32(appInfoID);
	hdr.sortInfoID = 0;

	hdr.type = PDBPalm32(pdb->type);
	hdr.creator = PDBPalm32(pdb->creator);

	hdr.id_seed = 0;
	hdr.nextRecordList = 0;
	hdr.numRecords = PDBPalm16(pdb->numRecords);

	of = fopen(filename,"wb");
	
	/*
	 * output header
	 *
	 */

	fwrite(&hdr, sizeof(hdr), 1, of);

	/*
	 * output record header block
	 *
	 * (NB Add app-info block size to this later)
	 *
	 */

	bytesFromStart = sizeof(hdr) + 2 + pdb->appInfoSize + 
		pdb->numRecords * sizeof(struct PDBRecHeader);

	for (j=0; j < pdb->numRecords; j++) {

		struct PDBRecHeader rh;

		rh.offset = PDBPalm32(bytesFromStart);
		rh.attributes = 0x40;
		rh.uniqueID[0] = (pdb->uniqueID[j] & 0xFF0000) >> 16;
		rh.uniqueID[1] = (pdb->uniqueID[j] & 0xFF00) >> 8;
		rh.uniqueID[2] = (pdb->uniqueID[j] & 0xFF);

		fwrite(&rh, sizeof(rh), 1, of);

		bytesFromStart += pdb->recSize[j];

	}
	
	/*
	 * 2 bytes of padding
	 *
	 */

	j = 0;
	fwrite(&j,2,1,of);

	/*
	 * NB Output appinfo block
	 *
	 */

	if (pdb->appInfo) fwrite(pdb->appInfo, 1, pdb->appInfoSize,of);
		
	/*
	 * output records
	 *
	 */

	for (j=0; j < pdb->numRecords; j++) {

		fwrite(pdb->record[j], pdb->recSize[j], 1, of);

	}

	fclose(of);

}

/*
 * PDBGetCreateDate
 *
 */

UInt32 PDBGetCreateDate(PDBType pdb) {

	if (!pdb){ printf("pdb not allocated\n");exit(1);}

	return pdb->createDate;

}
	
/*
 * PDBFree
 *
 */

void PDBFree(PDBType pdb) {

	int j;

	for (j=0; j < pdb->numRecords; j++) {

		free(pdb->record[j]);

	}

	if (pdb->appInfo) free(pdb->appInfo);
	free(pdb);

}

/*
 * PDBDataPack
 *
 * Reverses the order of the specified data, returning a pointer to it. Handles
 * up to 16 bytes
 *
 */

void *PDBDataPack(void *data, int size) {

	static char result[16];
	int j;

	if (size>16) { 
		
		printf("Exception in PDBDataPack");
		exit(1);

	}
	

	for (j=0;j < size;j++) {

		result[j] = ((char*)data)[size-j-1];

	}
	
	return result;

}
