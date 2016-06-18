#include <stdio.h>
#include <string.h>
#include <time.h>

#include "PDBManager.h"

#define PI 	3.1415926535897932384626433832795
#define DEGTORAD(x) (x*PI/180)

typedef struct {

	double lat,lon;
	float magv, alt;

	char text[3];

} __attribute__ ((packed)) 
	CoPilotWaypointType;


int main(int argc, char **argv) {

	char ident[11], name[101];

	UInt32 uniqueIds[32768];

	float frequency;
	float latf,lonf;
	float magv,alt;
	char  *mydisclaimer;
	int   count = 0;

	UInt32 createDate;
	UInt32 creator;
	UInt32 type;

	UInt32 uid;
	Boolean uniqueIDFound; 
	Boolean logging = false;

	int j;
	int freq[256];
	PDBType pdb;

	if (argc < 3 || argc > 4) {

		printf("Usage: <process> | waypoint.exe <outfile> <dbname> [<message>]\n");
		exit(0);

	}

	if (strcmp(argv[2],"CoPilot Waypoint") == 0) {

		creator = 'GXBU';
		type = 'wayp';

	} else if (strcmp(argv[2],"System Update") == 0) {
	
		creator = 'GXBU';
		type = 'swpu';

	} else {

		creator = 'BHMN';
		type = 'FMDB';

	}

	pdb = PDBNew(argv[2], creator, type);

	for (j=0;j<256;j++) freq[j] = 0;

	createDate= PDBGetCreateDate(pdb);

	while (scanf("%s %f %f %f %f\n", &ident, &latf, &lonf, &magv, &alt) != EOF) {

		CoPilotWaypointType *wp;
		double lat,lon;
		void *packPtr;
		char noteLine[160];
		char notes[2048];
		int wpSize;
		int idlen;
		UInt32 tumbler;
		UInt32 tmpuid;


		latf = DEGTORAD(latf);
		lonf = -DEGTORAD(lonf);
		magv = -DEGTORAD(magv);
		
		lat = latf;
		lon = lonf;

		gets(name);

		notes[0] = 0;

		/*
		 * notes, lines terminated by line of XXX
		 *
		 */

		gets(noteLine);
		while (strcmp(noteLine,"XXX") != 0) {

			strcat(notes, noteLine);
			strcat(notes, "\n");
			gets(noteLine);

			if (strlen(notes) > 1800) {

				printf("Error: Notes too long for %s", ident);
				exit(1);

			}

		}

		/*
		 * frequency analysis
		 *
		 */

		for (j=0;j<strlen(ident);j++) {

			freq[ident[j]]++;

		}

		/*
		 * calculate a unique ID for the waypoint.
		 *
		 * This is the same algorithm as used by Paul Tomblin's waypoint
		 * generator at navaid.com
		 *
		 * Encodes 0-9  ->  1-10
		 *         A-Z  -> 11-36
		 *         a-z  -> 11-36
		 *
		 * To get around the duplicate ID problem as far as possible we use
		 * a 'tumbler' variable if the ID isn't unique. Basically, this
		 * 'uses up' the values from 37 to 63 that are unused in each of 
		 * the 6 bit encodings, starting from the right-most 6 bits and
		 * moving to the left at 27 and 54.
		 *
		 * It will inevitably lead to some IDs changing as the database
		 * changes over time.
		 *
		 */

		idlen = strlen(ident);
		if (idlen > 4) idlen = 4;
		uid=0;
		for (j=0; j<idlen; j++) {

			uid <<= 6;

			if (ident[j] >= '0' && ident[j] <= '9')  // 0 to 9

				uid += ident[j] - 47;

			else if (ident[j] >= 'A' && ident[j] <= 'Z') 

				uid += ident[j] - 54;

			else if (ident[j] >= 'a' && ident[j] <= 'z') 

				uid += ident[j] - 86;

		}

		tumbler = 0;
		tmpuid = uid;
		do {
			/*
			 * check for UID clashes
			 *
			 */

			uniqueIDFound=true;
			for (j=0; j<count;j++) {

				/* 
				 * try next combination if this one isn't unique
				 *
				 */

				if (uid == uniqueIds[j]) {
					
					uniqueIDFound = false;
					if (tumbler < 27) {

						uid = (tmpuid & 0x00FFFFC0) | (tumbler + 37);

					} else if (tumbler < 27*2) {

						uid = (tmpuid & 0x00FFF03F) | ((tumbler - 27) << 6);

					} else {

						uid = (tmpuid & 0x00FC0FFF) | ((tumbler - 27*2) << 12);

					}	

					if (++tumbler == 27*3) {

						printf("Warning: no UID possible for waypoint %s\n", ident);

					}

					break;
					
				}

			}

		} while (!uniqueIDFound);

		if (logging) {

			printf("%s %lu %lu\n", ident, uid, tumbler);

		}

		uniqueIds[count] = uid;

		wpSize = sizeof(CoPilotWaypointType)+strlen(ident)+strlen(name)+strlen(notes);
		wp = malloc(wpSize);
		if (!wp) {

			printf("Memory allocation failed!\n");
			exit(1);

		}

		packPtr = (void*)wp;

		wp->lat = PDBPalmDouble(lat);
		wp->lon = PDBPalmDouble(lon);
		wp->magv = PDBPalmFloat(magv);
		wp->alt = PDBPalmFloat(alt);

		packPtr = &(wp->text[0]);
		PDBPackString(packPtr, ident);
		PDBPackString(packPtr, name);
		PDBPackString(packPtr, notes);

		PDBAddRecord(pdb, (void*)wp, wpSize, uid);

		free(wp);
		if (count++ == 32767) {

			printf("Error: too many waypoints\n");
			exit(1);

		}

	}

	/*
	 * AppInfo comprises version (Int16) and creation date (UInt32) then the
	 * text itself.
	 *
	 * (CoPilot specification)
	 *
	 */

	if (argc == 4) {

		mydisclaimer = malloc(strlen(argv[3])+7 );

	} else {

		mydisclaimer = malloc(6);

	}

	*((Int16*)mydisclaimer) = 0;
	*((UInt32*)(&mydisclaimer[2])) = PDBPalm32(createDate);

	if (argc == 4) {
		
		strcpy(&mydisclaimer[6], argv[3]);

	}


	PDBSetAppInfo(pdb, mydisclaimer, strlen(&mydisclaimer[6])+7);
	PDBWrite(pdb, argv[1]);

	PDBFree(pdb);

#ifdef STATS
	for (j=0;j<128;j++) {

		if (freq[j]) printf("%ld %c\n", freq[j], j);

	}
#endif

}
