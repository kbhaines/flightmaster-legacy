/*
 * regcodegen.c
 *
 * Generate a mininav registration code based on a supplied
 * HotSync ID passed in parameter 1.
 */

#include <time.h>
#include <unistd.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>


typedef unsigned short int 	UInt16;
typedef short int		Int16;
typedef unsigned int		UInt32;
typedef int			Int32;
typedef unsigned char		Boolean;

enum { false = 0, true = 1 };

#define StrLen(s)  strlen(s)
#define StrCopy(d,s) strcpy(d,s)

#include "RegistrationCode.c"

/*
 * function : GenerateDemoCode
 * 
 * Demonstration version of FlightMaster can have an expiry date which is
 * encoded into the 32 bit reg. code. The date code is the seed for the
 * registration code
 *
 */

static UInt32 GenerateDemoCode(UInt32 seed, UInt32 prime, UInt32 epochYear, const char *uid, UInt32 isoDate) {

	const UInt32 dateMask = 0xFFC00FFF;
	UInt32 dateBits;
	UInt32 year = isoDate/10000;
	UInt32 month= (isoDate/100) % 100;
	UInt32 day  = (isoDate % 100);

	if (year-epochYear != 0 && year-epochYear != 1) {
	
		fprintf(stderr, "Error: year epoch mismatch\n");
		return 0;
	}
	
	dateBits = ((year - epochYear) << 9) | (month << 5) | day;

	return (GenerateRegistrationCode(seed, prime, uid) & dateMask) | (dateBits << 12);

}


/***************************************************************************/

int main(int argc, char **argv) {

	UInt32 prime = 0;
	UInt32 seed  = 0;

	int   optChar;
	extern char *optarg;
	extern int optind;

	Int16 codeVersion = 1;
	UInt32 demoDate = 0;
	UInt32 newCode;
	UInt32 epochYear = 2009;
	UInt32 demoCode = 0;

	/*
	 * parse command line options
	 *
	 */

	if (argc == 1) {
		
		printf("Usage: %s [-d <demodate> [-e epoch-year]] [-s seed] [-p prime] [-r code]\n"
			    "  where demodate format= 20081202 (ISO Format)\n"
				"  -r = to extract demo date from <code>\n"
				"  default epoch = 2009\n"
			   ,argv[0]);
		exit(0);
	}
	
	while ((optChar = getopt(argc, argv, "12e:d:s:p:r:")) > -1) {

		switch (optChar) {

		case '1': codeVersion = 0;
			  break;

		case '2': codeVersion = 1;
			  break;

		case 'd': demoDate = atoi(optarg);
			  break;

		case 'e': epochYear = atoi(optarg);
			  break;
			  
		case 's': seed = atoi(optarg);
			break;

		case 'p': prime = atoi(optarg);
			break;
				  
		case 'r': demoCode = atoi(optarg);
			break;
			
		case '?': exit(1);
			  break;

		}

	}

	
	if (optind != argc-1) {
		printf("Error: Please supply a hotsync ID (in quotes if necessary\n");
		exit(1);
	}

	if (demoCode) {
		
		UInt32 date = GetDemoDate(demoCode, argv[optind], seed, prime, epochYear);
		
		if (date) {
			
			printf("%ld\n", date);
			exit(0);
			
		} else {
			
			fprintf(stderr, "Error: %ld invalid demo code\n", demoCode);
			exit(1);
		}
		
	}
	if (demoDate) {

		newCode = GenerateDemoCode(seed, prime, epochYear, argv[optind], demoDate);

	} else {

		newCode = GenerateRegistrationCode(seed, prime, argv[optind]);
	}

	printf ("%lu\n", newCode);

}
