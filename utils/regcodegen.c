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

/*
 * GenerateRegistrationCode_v1
 *
 * Generates a registration code based on the
 * string. If the string is longer than 10 characters, only the
 * first 5 and last 5 characters are used.
 *
 * Returns the registration code
 *
 */
static UInt16 GenerateRegistrationCode_v1(const char *uid) {
	char str[11];
	Int16 j, uidLen;
	UInt16 code = 65535;

	uidLen = StrLen(uid);
	if (uidLen>10) {
		/* chop out first 5 and last 5 characters */
		for (j=0;j<5;j++) {
			str[j] = uid[j];
		}
		for (j=0;j<5;j++) {
			str[j+5] = uid[j+uidLen-5];
		}
		str[10] = 0;
	} else {
		StrCopy(str, uid);
	}
	
	code = 49151;
	for (j=0;j<StrLen(str);j++) {
		code += str[j]*str[j]*(89*j+2579);
	}

	return code;
}

/*
 * GenerateRegistrationCode_v2
 *
 * Generates a registration code based on the string. 
 *
 * Returns the registration code
 *
 */

static UInt32 GenerateRegistrationCode_v2(UInt32 seed, UInt32 prime,
		const char *uid) {

	UInt16 j;
	UInt32 code = 65535;

	code = seed == 0 ? 49151 << 5 : seed;

	for (j=0;j<StrLen(uid);j++) {
		code += uid[j]*uid[j]*(prime*j*(j+1)+2579*prime);
	}

	return code;
}

/*
 * function : GenerateDemoCode
 * 
 * Demonstration version of FlightMaster can have an expiry date which is
 * encoded into the 32 bit reg. code. The date code is the seed for the
 * registration code
 *
 */

static UInt32 GenerateDemoCode(UInt32 prime, const char *uid, UInt32 isoDate) {

	const UInt32 dateMask = 0xFFC00FFF;
	UInt32 dateBits;
	UInt32 year = isoDate/10000;
	UInt32 month= (isoDate/100) % 100;
	UInt32 day  = (isoDate % 100);

	dateBits = ((year - 2009) << 9) | (month << 5) | day;

	return (GenerateRegistrationCode_v2(0, prime, uid) & dateMask) | (dateBits << 12);

}

/***************************************************************************/

int main(int argc, char **argv) {

	/*
	 * prime is for FM.
	 *
	 * Use -a option to set prime for AP
	 *
	 */

	UInt32 prime = 93997;
	UInt32 seed  = 0;

	int   optChar;
	extern char *optarg;
	extern int optind;

	Int16 codeVersion = 1;
	UInt32 demoDate = 0;
	UInt32 newCode;

	/*
	 * parse command line options
	 *
	 */

	while ((optChar = getopt(argc, argv, "12d:s:p:")) > -1) {

		switch (optChar) {

		case '1': codeVersion = 0;
			  break;

		case '2': codeVersion = 1;
			  break;

		case 'd': demoDate = atoi(optarg);
			  break;

		case 's': seed = atoi(optarg);
			break;

		case 'p': prime = atoi(optarg);
			break;
				  
		case '?': exit(1);
			  break;

		}

	}

	if (optind != argc-1) {
		printf("Error: Please supply a hotsync ID (in quotes if necessary\n");
		exit(1);
	}

	if (demoDate) {

		newCode = GenerateDemoCode(prime, argv[optind], demoDate);

	} else {

		switch (codeVersion) {

		case 0: newCode = GenerateRegistrationCode_v1(argv[optind]);
			break;

		case 1: newCode = GenerateRegistrationCode_v2(seed, prime, 
					argv[optind]);
			break;

		}
	}

	printf ("%lu\n", newCode);

}
