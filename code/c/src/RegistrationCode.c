/*
 * RegistrationCode.c
 *
 * FlightMaster's registration code generator
 */

/*******************************************************************************
 *
 * public functions
 *
 */


/*
 * GenerateRegistrationCode
 *
 * Generates a registration code based on the
 * string. If the string is longer than 10 characters, only the
 * first 5 and last 5 characters are used.
 *
 * Returns the registration code
 *
 */

#define REVOKE_COUNT 0

static UInt32 GenerateRegistrationCode(UInt32 seed, UInt32 prime, const char *uid) {

	UInt16 j;
	UInt32 code;

	code = seed == 0 ? 49151 << 5 : seed;

	for (j=0;j<StrLen(uid);j++) {
		code += uid[j]*uid[j]*(prime*j*(j+1)+2579*prime);
	}

	//for (j=0;j<REVOKE_COUNT;j++) {
		//if ((revokedCodes[j] ^ 'APFM') == code)
			//return code ^ 'APFM';
	//}

	return code;

}

/*
 * function : GetDemoDate
 *
 * Extracts the demo-date bits from the supplied registration
 * code
 *
 * Returns the demo-date as a value like 20070401 for 1st April 2007. Returns
 * zero if the code/date is invalid
 *
 * Demo date is 10 bits at bits 12 to 21: 
 *
 * 0-4 = day
 * 5-8 = month
 * 9   = year (2007 or 2008)
 *
 */

static UInt32 GetDemoDate(UInt32 code, const char *uid, UInt32 seed, UInt32 prime, UInt32 epochYear) {

	const UInt32 dateMask = 0xFFC00FFF;
	UInt32 dateBits = ((code & ~dateMask) >> 12);
	UInt32 date = (epochYear + (dateBits >> 9)) * 10000 + ((dateBits >> 5) & 0xF)*100 + (dateBits & 0x1F);

	code &= dateMask;


	if ((GenerateRegistrationCode(seed, prime, uid) & dateMask) == code) 

		return date;

	else 

		return 0;

}


