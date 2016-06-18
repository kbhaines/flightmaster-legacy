#ifndef REGISTRATION_CODE_H_INCLUDED
#define REGISTRATION_CODE_H_INCLUDED
#include "Platform.h"
/*
 * RegistrationCode.h
 *
 * MiniNav's registration code generator
 *
 * NB This header is never used...
 *
 */

/*
 * function : GetDemoDate
 *
 */

extern UInt32 GetDemoDate(UInt32 code, const char *uid);

/*
 * GenerateRegistrationCode
 *
 * Given a HotSyncID, generates a registration code based on the
 * string. If the string is longer than 10 characters, only the
 * first 5 and last 5 characters are used.
 *
 * Returns the registration code
 */
#ifdef AEROPALM
extern inline UInt32 GenerateRegistrationCode(UInt32 seed, UInt32 prime, const char *uid);
#else
extern inline UInt16 GenerateRegistrationCode(const char *uid);
#endif

#endif
