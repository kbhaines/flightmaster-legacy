/*
 *
 * Fixed point maths library
 *
 */

#ifndef _FIXED_H_INCLUDED
#define _FIXED_H_INCLUDED

#include "Platform.h"

typedef long long int64;

/*
 *
 * fixed point math
 *
 */

typedef Int32 FP14;


/*
 *
 * function : FPDiv
 *
 * Divide two FP14 numbers
 *
 * result = d1 / d2
 *
 */

static FP14 FPDiv(FP14 d1, FP14 d2) {

	return (FP14) (((int64)d1 << 14 )/ (int64)d2);
}

#define FPMul(m1,m2) ( ((int64)m1*(int64)m2) >> 14 )

/*
 * function : FPMul
 *
 * Multiply two FP14 numbers
 *
 */

/*static FP14 FPMul(FP14 m1, FP14 m2) {

	int64 temp = (int64)m1 * (int64)m2;

	return (temp >> 14);

}
*/

/*
 *
 * FPAdd and FPSub are macros...
 *
 */

#define FPAdd(a1, a2) ((a1) + (a2))
#define FPSub(s1, s2) ((s1) - (s2))

/*
 *
 * Int2FP, FP2Int and Double2FP, FP2Double are macros...
 *
 */

#define Int2FP(x) ((FP14)(x) << 14)
#define FP2Int(x) ((x) >> 14)
#define FP2Double(x) (((double)(x)) / (1 << 14))
#define Double2FP(x) ((FP14) (((double)x) * (1 << 14)))

/*
 *
 * Useful fixed point constants
 *
 */
#define FPZero Int2FP(0)
#define FPOne Int2FP(1)
#define FPTwo Int2FP(2)
#define FPHalf (1 << 13)

/*
 *
 * FPFract returns fractional part of FP14, FPRound
 * rounds the number first
 *
 */

#define FPFract(x) ((x) & 0x3fff)
#define FPRoundInt(x) ( (x) > 0 ? (FP2Int((x)+FPHalf)) : (FP2Int((x)-FPHalf)) )


#ifdef COMPLEX
/*
 *
 * Table based fixed point sine and cosine 
 * functions.
 *
 */

extern FP14 sine[];
extern FP14 cosine[];

#define FPSin(x) (sine[(x) & 0xff])
#define FPCos(x) (cosine[(x) & 0xff])

#define FPSqrt(x) FPSqrt2(x)
extern FP14 FPSqrt1(FP14 value);
extern FP14 FPSqrt2(FP14 value);

#endif

#endif
