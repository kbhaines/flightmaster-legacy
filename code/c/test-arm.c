#include <PceNativeCall.h>
#include "src/Utils.h"

unsigned int TestArm(const void *emulStateP, void *userData, Call68KFuncType *call68kf) {

	unsigned int j = 0;
	unsigned long x ,y;

	for (y=4800; y; y--) {

		for (x=3200;x;x--) {

			j++;

		}

	}

	return j;

}
