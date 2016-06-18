#include <stdio.h>
#include <stdlib.h>

/*
 * This program converts from globe format into flightmaster normalised format.
 * 
 * The input file is in globe format, comprising 2 bytes per 30 arc-second of
 * data.  Altitude is in metres, from -407 to 8752 metres and -500 is used to
 * represent areas of ocean (sea level).
 *
 * The output file must be in flight master normalised format. This is 8 bits
 * per 30 arc-second of data. This format uses 125 feet per sample size:
 *
 * 0 = -1500 1 = -1375 2 = -1250 etc. Total range is from 0 to 242, leaving the
 * upper range 254-255 for future expansion.
 *
 * 12 = sea level, and is used to represent ocean (values of -500 from the
 * input file). Areas of land which fall at sea level (12) will be adjusted upwards
 * from 12 to 13.
 *
 * To convert from altitude (in feet) to normalised form:
 *
 * 	alt + 1500
 * 	----------
 * 		125
 *
 * To convert from normalised form to altitude:
 *
 *  ( n * 125 ) - 1500
 *
 */


#define OCEAN 12

int main(int argc, char **argv) {

	FILE *f, *fo;
	int count = 0;
	short int i;
	unsigned short int max = 0;
	unsigned char out;

	if (argc != 3) {

		printf("Usage: convert_globe <input-filename> <output-filename>\n");
		exit(0);

	}

	f = fopen(argv[1],"r");
	if (!f) {

		printf("Error opening input file\n");
		exit(1);

	}

	fo = fopen(argv[2],"w");
	if (!fo) {

		printf("Error opening output file\n");
		exit(1);

	}
		
	do {

		fread(&i, 2, 1, f);
		if (feof(f)) break;

		/*
		 * make note of maximum altitude in file
		 *
		 */

		if (i > max) max = i;

		if (i == -500) {

			/*
			 * ocean
			 *
			 */

			out = OCEAN;

		} else {

			int feet = ((int)i * 3281) / 1000;

			
			/*
			 * +63 feet to round up/down
			 *
			 */

			out = (feet + 1563) / 125;

			/*
			 * modify land value to not use the OCEAN value
			 *
			 */

			if (out == OCEAN) out = OCEAN + 1;

		}

		fwrite(&out,1,1,fo);
		count++;

	} while (1);

	fclose(f);
	fclose(fo);

	printf("Read: %d Max: %d\n", count, max);
	
}
