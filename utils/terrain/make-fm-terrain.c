#include <stdio.h>
#include <stdlib.h>

/*
 * make-fm-terrain
 *
 * This program will produce an FM compatible terrain file from
 * the master globe file (which should be 704643072 bytes, 
 * 32768 wide x 16384 tall at 3 resolutions)
 *
 * The file format is:
 *
 * <lat1> <lon1> <lat3> <lon3> <row1...rowN>
 *
 * Lat and Lon are specified in terms of terrain coordinate system, this
 * is based on the globe being 32768 units wide, 16384 units tall. The
 * values are encoded in 32 bits, big endian.
 *
 */

#define GLOBE_WIDTH 32768
#define GLOBE_HEIGHT 16384
#define GLOBE_SIZE (GLOBE_WIDTH*GLOBE_HEIGHT)

#define TC(x) (((x) * 16384)/180)

#define SWAP32(x) ( (((x) & 0xff000000) >> 24) | (((x) & 0x00ff0000) >> 8) \
                   |   (((x) & 0x0000ff00) << 8) | (((x) & 0x000000ff) << 24))


unsigned char buffer[32768];

int write_from_globe(FILE *globe, FILE *fo, int resolution,
		int lat1, int lon1, int lat3, int lon3) {

	int master_offset;
	int row_width;
	int j;

	lat1 = 8191 - lat1; lat3 = 8191 - lat3;
	lon1 += 16384; lon3 += 16384;

	lat1 /= (1 << resolution);
	lon1 /= (1 << resolution);
	lat3 /= (1 << resolution);
	lon3 /= (1 << resolution);

	switch (resolution) {

	case 0: master_offset=0;
			row_width = 32768;
			break;

	case 1: master_offset=GLOBE_SIZE;
			row_width = 16384;
			break;

	case 2: master_offset=GLOBE_SIZE + GLOBE_SIZE / 4;
			row_width = 8192;
			break;

	}

	printf("Processing from %d to %d height %d, column %d, width %d\n",
			lat1, lat3, (lat3-lat1), lon1, (lon3-lon1));

	for (j = lat1; j < lat3; j++) {

		fseek(globe, master_offset + j * row_width + lon1, SEEK_SET);
		fread(buffer, sizeof(unsigned char), (lon3 - lon1) , globe);
		fwrite(buffer, sizeof(unsigned char), (lon3 - lon1) , fo);

	}

}


int main(int argc, char **argv) {

	FILE *fi, *fo;
	int j;
	unsigned short int max = 0;
	unsigned char out;
	int lat1, lon1, lat3, lon3;
	int swp;

	if (argc != 7) {

		printf("Usage: make-fm-terrain <lat1> <lon1> <lat3> <lon3> <input-filename> <output-filename>\n");
		printf("       where lat1/lon1 is top left corner, lat3/lon3 is bottom right (in degrees)\n");
		exit(0);

	}

	lat1 = TC(atoi(argv[1]));
	lon1 = TC(atoi(argv[2]));
	lat3 = TC(atoi(argv[3]));
	lon3 = TC(atoi(argv[4]));

	lon1 = (lon1 / 4) * 4;
	lon3 = (lon3 / 4) * 4;

	fi = fopen(argv[5],"r");
	if (!fi) {

		printf("Error opening input file\n");
		exit(1);

	}

	fo = fopen(argv[6],"w");
	if (!fo) {

		printf("Error opening output file\n");
		exit(1);

	}
	
	/*
	 * output big endian header information
	 *
	 */

	swp = SWAP32(lat1);
	fwrite(&swp, sizeof(swp), 1, fo);
	swp = SWAP32(lon1);
	fwrite(&swp, sizeof(swp), 1, fo);
	swp = SWAP32(lat3);
	fwrite(&swp, sizeof(swp), 1, fo);
	swp = SWAP32(lon3);
	fwrite(&swp, sizeof(swp), 1, fo);

	/*
	 * write the first resolution's worth of data
	 *
	 */

	write_from_globe(fi, fo, 0, lat1, lon1, lat3, lon3);				
	write_from_globe(fi, fo, 1, lat1, lon1, lat3, lon3);				
	write_from_globe(fi, fo, 2, lat1, lon1, lat3, lon3);				
			
	fclose(fi);
	fclose(fo);

}
