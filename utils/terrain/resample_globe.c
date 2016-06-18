#include <stdio.h>
#include <stdlib.h>

/*
 * This program resamples a previously converted globe file from one size to
 * another. Typically, from 10800 to 8192/4096/2048.
 * 
 * The input file is in flightmaster normalised byte format. This is 8 bits per
 * 30 arc-second of data. 
 *
 * The resampling process preserves the tallest (brightest) pixel of the sample
 * set, different from image processing systems which tend to average out the
 * highest points.
 *
 */

#define IN_WIDTH 10800
#define IN_HEIGHT 10800
#define IN_SIZE (IN_WIDTH*IN_HEIGHT)

#define OUT_WIDTH 8192
#define OUT_HEIGHT 8192
#define OUT_SIZE (OUT_WIDTH*OUT_HEIGHT)

unsigned char src[IN_HEIGHT][IN_WIDTH];
unsigned char dst1[IN_HEIGHT][OUT_WIDTH]; 	// intermediate resample (x only)
unsigned char dst2[OUT_HEIGHT][OUT_WIDTH];	// final resample (x and y)

/*
 * resample
 *
 * Heart of the resampling program, this algorithm is based on a fast and
 * simple bitmap resize algorithm
 *
 */

void resample(unsigned char *src, int src_size, unsigned char *dst,  int dst_size) {

	int sp = 0, dp = 0;	// src/dst pointers
	int inc = 0;
	unsigned char pixel = 0;

	while (sp < src_size) {

		/*
		 * save the highest point
		 *
		 */

		if (src[sp] > pixel) pixel = src[sp];

		inc += dst_size;

		if (inc > src_size || sp == src_size-1) {

			inc -= src_size;
			dst[dp++] = pixel;
			pixel = 0;

		}

		sp++;

	}
	
}


int main(int argc, char **argv) {

	FILE *fi, *fo;
	int j;
	unsigned short int max = 0;
	unsigned char out;
	int in_width, in_height;
	int out_width, out_height;

	if (argc != 7) {

		printf("Usage: resample_globe <in-x> <in-y> <out-x> <out-y> <input-filename> <output-filename>\n");
		printf("(Input file limited to 10800x10800 byte-sized samples)\n");
		exit(0);

	}

	in_width = atoi(argv[1]);
	in_height = atoi(argv[2]);
	out_width = atoi(argv[3]);
	out_height = atoi(argv[4]);

	if (in_width == 0 || in_height == 0 || out_width ==0 || out_height == 0) {

		printf("Error in dimension parameters\n");
		exit(1);

	}

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
	
	printf("Reading input...\n");
	for (j = 0; j< in_height; j++) 

		fread(src[j], sizeof(char), in_width, fi);

	/*
	 * first resample the file in the x direction
	 *
	 */

	printf("Resampling along X-axis...\n");
	for (j=0;j < in_height; j++) {

		resample(src[j], in_width, dst1[j], out_width );

	}

	printf("Resampling along Y-axis...\n");
	for (j=0; j < out_width; j++) {

		unsigned char tmp_src[in_height];
		unsigned char tmp_dst[out_height];
		int k;

		/*
		 * copy a vertical slice from dst1, resample it
		 * and then put it into dst2
		 *
		 */

		for (k = 0; k < in_height; k++) tmp_src[k] = dst1[k][j];
		
		resample(tmp_src, in_height, tmp_dst, out_height);

		for (k = 0; k < out_height; k++) dst2[k][j] = tmp_dst[k];

	}

	printf("Writing results...\n");
	for (j = 0; j<out_height;j++) 
		fwrite(dst2[j], sizeof(char), out_width,fo);

	fclose(fi);
	fclose(fo);

}
