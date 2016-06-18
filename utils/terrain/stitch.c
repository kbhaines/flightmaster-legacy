#include <stdio.h>
#include <stdlib.h>

int main(int argc, char **argv) {

	FILE *fi[8];
	FILE *fo;
	int j;
	int numfiles;
	int rowsize;
	unsigned char *row;

	if (argc < 5) {

		printf("Usage: stitch <rowsize> <file1>...<file8> <outputfile>\n");
		exit(0);

	}

	rowsize = atoi(argv[1]);
	row = malloc(rowsize);

	/*
	 * open all the files
	 *
	 */

	numfiles = argc-3;

	for (j=0; j< numfiles; j++) {

		FILE **f = &fi[j];

		*f = fopen(argv[j+2],"r"); 
		
		if (!*f) {

			printf("Error opening %s\n", argv[j+2]);
			exit(1);

		}

	}

	fo = fopen(argv[argc-1],"w"); 
	if (!fo) {

		printf("Error opening output file\n");
		exit(1);

	}

	do {

		for (j=0;j<numfiles;j++) {

			int bytes_read = fread(row, sizeof(char) , rowsize, fi[j]);
			fwrite(row,sizeof(char),bytes_read, fo);

			if (feof(fi[j])) break;

		}

		if (feof(fi[0])) break;

	} while (1);

	for (j=0;j<numfiles;j++) {
	
		fclose(fi[j]);

	}

	fclose(fo);

}
