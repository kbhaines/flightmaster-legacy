#include <windows.h>
#include <io.h>
#include <stdio.h>

int main(int argc, char **argv) {
	
	HANDLE hSerial;
    int serialfd;
    FILE *serialFILE;

	char port[128];
    int ackCount = 100;

	sprintf(port, "\\\\.\\COM%s", TEXT(argv[1]));
	hSerial = CreateFile(port,
		GENERIC_READ,
		0,
		NULL,
		OPEN_EXISTING,
		0,
		NULL);

	if (!hSerial) {
		printf("Error: could not open\n");
		exit(1);

	}

    //serialfd = _open_osfhandle(hSerial, _O_RDONLY);
    //serialFILE = _fdopen(serialfd, "r");

	while (!feof(stdin)) {

		int j;
		DWORD num;
		unsigned char buffer[9];

		ReadFile(hSerial, &buffer, sizeof(buffer)-1, &num, NULL);
		buffer[num]=0;
		if (num==0) exit(1);
		for (j=0;j<num;j++) printf("%c",buffer[j]);
		fflush(stdout);

	}
}
