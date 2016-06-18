%option noyywrap

	int i,j;

	int   deg, min, sec;
	float lat[100];
	float lon[100];
	float radius;
	int   numCoords = 0;

	int airfieldLocation = 0;
	int coordBreak = 0;

	char  *notamText;

	struct {

		char id[6];
		float lat,lon;

	} lookup[2048];

	int   numLookups = 0;

	char  altChar[2]; /* 0 = lower, 1 = upper */
	int   alt[2];

	int inNotam = 0;

	FILE *plot, *noplot;

	int LookupAndStore(const char *id) {

		int j;

		for (j=0;j<numLookups;j++) {

			if (strcmp(lookup[j].id, id) == 0)
				break;

		}

		if (j != numLookups) {

			lat[numCoords] = lookup[j].lat;
			lon[numCoords++] = lookup[j].lon;

			return 1;
		}

		return 0;

	}


	void OutputNotam(void) {

		if (numCoords) {

			fprintf(plot, "SW\n%s-\n0\n%c%d\n%c%d\n", notamText,
				 altChar[0], alt[0], altChar[1], alt[1]);
			
			// remove airfield location if not the only coordinate

			if (airfieldLocation && numCoords > 1) {

				numCoords--;
				for (j=0;j< numCoords;j++) {
		
					lat[j] = lat[j+1]; lon[j] = lon[j+1];

				}

				airfieldLocation = 0;

			}

			if (numCoords == 1 && radius == 0.0) radius = airfieldLocation ? 5.0 : 1.0;

		

			if (radius > 0.0 ) {

				fprintf(plot, "C%.5f %.5f %.2f\n", lat[0], lon[0], radius);

			} else {
				
				for (j=0;j<numCoords;j++) 
					fprintf(plot, "L%.5f %.5f\n", lat[j], lon[j]);

			} 

			fprintf(plot, "X\n");

		} else {

			fprintf(noplot, "%s\n", notamText);

		}

		free(notamText);
		inNotam = 0;
		airfieldLocation = 0;
		coordBreak = 0;

		altChar[0] = 'G'; altChar[1] = 'F';
		alt[0] = 0; alt[1] = 999;

	}

WS [ \t\n]*
NUM [0-9]+(\.[0-9]+)?
RADIUS RAD(IUS)?
LAT [0-9]{4,6}[NS]
LON [0-9]{5,7}[WE]


HDR ^[AB]\).*$
SUBHDR ^[CDFG]\).*$
LINE ^.+$

RAD1 {NUM}{WS}NM{WS}{RADIUS}
RAD2 {RADIUS}{WS}{NUM}{WS}NM
RAD3 WI{WS}{NUM}{WS}NM

ALT1 [FG]\)[0-9]+"FT AGL"
ALT2 [FG]\)[0-9]+"FT AMSL"
ALT3 [FG]\)SFC
ALT4 [FG]\)UNL
ALT5 [FG]\)FL[0-9]+

LOOKUPWORD \([A-Z0-9]{3,5}\)

BLANKLINE ^\n

%%

{LINE} %{

	if (inNotam) {

		strcat(notamText,yytext);
		strcat(notamText,"\n");

	}
	REJECT;

%}

{HDR} %{
	if (inNotam) {
		printf("Error, header detected in middle\n");
		exit(1);
	}

	inNotam=1;

	notamText = malloc(4096);
	notamText[0] = 0;
	strcat(notamText,yytext);
	strcat(notamText, "\n");

	numCoords = 0;
	radius    = 0;

	altChar[0] = 'G'; altChar[1] = 'F';
	alt[0] = 0; alt[1] = 999;

	if (yytext[0] == 'A') {

		for (j=2;yytext[j] != ' ';j++);

		yytext[j]=0;

		airfieldLocation = LookupAndStore(&yytext[2]);

	}

%}

{RAD1} %{

	sscanf(yytext, "%f", &radius);

%}

{RAD2}|{RAD3} %{

	for (j=0;yytext[j] < '0' || yytext[j] > '9'; j++);

	sscanf(&yytext[j], "%f", &radius);

%}

{LAT} %{

	if (coordBreak) {

		printf("Detected break: %s\n\n", notamText);
		coordBreak = 0;

	}

	if (yyleng == 5) {

		sscanf(yytext,"%2d%2d", &deg, &min);
		sec = 0;

	} else {

		sscanf(yytext,"%2d%2d%2d", &deg, &min,&sec);

	}

	lat[numCoords] = (float)deg + (float)min/60 + (float)sec/3600;
	if (yytext[yyleng-1] == 'S') lat[numCoords] = -lat[numCoords];

%}
	

{LON}\. %{

	if (inNotam && numCoords > 3) {
		
		coordBreak = 1;

	}

	REJECT;

%}

{LON} %{

	if (yyleng == 6) {

		sscanf(yytext,"%3d%2d", &deg, &min);
		sec = 0;

	} else {

		sscanf(yytext,"%3d%2d%2d", &deg, &min,&sec);

	}

	lon[numCoords] = (float)deg + (float)min/60 + (float)sec/3600;
	if (yytext[yyleng-1] == 'W') lon[numCoords] = -lon[numCoords];

	numCoords++;

%}

{ALT1}|{ALT2} %{

	i = yytext[0] - 'F';

	altChar[i] = strstr(yytext, "AGL") ? 'G' : 'A';
	alt[i]     = atoi(&yytext[2]);

%}

{ALT3}|{ALT4} %{

	i = yytext[0] - 'F';

	if (strstr(yytext,"SFC")) {

		altChar[i] = 'G';
		alt[i]     = 0;

	} else {

		altChar[i] = 'F';
		alt[i]     = 999;
	
	}

%}

{ALT5} %{

	i = yytext[0] - 'F';

	altChar[i] = 'F';
	alt[i]     = atoi(&yytext[4]);

%}


{LOOKUPWORD} %{

	if (inNotam) {

		int j;
		yytext[yyleng - 1] = 0;

		/*
		 * note: only looking at part between "(" & ")"
  		 *
		 */

		LookupAndStore(&yytext[1]);

	}

%}

\ ORIGIN[\. ] %{

	if (!airfieldLocation && numCoords > 1) {

		lat[numCoords] = lat[0];
		lon[numCoords++] = lon[0];

	}

%}

{BLANKLINE} %{

	if (inNotam) OutputNotam(); 

%}

. { }

\n { }

<<EOF>> if (inNotam) OutputNotam(); exit(0);

%%

int main(int argc, char **argv) {

	FILE *lookupFile = fopen("lookup.dat","r");
	int i;

	if (!lookupFile) {

		printf("Error: can't open lookup.dat\n");
		exit(1);

	}

	i = 0;
	while (!feof(lookupFile)) {

		char id[20];
		float lat, lon;

		fscanf(lookupFile, "%s %f %f", id, &lat, &lon);

		lookup[i].lat = lat; lookup[i].lon = lon;
		strcpy(lookup[i].id, id);
		i++;

	}
	numLookups = i;
	fclose(lookupFile);

	plot = fopen("tfrs.fma", "w");
	noplot = fopen("noplot.txt", "w");

	yylex();

	fclose(plot);
	fclose(noplot);

}
