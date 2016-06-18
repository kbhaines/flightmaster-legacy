/*
 * AvCalcs.c
 *
 * A set of aviation and navigation calculations & alogrithms
 *
 * (c) 2003 Blackhawk Systems Ltd.
 * 
 */

#include "Platform.h"
#include "AvCalcs.h"
#include "MathLib.h"
#include "GlobalTypes.h"

/******************************************************************************
 *
 * Public functions
 *
 *
 */

void AvCalcRhumbLine(double lat1, double lon1,
		double lat2, double lon2, double *bearing, double *distance) {

	double tol = 0.000000000000001;
	double  dlon_w, dlon_e, dphi, q;
	double tc,d;

	tol = 0.000000000000001;
	dlon_w = fmod(lon2 - lon1, 2*PI);
	if (dlon_w<0)
		dlon_w += 2*PI;
	dlon_e = fmod(lon1 - lon2, 2*PI);
	if (dlon_e<0)
		dlon_e += 2*PI;
	dphi = log(tan(lat2/2+PI/4)/tan(lat1/2+PI/4));
	if (fabs(lat2-lat1) < sqrt(tol)) {
		q = cos(lat1);
	} else {
		q=(lat2-lat1)/dphi;
	}
	if (dlon_w < dlon_e) {
		tc = fmod(atan2(-dlon_w,dphi),2*PI);
		d = sqrt(q*q*dlon_w*dlon_w+pow(lat2-lat1,2)) * 180*60/PI;
	} else {
		tc = fmod(atan2(dlon_e,dphi),2*PI);
		d = sqrt(q*q*dlon_e*dlon_e+pow(lat2-lat1,2)) * 180*60/PI;
	}
	if (tc<0)
		tc+=2*PI;

	*bearing=tc;
	*distance=d;
}

/****************************************************************************/

double AvCalcGreatCircleRange(double lat1, double lon1, 
		double lat2, double lon2) {
	return acos(sin(lat1)*sin(lat2)+cos(lat1)*cos(lat2)*cos(lon1-lon2));

}

/****************************************************************************/

double AvCalcGreatCircleCourse(double lat1, double lon1,
		double lat2, double lon2, double *range) {

	double sinlat1 = sin(lat1);
	double sinlat2 = sin(lat2);
	double coslat1 = cos(lat1);
	double rng;
	double crs;

	rng=acos(sinlat1*sinlat2+coslat1*cos(lat2)*cos(lon1-lon2));

	if (fabs(lon1 - lon2) < 1e-8) {
		LOGTAG("N/S Line");
		if (lat1>lat2)
			crs = PI;
		else
			crs = 0;
	} else {
		LOGTAG("Not N/S Line");
		crs = acos((sinlat2-sinlat1*cos(rng))/(sin(rng)*coslat1));
		LOGINT32((Int32)(crs*180/PI));
		if (! (sin(lon1-lon2)<0))  {
			LOGTAG("Flip");
			crs = 2*PI - crs;
		}
	}
	*range = rng;
	return crs;
}

/****************************************************************************/

double AvCalcOffTrackError(double crsAB, double crsAD, double d) {

	return asin(sin(d)*sin(crsAD-crsAB));

}

/****************************************************************************/

void AvCalcShiftPoint(double *lat, double *lon, double bearing, double range) {

	double latP, lonP, dlon;

	latP =asin(sin(*lat)*cos(range)+cos(*lat)*sin(range)*cos(bearing));
	dlon=atan2(sin(bearing)*sin(range)*cos(*lat),cos(range)-sin(*lat)*sin(latP));
	lonP=fmod( *lon+dlon + PI,2*PI)-PI;

#ifdef XXXX

	/*
	 * this 'old' version only works for dlon < pi/2
	 *
	 */

	latP = asin(sin(*lat)*cos(range)+cos(*lat)*sin(range)*cos(bearing));

	if (latP == 0)  {
		
		lonP = *lon;
		
	} else {
		/*
		double sindlon = sin(bearing)*sin(range)/cos(latP);
		double cosdlon = (cos(range)-sin(*lat)*sin(latP))/(cos(*lat)*cos(latP));
		double dlon = atan2(sindlon, cosdlon);

		lonP = fmod(*lon - dlon + PI, 2*PI) - PI;
		*/
		lonP = fmod(*lon-asin(sin(bearing)*sin(range)/cos(latP))+PI,2*PI)-PI;
	}
#endif
		
	*lat = latP;
	*lon = lonP;
}

/*
 * private functions for CalcMagVarn
 *
 */

static void geomag(int *maxdeg) AVCALCS_SECTION;
static void geomg1(float alt,float glat,float glon,float time,
		float *dec,float *dip,float *ti, float *gv) AVCALCS_SECTION;

#define COFF_LINES 90

typedef struct {

      int n,m;
      float gnm,hnm,dgnm,dhnm;

} CoffData;

static CoffData wmm_coff[] = { 
	{  1, 0, -29616.0,      0.0,      14.7,       0.0},
	{  1, 1,  -1722.7,   5194.5,      11.1,     -20.4},
	{  2, 0,  -2266.7,      0.0,     -13.6,       0.0},
	{  2, 1,   3070.2,  -2484.8,      -0.7,     -21.5},
	{  2, 2,   1677.6,   -467.9,      -1.8,      -9.6},
	{  3, 0,   1322.4,      0.0,       0.3,       0.0},
	{  3, 1,  -2291.5,   -224.7,      -4.3,       6.4},
	{  3, 2,   1255.9,    293.0,       0.9,      -1.3},
	{  3, 3,    724.8,   -486.5,      -8.4,     -13.3},
	{  4, 0,    932.1,      0.0,      -1.6,       0.0},
	{  4, 1,    786.3,    273.3,       0.9,       2.3},
	{  4, 2,    250.6,   -227.9,      -7.6,       0.7},
	{  4, 3,   -401.5,    120.9,       2.2,       3.7},
	{  4, 4,    106.2,   -302.7,      -3.2,      -0.5},
	{  5, 0,   -211.9,      0.0,      -0.9,       0.0},
	{  5, 1,    351.6,     42.0,      -0.2,       0.0},
	{  5, 2,    220.8,    173.8,      -2.5,       2.1},
	{  5, 3,   -134.5,   -135.0,      -2.7,       2.3},
	{  5, 4,   -168.8,    -38.6,      -0.9,       3.1},
	{  5, 5,    -13.3,    105.2,       1.7,       0.0},
	{  6, 0,     73.8,      0.0,       1.2,       0.0},
	{  6, 1,     68.2,    -17.4,       0.2,      -0.3},
	{  6, 2,     74.1,     61.2,       1.7,      -1.7},
	{  6, 3,   -163.5,     63.2,       1.6,      -0.9},
	{  6, 4,     -3.8,    -62.9,      -0.1,      -1.0},
	{  6, 5,     17.1,      0.2,      -0.3,      -0.1},
	{  6, 6,    -85.1,     43.0,       0.8,       1.9},
	{  7, 0,     77.4,      0.0,      -0.4,       0.0},
	{  7, 1,    -73.9,    -62.3,      -0.8,       1.4},
	{  7, 2,      2.2,    -24.5,      -0.2,       0.2},
	{  7, 3,     35.7,      8.9,       1.1,       0.7},
	{  7, 4,      7.3,     23.4,       0.4,       0.4},
	{  7, 5,      5.2,     15.0,       0.0,      -0.3},
	{  7, 6,      8.4,    -27.6,      -0.2,      -0.8},
	{  7, 7,     -1.5,     -7.8,      -0.2,      -0.1},
	{  8, 0,     23.3,      0.0,      -0.3,       0.0},
	{  8, 1,      7.3,     12.4,       0.6,      -0.5},
	{  8, 2,     -8.5,    -20.8,      -0.8,       0.1},
	{  8, 3,     -6.6,      8.4,       0.3,      -0.2},
	{  8, 4,    -16.9,    -21.2,      -0.2,       0.0},
	{  8, 5,      8.6,     15.5,       0.5,       0.1},
	{  8, 6,      4.9,      9.1,       0.0,      -0.1},
	{  8, 7,     -7.8,    -15.5,      -0.6,       0.3},
	{  8, 8,     -7.6,     -5.4,       0.1,       0.2},
	{  9, 0,      5.7,      0.0,       0.0,       0.0},
	{  9, 1,      8.5,    -20.4,       0.0,       0.0},
	{  9, 2,      2.0,     13.9,       0.0,       0.0},
	{  9, 3,     -9.8,     12.0,       0.0,       0.0},
	{  9, 4,      7.6,     -6.2,       0.0,       0.0},
	{  9, 5,     -7.0,     -8.6,       0.0,       0.0},
	{  9, 6,     -2.0,      9.4,       0.0,       0.0},
	{  9, 7,      9.2,      5.0,       0.0,       0.0},
	{  9, 8,     -2.2,     -8.4,       0.0,       0.0},
	{  9, 9,     -6.6,      3.2,       0.0,       0.0},
	{ 10, 0,     -2.2,      0.0,       0.0,       0.0},
	{ 10, 1,     -5.7,      0.9,       0.0,       0.0},
	{ 10, 2,      1.6,     -0.7,       0.0,       0.0},
	{ 10, 3,     -3.7,      3.9,       0.0,       0.0},
	{ 10, 4,     -0.6,      4.8,       0.0,       0.0},
	{ 10, 5,      4.1,     -5.3,       0.0,       0.0},
	{ 10, 6,      2.2,     -1.0,       0.0,       0.0},
	{ 10, 7,      2.2,     -2.4,       0.0,       0.0},
	{ 10, 8,      4.6,      1.3,       0.0,       0.0},
	{ 10, 9,      2.3,     -2.3,       0.0,       0.0},
	{ 10,10,      0.1,     -6.4,       0.0,       0.0},
	{ 11, 0,      3.3,      0.0,       0.0,       0.0},
	{ 11, 1,     -1.1,     -1.5,       0.0,       0.0},
	{ 11, 2,     -2.4,      0.7,       0.0,       0.0},
	{ 11, 3,      2.6,     -1.1,       0.0,       0.0},
	{ 11, 4,     -1.3,     -2.3,       0.0,       0.0},
	{ 11, 5,     -1.7,      1.3,       0.0,       0.0},
	{ 11, 6,     -0.6,     -0.6,       0.0,       0.0},
	{ 11, 7,      0.4,     -2.8,       0.0,       0.0},
	{ 11, 8,      0.7,     -1.6,       0.0,       0.0},
	{ 11, 9,     -0.3,     -0.1,       0.0,       0.0},
	{ 11,10,      2.3,     -1.9,       0.0,       0.0},
	{ 11,11,      4.2,      1.4,       0.0,       0.0},
	{ 12, 0,     -1.5,      0.0,       0.0,       0.0},
	{ 12, 1,     -0.2,     -1.0,       0.0,       0.0},
	{ 12, 2,     -0.3,      0.7,       0.0,       0.0},
	{ 12, 3,      0.5,      2.2,       0.0,       0.0},
	{ 12, 4,      0.2,     -2.5,       0.0,       0.0},
	{ 12, 5,      0.9,     -0.2,       0.0,       0.0},
	{ 12, 6,     -1.4,      0.0,       0.0,       0.0},
	{ 12, 7,      0.6,     -0.2,       0.0,       0.0},
	{ 12, 8,     -0.6,      0.0,       0.0,       0.0},
	{ 12, 9,     -1.0,      0.2,       0.0,       0.0},
	{ 12,10,     -0.3,     -0.9,       0.0,       0.0},
	{ 12,11,      0.3,     -0.2,       0.0,       0.0},
	{ 12,12,      0.4,      1.0,       0.0,       0.0}
};

float AvCalcMagVarn(double lat, double lon, float altitude, Int16 year) {     
      static int maxdeg;
      static float altm, dlat, dlon;
      static float ati, adec, adip;
      static float alt, time, dec, dip, ti, gv;
      static float time1, dec1, dip1, ti1;
      static float time2, dec2, dip2, ti2;
      float x1,x2,y1,y2,z1,z2,h1,h2;
      float ax,ay,az,ah;
      float rTd=0.017453292;

      LOGENTRY;
      
      maxdeg = 12;

      geomag(&maxdeg);

      dlat = RAD_TO_DEG(lat);
      dlon = RAD_TO_DEG(lon);

      altm = altitude;
      alt = altm/1000;

      time = (float)year;

      geomg1(alt,dlat,dlon,time,&dec,&dip,&ti,&gv);
      time1 = time;
      dec1 = dec;
      dip1 = dip;
      ti1 = ti;
      time = time1 + 1.0;

      geomg1(alt,dlat,dlon,time,&dec,&dip,&ti,&gv);
      time2 = time;
      dec2 = dec;
      dip2 = dip;
      ti2 = ti;

/*COMPUTE X, Y, Z, AND H COMPONENTS OF THE MAGNETIC FIELD*/

      x1=ti1*(cos((dec1*rTd))*cos((dip1*rTd)));
      x2=ti2*(cos((dec2*rTd))*cos((dip2*rTd)));
      y1=ti1*(cos((dip1*rTd))*sin((dec1*rTd)));
      y2=ti2*(cos((dip2*rTd))*sin((dec2*rTd)));
      z1=ti1*(sin((dip1*rTd)));
      z2=ti2*(sin((dip2*rTd)));
      h1=ti1*(cos((dip1*rTd)));
      h2=ti2*(cos((dip2*rTd)));
      
/*  COMPUTE ANNUAL CHANGE FOR TOTAL INTENSITY  */
      ati = ti2 - ti1;

/*  COMPUTE ANNUAL CHANGE FOR DIP & DEC  */
      adip = (dip2 - dip1) * 60.;
      adec = (dec2 - dec1) * 60.;


/*  COMPUTE ANNUAL CHANGE FOR X, Y, Z, AND H */
      ax = x2-x1;
      ay = y2-y1;
      az = z2-z1;
      ah = h2-h1;

      LOGEXIT;
      
      return dec1;

}

/*************************************************************************/

static void E0000(int IENTRY,int *maxdeg, float alt,float glat,float glon,float time,
		float *dec,float *dip,float *ti, float *gv)

{
     static int maxord,i,n,m,j,D1,D2,D3,D4;
     static float c[13][13],cd[13][13],tc[13][13],dp[13][13],snorm[169],
	  sp[13],cp[13],fn[13],fm[13],pp[13],k[13][13],pi,dtr,a,b,re,
	  a2,b2,c2,a4,b4,c4,epoch,gnm,hnm,dgnm,dhnm,flnmj,otime,oalt,
	  olat,olon,dt,rlon,rlat,srlon,srlat,crlon,crlat,srlat2,
	  crlat2,q,q1,q2,ct,st,r2,r,d,ca,sa,aor,ar,br,bt,bp,bpp,
	  par,temp1,temp2,parp,bx,by,bz,bh;
     static float *p = snorm;

     CoffData *wmmdat = &wmm_coff[0];

     switch(IENTRY){case 0: goto GEOMAG; case 1: goto GEOMG1;}

GEOMAG:
     //wmmdat = fopen("WMM.COF","r");

/* INITIALIZE CONSTANTS */
      maxord = *maxdeg;
      sp[0] = 0.0;
      cp[0] = *p = pp[0] = 1.0;
      dp[0][0] = 0.0;
      a = 6378.137;
      b = 6356.7523142;
      re = 6371.2;
      a2 = a*a;
      b2 = b*b;
      c2 = a2-b2;
      a4 = a2*a2;
      b4 = b2*b2;
      c4 = a4 - b4;

/* READ WORLD MAGNETIC MODEL SPHERICAL HARMONIC COEFFICIENTS */
      c[0][0] = 0.0;
      cd[0][0] = 0.0;
      epoch = 2000.0;


      for (i=0;i<COFF_LINES;i++) {

	      n = wmmdat->n;
	      m = wmmdat->m;
	      gnm = wmmdat->gnm;
	      hnm = wmmdat->hnm;
	      dgnm = wmmdat->dgnm;
	      dhnm = wmmdat->dhnm;

      //sscanf(c_str,"%d%d%f%f%f%f",&n,&m,&gnm,&hnm,&dgnm,&dhnm);
	      if (m <= n) 
	      {
			c[m][n] = gnm;
			cd[m][n] = dgnm;
			if (m != 0) 
			{
			  c[n][m-1] = hnm;
			  cd[n][m-1] = dhnm;
			}
	      }
	      wmmdat ++;

      }

/* CONVERT SCHMIDT NORMALIZED GAUSS COEFFICIENTS TO UNNORMALIZED */
S4:
      *snorm = 1.0;
      for (n=1; n<=maxord; n++) 
      {
	*(snorm+n) = *(snorm+n-1)*(float)(2*n-1)/(float)n;
	j = 2;
	for (m=0,D1=1,D2=(n-m+D1)/D1; D2>0; D2--,m+=D1) 
	{
	  k[m][n] = (float)(((n-1)*(n-1))-(m*m))/(float)((2*n-1)*(2*n-3));
	  if (m > 0) 
	  {
	    flnmj = (float)((n-m+1)*j)/(float)(n+m);
	    *(snorm+n+m*13) = *(snorm+n+(m-1)*13)*sqrt(flnmj);
	    j = 1;
	    c[n][m-1] = *(snorm+n+m*13)*c[n][m-1];
	    cd[n][m-1] = *(snorm+n+m*13)*cd[n][m-1];
	  }
	  c[m][n] = *(snorm+n+m*13)*c[m][n];
	  cd[m][n] = *(snorm+n+m*13)*cd[m][n];
	}
	fn[n] = (float)(n+1);
	fm[n] = (float)n;
      }
      k[1][1] = 0.0;

      otime = oalt = olat = olon = -1000.0;
      return;

/*************************************************************************/

GEOMG1:

      dt = time - epoch;
      if (otime < 0.0 && (dt < 0.0 || dt > 5.0)) 
      {
      }

      pi = 3.14159265359;
      dtr = pi/180.0;
      rlon = glon*dtr;
      rlat = glat*dtr;
      srlon = sin(rlon);
      srlat = sin(rlat);
      crlon = cos(rlon);
      crlat = cos(rlat);
      srlat2 = srlat*srlat;
      crlat2 = crlat*crlat;
      sp[1] = srlon;
      cp[1] = crlon;

/* CONVERT FROM GEODETIC COORDS. TO SPHERICAL COORDS. */
      if (alt != oalt || glat != olat) 
      {
	q = sqrt(a2-c2*srlat2);
	q1 = alt*q;
	q2 = ((q1+a2)/(q1+b2))*((q1+a2)/(q1+b2));
	ct = srlat/sqrt(q2*crlat2+srlat2);
	st = sqrt(1.0-(ct*ct));
	r2 = (alt*alt)+2.0*q1+(a4-c4*srlat2)/(q*q);
	r = sqrt(r2);
	d = sqrt(a2*crlat2+b2*srlat2);
	ca = (alt+d)/r;
	sa = c2*crlat*srlat/(r*d);
      }
      if (glon != olon) 
      {
	for (m=2; m<=maxord; m++) 
	{
	  sp[m] = sp[1]*cp[m-1]+cp[1]*sp[m-1];
	  cp[m] = cp[1]*cp[m-1]-sp[1]*sp[m-1];
	}
      }
      aor = re/r;
      ar = aor*aor;
      br = bt = bp = bpp = 0.0;
      for (n=1; n<=maxord; n++) 
      {
	ar = ar*aor;
	for (m=0,D3=1,D4=(n+m+D3)/D3; D4>0; D4--,m+=D3) 
	{
/*
   COMPUTE UNNORMALIZED ASSOCIATED LEGENDRE POLYNOMIALS
   AND DERIVATIVES VIA RECURSION RELATIONS
*/
	  if (alt != oalt || glat != olat) 
	  {
	    if (n == m) 
	    {
	      *(p+n+m*13) = st**(p+n-1+(m-1)*13);
	      dp[m][n] = st*dp[m-1][n-1]+ct**(p+n-1+(m-1)*13);
	      goto S50;
	    }
	    if (n == 1 && m == 0) 
	    {
	      *(p+n+m*13) = ct**(p+n-1+m*13);
	      dp[m][n] = ct*dp[m][n-1]-st**(p+n-1+m*13);
	      goto S50;
	    }
	    if (n > 1 && n != m) 
	    {
	      if (m > n-2) *(p+n-2+m*13) = 0.0;
	      if (m > n-2) dp[m][n-2] = 0.0;
	      *(p+n+m*13) = ct**(p+n-1+m*13)-k[m][n]**(p+n-2+m*13);
	      dp[m][n] = ct*dp[m][n-1] - st**(p+n-1+m*13)-k[m][n]*dp[m][n-2];
	     }
	  }
S50:
/*
    TIME ADJUST THE GAUSS COEFFICIENTS
*/
	  if (time != otime) 
	  {
	    tc[m][n] = c[m][n]+dt*cd[m][n];
	    if (m != 0) tc[n][m-1] = c[n][m-1]+dt*cd[n][m-1];
	  }
/*
    ACCUMULATE TERMS OF THE SPHERICAL HARMONIC EXPANSIONS
*/
	  par = ar**(p+n+m*13);
	  if (m == 0) 
	  {
	    temp1 = tc[m][n]*cp[m];
	    temp2 = tc[m][n]*sp[m];
	  }
	  else 
	  {
	    temp1 = tc[m][n]*cp[m]+tc[n][m-1]*sp[m];
	    temp2 = tc[m][n]*sp[m]-tc[n][m-1]*cp[m];
	  }
	  bt = bt-ar*temp1*dp[m][n];
	  bp += (fm[m]*temp2*par);
	  br += (fn[n]*temp1*par);
/*
    SPECIAL CASE:  NORTH/SOUTH GEOGRAPHIC POLES
*/
	  if (st == 0.0 && m == 1) 
	  {
	    if (n == 1) pp[n] = pp[n-1];
	    else pp[n] = ct*pp[n-1]-k[m][n]*pp[n-2];
	    parp = ar*pp[n];
	    bpp += (fm[m]*temp2*parp);
	  }
	}
      }
      if (st == 0.0) bp = bpp;
      else bp /= st;
/*
    ROTATE MAGNETIC VECTOR COMPONENTS FROM SPHERICAL TO
    GEODETIC COORDINATES
*/
      bx = -bt*ca-br*sa;
      by = bp;
      bz = bt*sa-br*ca;
/*
    COMPUTE DECLINATION (DEC), INCLINATION (DIP) AND
    TOTAL INTENSITY (TI)
*/
      bh = sqrt((bx*bx)+(by*by));
      *ti = sqrt((bh*bh)+(bz*bz));
      *dec = atan2(by,bx)/dtr;
      *dip = atan2(bz,bh)/dtr;
/*
    COMPUTE MAGNETIC GRID VARIATION IF THE CURRENT
    GEODETIC POSITION IS IN THE ARCTIC OR ANTARCTIC
    (I.E. GLAT > +55 DEGREES OR GLAT < -55 DEGREES)

    OTHERWISE, SET MAGNETIC GRID VARIATION TO -999.0
*/
      *gv = -999.0;
      if (fabs(glat) >= 55.) 
      {
	if (glat > 0.0 && glon >= 0.0) *gv = *dec-glon;
	if (glat > 0.0 && glon < 0.0) *gv = *dec+fabs(glon);
	if (glat < 0.0 && glon >= 0.0) *gv = *dec+glon;
	if (glat < 0.0 && glon < 0.0) *gv = *dec-fabs(glon);
	if (*gv > +180.0) *gv -= 360.0;
	if (*gv < -180.0) *gv += 360.0;
      }
      otime = time;
      oalt = alt;
      olat = glat;
      olon = glon;
      return;
}

/*************************************************************************/

static void geomag(int *maxdeg)
{
     E0000(0,maxdeg,0.0,0.0,0.0,0.0,NULL,NULL,NULL,NULL);
}

/*************************************************************************/

static void geomg1(float alt,float glat,float glon,float time,
		float *dec,float *dip,float *ti, float *gv)
{
     E0000(1,NULL,alt,glat,glon,time,dec,dip,ti,gv);
}
