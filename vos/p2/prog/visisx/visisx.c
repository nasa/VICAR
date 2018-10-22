/*******************************************************************************

VISISX  -- revision of the GLL/NIMS program VISIS2 to allow non-NIMS cubes to be 
converted between VICAR and ISIS.

main differences from VISIS2:

1. Only core, suffixes, and history allowed, no other objects (2D-Hist
 or specplots) supported; 
2. suffixes are more general than in visis2;
2. no label checking of input;
3. NIMS-specific parameters and hard-coded comments are removed;
4. enough VIMS parameters added to make output readable by ISIS/CV.

A principal goal (besides making the files readable in the two systems)
is to preserve any map-label information.

A large number of more general (but still Galileo-oriented) parameters
have been retained, which are optionally processed if present, and if
TASKNAME is specified (note that it's currently specified by default in
the .PDF).

TBDs:
 > is TASKNAME really needed for the zlgets?
 > currently only BSQ & BIL file orgs supported -- add BIP?

04dec98 -lwk- original version, adapted from GLL NIMS program VISIS2
09dec98 -lwk- added U_NL/S to zvopens in get_recinfo (for Solaris)
16dec98 -lwk- added support for BIL files and for SUFFIX planes 
	(backward mode only, not saved!);  specified "int" for functions 
	that return values
18dec98 -lwk- added HOST parameter
18jan99 -lwk- fixed HOST parameter
22jan99 -lwk- fixed CORE_ITEMS in TOISIS mode (was broken when BIL added),
	and CORE_ITEM_TYPE (was VAX-only); added CORE_NAME & CORE_UNIT as 
	they're required by Unix ISIS
26jan99 -lwk- use zvhost for int/realfmt
30jan99 -lwk- replaced HOST parameter by search on CORE_ITEM_TYPE
01feb99 -lwk- treat NATIVE_TIMEs differently per project (tovicar mode only)
02feb99 -lwk- added "U_ORG","BSQ" to zvopen in write_object (TOVICAR) to fix
	problem with BIL cubes
22apr99 -lwk- skip over the AXIS_NAME item in the specplot part of the label
	for the case of a NIMS cube
06jun99 -lwk- allow for partial subset of labels depending on 'maplabs'
	flag -- this is designed to allow NIMSSKY cubes to be processed, but
	ideally each label item should be checked separately
13sep99 -lwk- check for ORTHOGRAPHIC alone and replace by OBLIQUE_ORTHO;
	start search for map keywords at "GROUP = IMAGE_MAP_PROJECTION",
	since some cubes have RADIUS keywords before MAP_PROJECTION_TYPE
19jun00 -lwk- removed objectindex stuff from keyword_value (since we have
        only 1 object containing labels);  deleted a few residual "NIMS"s
        and function byte_nibble;  added EAST/WESTERNMOST_LONGITUDEs to
        ISIS->VICAR mode
20jun00 -bam - added new keywords FIRST_LINE_SAMPLE, LINE_EXPOSURE_DURATION,
        START_TIME, and EASTERNMOST/WESTERNMOST LONGITUDE (these replace
        MINIMUM/MAXIMUM LONGITUDE in the newer files). I have also added a
        new function for double precision values, get_double_value, and
        modified the code to accept this new function.  This is necessary
        for port to LINUX.
11jul00 -lwk- fixed bug checking for map labels in ToVic mode
18jul00 -lwk- checking for Property map labels even if no Task specified
15may02 -lwk- added ISIS 'CYLINDRICAL_EQUAL-AREA'
27apr04 -lwk- merged BAM's and my versions;  increased MAXBANDS to 600;
	check for fullword int in TOVICAR mode;  fixed bug in treatment of 
	backplanes in write_object
29apr04 -lwk- added support for suffix planes in TOVICAR mode;  replaced
	all references to "forward/backward" modes with "tovicar/toisis"
22may04 -lwk- added VIMS keywords: START/STOP_TIME (need special function 
	find_keyword_str to distinguish from NATIVE_TIME), INTERLINE_DELAY, 
	EXPOSURE_DURATION, X/Z_OFFSET
01jun04 -lwk- added VIMS keywords SAMPLING_MODE_ID & SCAN_MODE_ID;  fiddled
	with string delimiters in get_string_value() (again)
15jun04 -lwk- changed algorithm for distinguishing NATIVE_TIME and plain
	TIME, since Cassini has now made both strings
18jun04 -lwk- allow backplanes from co-cube in TOISIS mode;  changed treatment
	of the various suffix planes
22jun04 -lwk- translate CORE_NAME/UNIT to Vicar items; added BAND_SUFFIX items
	for display of backplanes in cv
28jun04 -lwk- added option to read wavelengths in from user parameter
14jul04 -lwk- added FAKEPROJ keyword as a workaround for limited ISIS projections
26jul04 -lwk- changed PROJECT_NAME to MISSION_NAME in TOISIS mode; fixed code to
	find SUFFIX_ITEM_TYPE
22sep04 -lwk- added more VIMS keywords at CCA's request;  fiddled some more with
	the code for START_TIME & NATIVE_START_TIME (latter can be with or without
	quotes!)
12oct04 -lwk- enabled fullword integer support in TOVICAR mode; added label items
	BAND_BIN_ORIGINAL_BAND/ORIGBANDS and (in VICAR) WAVUNIT;  read TARGET name
	in TOISIS mode
19oct04 -lwk- added CORE_MULTIPLIER input for Radar cubes
21oct04 -lwk- get NB from input cube label, not from WAVES! (TOISIS mode)
29oct04 -lwk- added ISIS special values to CORE & BAND_SUFFIX label items (TOISIS mode)
10dec04 -lwk- remove hard-coded nbkpln=9;  added FRAME_LINE/SAMP to BAND_SUFFIX items
	for VIMS
07may05 -lwk- added case of nbkpln=2
08may05 -lwk- write entire Vicar history to PDS History object in TOISIS mode
18may05 -lwk- fixed problems with backplanes in BSQ cube in TOISIS mode; ensure that
	co-cube is always BSQ in TOVICAR mode
01jun05 -lwk- fixed bug introduced by last change in calculation of FILE_RECORDS
27oct05 -lwk- look for LAT_TYPE as well as LATITUDE_TYPE
28oct05 -lwk- added X86 (Linux) hostname support, treat it as AXP.
29oct05 -lwk- changed AXP-UNIX to X86-LINUX at RGD's request since that's the only
	currently officially supported VICAR platform of that flavor

******************************************************************************/

/*#include "gll_main.h" */
#include "mp_routines.h"
#include <stdio.h>
#include <string.h>
#include <math.h>
#include <time.h>
#include <ctype.h>
#include "vicmain_c"                                    /* Vicar */
#include <stdlib.h>
#include "zifmessage.h"
					/*	ERROR HANDLING MACROS	     */
#define return_if_error(A)	zvsignal(A,vstat,0);	if(vstat<=0) return
#define continue_if_error(A)	zvsignal(A,vstat,0);	if(vstat<=0) continue
#define break_if_error(A)	zvsignal(A,vstat,0);	if(vstat<=0) break
#define fill_9s_error(A)	if(vstat<=0) strcpy(A,"999999999999")
#define zero_if_error(A)        if(vstat<=0) *(A)=0

	/*	DEFINE GLOBAL CONSTANTS	     */
/*#define FALSE 			0
#define TRUE 			1 (these are in mp_routines.h) */
#define TOISIS 			1
#define TOVICAR 		0
#define LABELMEMORY		84000
#define HISTMEMORY		24000
#define BUFFERSIZE		512
#define LBL_BLANK_RECORDS 	10
#define HST_BLANK_RECORDS 	20
#define NUMOFPTRS		4
#define MAXBANDS		500

/* definitions for platform-dependent special values:
 * (from $ISISINC/special_pixel.h) */
#define VALID_MIN_IEEE		"16#FF7FFFFA#"
#define NULL_IEEE		"16#FF7FFFFB#"
#define LOW_REPR_SAT_IEEE	"16#FF7FFFFC#"
#define LOW_INSTR_SAT_IEEE	"16#FF7FFFFD#"
#define HIGH_INSTR_SAT_IEEE	"16#FF7FFFFE#"
#define HIGH_REPR_SAT_IEEE	"16#FF7FFFFF#"
#define VALID_MIN_VAX		"16#FFEFFFFF#"
#define NULL_VAX		"16#FFFFFFFF#"
#define LOW_REPR_SAT_VAX	"16#FFFEFFFF#"
#define LOW_INSTR_SAT_VAX	"16#FFFDFFFF#"
#define HIGH_INSTR_SAT_VAX	"16#FFFCFFFF#"
#define HIGH_REPR_SAT_VAX	"16#FFFBFFFF#"

int orbands[MAXBANDS];	/* BAND_BIN_ORIGINAL_BANDS */

float waves[MAXBANDS];	/* Wavelength center in microns	*/
int uwaves;		/* flag for user-specified wavelengths */
char wavunit[20];

struct vitems {			/* VICAR label items or CATALOG fields   */
  double projitem1,		/* 1st std. prll. LAMB; Ref. lat OBLQ   */
	projitem2,		/* 2nd std. prll. LAMB; Ref. lon OBLQ   */
	radii[3],		/* A, B, and C axes lengths		*/
	rotation,		/* Map projection rotation in degrees   */
	scale,			/* Projection scale in KM/PIXEL		*/
	cscale,			/* camera scale (pixels/mm)		*/
	focal,			/* focal length (mm)			*/
	oaline,			/* line of optical axis intercept	*/
	oasamp,			/* sample of optical axis intercept	*/
	center_line,		/* line of MM projection tiepoint	*/
	center_samp,		/* sample of MM projection tiepoint	*/
	center_lat,		/* latitude of MM projection tiepoint	*/
	center_lon,		/* longitude of MM projection tiepoint	*/
	ssc_line,		/* line of subspacecraft point		*/
	ssc_samp,		/* sample of subspacecraft point	*/
	ssc_lat,		/* latitude of ssc point		*/
	ssc_lon,		/* longitude of ssc point		*/
	range;			/* spacecraft-to-planet range		*/
  float	max_range,		/* S/C - Planet maximum distance	*/
	min_range,		/* S/C - Planet minimum distance	*/
	min_sun_d,		/* S/C - Sun minimum distance		*/
	max_sun_d,		/* S/C - Sun maximum distance		*/
	min_cb_d,		/* S/C - Central body minimum distance	*/
	max_cb_d,		/* S/C - Central body maximum distance	*/
	b_ssc_lat,		/* latitude of starting sub-s/c point	*/
	b_ssc_lon,		/* longitude of starting sub-s/c  point	*/
	e_ssc_lat,		/* latitude of ending sub-s/c point	*/
	e_ssc_lon,		/* longitude of ending sub-s/c point	*/
	b_ssl_lat,		/* latitude of starting sub-sol point	*/
	b_ssl_lon,		/* longitude of starting sub-sol point	*/
	e_ssl_lat,		/* latitude of ending sub-sol point	*/
	e_ssl_lon,		/* longitude of ending sub-sol point	*/
	min_lat,		/* Minimum latitude of MM cube		*/
	min_lon,		/* Minimum longitude of MM cube		*/
	max_lat,		/* Maximum latitude of MM cube		*/
	max_lon,		/* Maximum longitude of MM cube		*/
	cen_lon,		/* center longitude of MM cube		*/
	inci_angle,		/* Photometric angles			*/
	emis_angle,		
	phas_angle,		
	phot_cut,		/* phot.func. cutoff wavelength		*/
	minn_exp,		/* exponent for Minnaert phot.func. 	*/
	azimuth_sun,		/* Azimuth of sun in planet's coordina. */
	azimuth_sc,		/* Azimuth of spacecraft in planet's co */
	slant_dist_base,	/* Slant distance base for GEOCUBE	*/
	slant_dist_mult,	/* Slant distance multiplier		*/
	solarflux[MAXBANDS];	/* Solar flux vector 			*/
  int	native_times[2][3],	/* RIM.MF.RTI- start and stop	*/
	sdband,sdgeo;		/* cube/cocube band #s for STD_DEV	*/
  char	obsnote[338],		/* Observation note			*/
	projection[41],		/* map projection type 			*/
	target[20],		/* Target body name			*/
	targetcode[4],		/* Target body code 			*/
	obsname[13],		/* observation name			*/
	phase_name[50],		/* mission phase name			*/
	proj_name[20],		/* project name				*/
	ins_name[20],		/* instrument name			*/
	prod_id[30],		/* product id 				*/
	dataname[35],		/* data type of cube, for label */
	dataunits[35],		/* units of cube, for label */
	event_start_time[25],	/* Event start time in PDS format	*/
	event_stop_time[25],	/* Event stop time in PDS format	*/
	photofunc[25];		/* photometric function name		*/
}	v;			/* Pointer to VITEMS structure		*/

int	numofbytes,		/* Number of bytes in PDS label		*/
	objptr[NUMOFPTRS],	/* pointer to objects			*/
	lsef,			/* Label search END flag		*/
	size,			/* Number of bytes in buffer	      	*/
	inunit,			/* Input file's unit number   		*/
	outunit,		/* Output file's unit number	   	*/
	sideunit,		/* Sideplane file's unit number	   	*/
	backunit,		/* Backplane file's unit number	   	*/
	bottunit,		/* Bottomplane file's unit number 	*/
	s1unit,s2unit,s3unit,	/* above get mapped to these dep'g on ORG */
	bytes,			/* Number of bytes/pixel 	     	*/
	flint,			/* float/int flag when 4 bytes/pixel    */
	vstat,			/* status variable for VICAR   	 	*/
	labend;			/* Pointer to end of label */
char 	*bufpoint,		/* Pointer for PDS item parsing 	*/
	*histpoint,		/* Pointer for History object   	*/
	*inptr,			/* Input file pointer 	     		*/
	*outptr,		/* Output file pointer          	*/
	value[8][7],		/* Object pointers	             	*/
	line[120],lineb[80];	/* character string buffers	      	*/

int pgraphic;			/* planetographic latitudes? */

int histfile;			/* History file stuff */
char hfilnam[100];
FILE *hfile,*fopen();

char tasknam[8];
int htask;			/* flag to do history processing */

char verdat[11];		/* VERSION_DATE */
double atof();

char hostname[33];
int ihost;
char format[8];			/* data format of Vicar input */
char forg[8];			/* file org (BSQ, BIL, ...) */
int n1, n2, n3;			/* core dimensions in storage order */
int ns, inl, nb;		/* core dimensions */
int n1suf, n2suf, n3suf;	/* suffix plane dimensions */
int suftyp;			/* suffix format: 0=INT, 1=REAL */
int cocub;			/* flag for presence of V/NIMSCMM co-cube */
int nbkpln;			/* # of planes in co-cube */

float *fdata;			/* buffer needed when backplanes written */

int maplabs;			/* flag for presence of map labels */

float core_mult;

/* stuff for VIMSCMM History: */
int icker, ndpnt, nepherr, nspk, vcmm_flag;
char spkerns[10][101], ckern[101];
float dpoint[100], epherr[100];

char message[80];		/* buffer for zvmessage */

char viclab[20001];		/* buffer for entire vicar label */
int viclabsize;

#define FUNCTION		/* delimit modules */

/***************************************************************************/
void main44(void)
{
  char	cubefile[100];		/* Input files for CUBE	     	     */
  int 	direction;			/* Direction of transformation 	     */

  /* inform user of Program version & update VERSION_DATE for label */
  zifmessage("   ");
  zifmessage("*** VISISX Version 2018-02-27 ***");
  strcpy( verdat, "2005-10-29");

  free(malloc(4096*4096));		/* 	Guarantee sufficient memory  */

  process_parms(&direction,cubefile);	/* 	Get all user parameters      */

  if(direction==TOISIS)
    VICARtoISIS(cubefile);
  else
    ISIStoVICAR();
}


/****************************************************************************/
FUNCTION create_PDSlabel(nrecs,nlabrecs,nhistrecs,cubefile) 
/*
 * Create PDS label for ISIS cube file.
 */
int	*nhistrecs,		/* 	Number of records in history object   */
	*nlabrecs,		/* 	Number of PDS label records	      */
	*nrecs;			/* 	Number of records for cube file	      */
char	cubefile[100];	/* 	Input files to generate cube          */
{
  int additionalbytes, bufptr, count, delim, index, object, labelbytes, nsi,
   i, ibk, j, len1, len2, len3, quoted, x, y, y1, z;
  float arval, pixelsperdegree, sav;
  char cc, bp_type[11], dtype[4], geoplane[31], notes[7][80], tstr[50], obuf[338];
  char *hpoint;
  time_t lt;

  struct tm *ptr;

	/* Allocate memory for buffer */
  bufpoint = (char *)calloc(LABELMEMORY,sizeof(char));	
  if( bufpoint==NULL )
    zmabend( "Memory allocation error in create_PDSlabel routine");

  strcpy(bufpoint,"CCSD3ZF0000100000001NJPL3IF0PDS200000001 = SFDU_LABEL\r\n");
  strcat(bufpoint,"\r\n/* File Structure */\r\n\r\n");
  strcat(bufpoint,"RECORD_TYPE = FIXED_LENGTH\r\n");
  strcat(bufpoint,"RECORD_BYTES = 512\r\n");
  strcat(bufpoint,"FILE_RECORDS = XXXXXX\r\n");
  strcat(bufpoint,"LABEL_RECORDS = XXXXXX\r\n");
  strcat(bufpoint,"FILE_STATE = DIRTY\r\n\r\n");

  strcat(bufpoint,"^HISTORY = XXXXXX\r\n");	
  strcat(bufpoint,"OBJECT = HISTORY\r\n");
  strcat(bufpoint,"END_OBJECT = HISTORY\r\n\r\n");

  strcat(bufpoint,"^QUBE = XXXXXX\r\n");	/* QUBE OBJECT		*/
  strcat(bufpoint,"OBJECT = QUBE\r\n\r\n");
  strcat(bufpoint,"/* Qube structure */");
  strcat(bufpoint,"\r\n\r\n AXES = 3\r\n");

  if (!strcmp(forg,"BSQ")) 
   strcat(bufpoint," AXIS_NAME = (SAMPLE,LINE,BAND)\r\n\r\n");
  else if (!strcmp(forg,"BIL"))
   strcat(bufpoint," AXIS_NAME = (SAMPLE,BAND,LINE)\r\n\r\n");

  strcat(bufpoint,"/*  Core description */\r\n\r\n");
  if (!strcmp(forg,"BSQ")) 
    sprintf(line," CORE_ITEMS = (%d,%d,%d)\r\n",ns,inl,nb);
  else if (!strcmp(forg,"BIL"))
    sprintf(line," CORE_ITEMS = (%d,%d,%d)\r\n",ns,nb,inl);
  strcat(bufpoint,line);

  sprintf(line," CORE_NAME = %s\r\n",v.dataname);
  strcat(bufpoint,line);
  sprintf(line," CORE_UNIT = %s\r\n",v.dataunits);
  strcat(bufpoint,line);

  sprintf(line," CORE_ITEM_BYTES = %d\r\n", bytes);
  strcat(bufpoint,line);

  /* construct CORE_ITEM_TYPE
   * Note that AXP-Unix/Linux (and X86) have PC architecture, and all other
   * Unix's are considered SUN */
  strncpy( dtype, hostname, 3);
  dtype[3] = '\0';
  if (!strcmp(hostname,"AXP-VMS"))
    strcpy( dtype, "VAX");
  else if (!strcmp(dtype,"AXP") || !strcmp(dtype,"X86") || !strcmp(hostname,"DECSTATION"))
    strcpy( dtype, "PC");
  if (!strcmp(dtype,"HP-") || !strcmp(dtype,"SGI")) 
    strcpy( dtype, "SUN");
  if (bytes==1) 
    sprintf( line," CORE_ITEM_TYPE = %s_UNSIGNED_INTEGER\r\n",dtype);
  else if (bytes==2) 
    sprintf( line," CORE_ITEM_TYPE = %s_INTEGER\r\n",dtype);
  else if (bytes==4) {
    if (!strncmp(format,"REAL",4)) 
      sprintf( line," CORE_ITEM_TYPE = %s_REAL\r\n",dtype);
    else 
      sprintf( line," CORE_ITEM_TYPE = %s_INTEGER\r\n",dtype);
  }
  else 
    zmabend(" CORE_ITEM_TYPE not supported");
  strcat( bufpoint, line);

  if (bytes==2) {
    strcat(bufpoint," CORE_VALID_MINIMUM =         -32752\r\n");
    strcat(bufpoint," CORE_HIGH_REPR_SATURATION =  -32764\r\n");
    strcat(bufpoint," CORE_HIGH_INSTR_SATURATION = -32765\r\n");
    strcat(bufpoint," CORE_LOW_INSTR_SATURATION =  -32766\r\n");
    strcat(bufpoint," CORE_LOW_REPR_SATURATION =   -32767\r\n");
    strcat(bufpoint," CORE_NULL =                  -32768\r\n");
  }
  else if (bytes==4) {
    if (!strncmp(format,"REAL",4)) {
      if (!strcmp(hostname,"AXP-VMS")) {
	strcat(bufpoint," CORE_VALID_MINIMUM =         ");
	strcat( bufpoint, VALID_MIN_VAX);
	strcat( bufpoint, "\r\n");
	strcat(bufpoint," CORE_HIGH_REPR_SATURATION =  ");
	strcat( bufpoint, HIGH_REPR_SAT_VAX);
	strcat( bufpoint, "\r\n");
	strcat(bufpoint," CORE_HIGH_INSTR_SATURATION = ");
	strcat( bufpoint, HIGH_INSTR_SAT_VAX);
	strcat( bufpoint, "\r\n");
	strcat(bufpoint," CORE_LOW_INSTR_SATURATION =  ");
	strcat( bufpoint, LOW_INSTR_SAT_VAX);
	strcat( bufpoint, "\r\n");
	strcat(bufpoint," CORE_LOW_REPR_SATURATION =   ");
 	strcat( bufpoint, LOW_REPR_SAT_VAX);
	strcat( bufpoint, "\r\n");
	strcat(bufpoint," CORE_NULL =                  ");
	strcat( bufpoint, NULL_VAX);
	strcat( bufpoint, "\r\n");
      }
      else {
	strcat(bufpoint," CORE_VALID_MINIMUM =         ");
	strcat( bufpoint, VALID_MIN_IEEE);
	strcat( bufpoint, "\r\n");
	strcat(bufpoint," CORE_HIGH_REPR_SATURATION =  ");
	strcat( bufpoint, HIGH_REPR_SAT_IEEE);
	strcat( bufpoint, "\r\n");
	strcat(bufpoint," CORE_HIGH_INSTR_SATURATION = ");
	strcat( bufpoint, HIGH_INSTR_SAT_IEEE);
	strcat( bufpoint, "\r\n");
	strcat(bufpoint," CORE_LOW_INSTR_SATURATION =  ");
	strcat( bufpoint, LOW_INSTR_SAT_IEEE);
	strcat( bufpoint, "\r\n");
	strcat(bufpoint," CORE_LOW_REPR_SATURATION =   ");
 	strcat( bufpoint, LOW_REPR_SAT_IEEE);
	strcat( bufpoint, "\r\n");
	strcat(bufpoint," CORE_NULL =                  ");
	strcat( bufpoint, NULL_IEEE);
	strcat( bufpoint, "\r\n");
    } }
    else {
      /* ?? TBD */
  } }

  /* only backplanes allowed in TOISIS mode: */
  if (backunit<0)
    strcat(bufpoint," SUFFIX_ITEMS = (0,0,0)\r\n");
  else {
    if (!strcmp(forg,"BSQ")) sprintf( line," SUFFIX_ITEMS = (0,0,%d)\r\n",n3suf);
    else sprintf( line," SUFFIX_ITEMS = (0,%d,0)\r\n",n2suf);
    strcat( bufpoint, line);
    /* and they must be REAL: */
    sprintf( line," SUFFIX_ITEM_TYPE = %s_REAL\r\n",dtype);
    strcat( bufpoint, line);

    /* SUFFIX stuff for CMM cocube planes: */
    if (cocub) {
      if (nbkpln>=9) {
        strcat(bufpoint," BAND_SUFFIX_NAME = (LATITUDE,LONGITUDE,INCIDENCE_ANGLE,\r\n  ");
        strcat( bufpoint, "EMISSION_ANGLE,PHASE_ANGLE,SLANT_DISTANCE,INTERCEPT_ALTITUDE,\r\n  ");
        if (v.sdgeo==3) strcpy(geoplane,"INCIDENCE_ANGLE");
        else if (v.sdgeo==4) strcpy(geoplane,"EMISSION_ANGLE");
        else if (v.sdgeo==5) strcpy(geoplane,"PHASE_ANGLE");
        else if (v.sdgeo==6) strcpy(geoplane,"SLANT_DISTANCE");
        else if (v.sdgeo==7) strcpy(geoplane,"INTERCEPT_ALTITUDE");
        sprintf(line,"%s_STD_DEV,%s_STD_DEV",geoplane,v.dataname);
        strcat(bufpoint,line);
        if (vcmm_flag && nbkpln==11) strcat(bufpoint,",FRAME_LINE,FRAME_SAMPLE");
        strcat(bufpoint,")\r\n");
        strcat(bufpoint," BAND_SUFFIX_UNIT = (DEGREE,DEGREE,DEGREE,DEGREE,DEGREE");
        strcat(bufpoint,",KILOMETER,\r\n");
        sprintf(line,"  KILOMETER,");
        if (v.sdgeo<6) strcat(line,"DEGREE,");
        else strcat(line,"KILOMETER,");
        sprintf(lineb,"%s",v.dataunits);
        strcat(line,lineb);
        if (vcmm_flag && nbkpln==11) strcat(line,",DIMENSIONLESS,DIMENSIONLESS");
        strcat(line,")\r\n\r\n");
        strcat(bufpoint,line);
      }
      else if (nbkpln==2) {
        strcat(bufpoint," BAND_SUFFIX_NAME = (LATITUDE,LONGITUDE)\r\n");
        strcat(bufpoint," BAND_SUFFIX_UNIT = (DEGREE,DEGREE)\r\n");
      }
      else zmabend("unsupported backplane count");
      write_PDS_aline(bufpoint,'I'," BAND_SUFFIX_ITEM_BYTES",4,nbkpln);
      strcpy(bp_type,dtype);
      strcat(bp_type,"_REAL");
      write_PDS_aline(bufpoint,'C'," BAND_SUFFIX_ITEM_TYPE",bp_type,nbkpln);
      arval = 0.0;
      write_PDS_aline(bufpoint,'F'," BAND_SUFFIX_BASE",&arval,nbkpln);
      arval = 1.0;
      write_PDS_aline(bufpoint,'F'," BAND_SUFFIX_MULTIPLIER",&arval,nbkpln);
      strcat(bufpoint,"\r\n");

      if (!strcmp(hostname,"AXP-VMS")) {
	write_PDS_aline(bufpoint,'C'," BAND_SUFFIX_VALID_MINIMUM ",
	 VALID_MIN_VAX,nbkpln);
	write_PDS_aline(bufpoint,'C'," BAND_SUFFIX_LOW_REPR_SAT  ",
	 LOW_REPR_SAT_VAX,nbkpln);
	write_PDS_aline(bufpoint,'C'," BAND_SUFFIX_LOW_INSTR_SAT ",
	 LOW_INSTR_SAT_VAX,nbkpln);
	write_PDS_aline(bufpoint,'C'," BAND_SUFFIX_HIGH_INSTR_SAT",
	 HIGH_INSTR_SAT_VAX,nbkpln);
	write_PDS_aline(bufpoint,'C'," BAND_SUFFIX_HIGH_REPR_SAT ",
	 HIGH_REPR_SAT_VAX,nbkpln);
	write_PDS_aline(bufpoint,'C'," BAND_SUFFIX_NULL          ",
	 NULL_VAX,nbkpln);
      }
      else {
	write_PDS_aline(bufpoint,'C'," BAND_SUFFIX_VALID_MINIMUM ",
	 VALID_MIN_IEEE,nbkpln);
	write_PDS_aline(bufpoint,'C'," BAND_SUFFIX_LOW_REPR_SAT  ",
	 LOW_REPR_SAT_IEEE,nbkpln);
	write_PDS_aline(bufpoint,'C'," BAND_SUFFIX_LOW_INSTR_SAT ",
	 LOW_INSTR_SAT_IEEE,nbkpln);
	write_PDS_aline(bufpoint,'C'," BAND_SUFFIX_HIGH_INSTR_SAT",
	 HIGH_INSTR_SAT_IEEE,nbkpln);
	write_PDS_aline(bufpoint,'C'," BAND_SUFFIX_HIGH_REPR_SAT ",
	 HIGH_REPR_SAT_IEEE,nbkpln);
	write_PDS_aline(bufpoint,'C'," BAND_SUFFIX_NULL          ",
	 NULL_IEEE,nbkpln);
      }

      if (nbkpln==2) {
        strcat(bufpoint,"/* The backplanes contain Latitudes and Longitudes only. */\r\n");
        strcat(bufpoint,"/* Longitude ranges from 0 to 360 degrees, with positive direction West. */\r\n");
        strcat(bufpoint,"/* Latitudes are planetocentric. */\r\n\r\n");
      }
      else {
        strcat(bufpoint,"/* The backplanes contain 7 geometric parameters, the ");
        strcat(bufpoint,"standard deviation */\r\n/* of one of them, and the standard ");
        strcat(bufpoint,"deviation of a selected data band. */\r\n\r\n");
        strcat(bufpoint,"/* Longitude ranges from 0 to 360 degrees, with positive ");
        strcat(bufpoint,"direction */\r\n/* specified by POSITIVE LONGITUDE DIRECTION ");
        strcat(bufpoint,"in the IMAGE MAP PROJECTION */\r\n/*");
        if (pgraphic==1) strcat(bufpoint," group.  Latitudes are planetographic. */\r\n\r\n");
        else if (pgraphic==0) strcat(bufpoint," group.  Latitudes are planetocentric. */\r\n\r\n");
        else strcat(bufpoint,"  group. */\r\n\r\n");

        strcat(bufpoint,"/* SLANT DISTANCE contains the distance from the observer");
        strcat(bufpoint," to the */\r\n/* intercept point of the line of sight with");
        strcat(bufpoint," the target body surface. */\r\n/* Normally, this is the");
        strcat(bufpoint," distance at the time of observation (or */\r\n/* the mean");
        strcat(bufpoint," time, when projected pixels are averaged).  However, */\r\n");
        strcat(bufpoint,"/* in the case of a Perspective projection, the distance is");
        strcat(bufpoint," measured */\r\n/* from the perspective point of the");
        strcat(bufpoint," projection. */\r\n\r\n");

        strcat(bufpoint,"/* INTERCEPT ALTITUDE contains values for the DIFFERENCE ");
        strcat(bufpoint,"between */\r\n/* the length of the normal from the center of ");
        strcat(bufpoint,"the target body to the */\r\n/* line of sight AND the radius of");
        strcat(bufpoint," the target body.  On-target points */\r\n/* have zero values. ");
        strcat(bufpoint," Points beyond the maximum expanded radius have */\r\n");
        strcat(bufpoint,"/* null values.  This plane thus also serves as a set of ");
        strcat(bufpoint,"\"off-limb\" */\r\n/* flags.  It is meaningful only for the ");
        strcat(bufpoint,"ORTHOGRAPHIC and */\r\n/* POINT PERSPECTIVE projections; other");
        strcat(bufpoint,"wise all values are zero. */\r\n\r\n");

        sprintf(line," STD_DEV_SELECTED_BAND_NUMBER = %d\r\n", v.sdband);
        strcat(bufpoint,line);
        sprintf(line," STD_DEV_SELECTED_BACKPLANE = %d\r\n\r\n", v.sdgeo);
        strcat(bufpoint,line);
  } } }

	/* Get current date and time -- use date part only
	 * for PRODUCT_CREATION_DATE, then add time for
	 * DATE_TIME (below) */
  lt = time(NULL);
  ptr = localtime(&lt);
  if((*ptr).tm_year>99) {
    i = 1900+(*ptr).tm_year;
    sprintf(tstr,"%04d",i);
  }
  else
    sprintf(tstr,"19%02d",(*ptr).tm_year);
  sprintf( line, "-%02d-%02d", ((*ptr).tm_mon+1), (*ptr).tm_mday);
  strcat( tstr, line);
  sprintf( line, " PRODUCT_CREATION_DATE = %s\r\n", tstr);
  strcat( bufpoint, line);
  sprintf( line, "T%02d:%02d:%02d", (*ptr).tm_hour, (*ptr).tm_min, (*ptr).tm_sec);
  strcat(tstr,line);	/* to be used below */

  /* The following items are required by VMS ISIS:
   * (but also turn out to be useful for scaled data) */
  sprintf( line," CORE_BASE = 0.0\r\n CORE_MULTIPLIER = %f\r\n", core_mult);
  strcat( bufpoint, line);
  strcat(bufpoint,"/* Core scaling is:  True_value = base + (multiplier *");
  strcat(bufpoint," stored_value) */\r\n");
  strcat(bufpoint," SUFFIX_BYTES = 4\r\n");

  strcat(bufpoint,"\r\n");

  if (!htask) goto map_lab;

strcat(bufpoint,"/*  Data description: general */\r\n\r\n");

  if (!strncmp( v.proj_name, "CAS",3))
    strcat(bufpoint," MISSION_NAME = \"CASSINI-HUYGENS\"\r\n");
  else {
    sprintf(line," MISSION_NAME = %s\r\n",v.proj_name);
    strcat(bufpoint,line);
  }
  sprintf(line," MISSION_PHASE_NAME = %s\r\n",v.phase_name);
  strcat(bufpoint,line);
  sprintf(line," TARGET_NAME = %s\r\n",v.target);
  strcat(bufpoint,line);

	/* ISIS "INSTRUMENT_NAME" is too verbose for Vicar, we just
	 * carry the ID ... */
  sprintf(line," INSTRUMENT_ID = %s\r\n",v.ins_name);
  strcat(bufpoint,line);

  if (!strncmp( v.proj_name, "CAS",3)) {
    sprintf(line," NATIVE_START_TIME = \"%d.%05d\"\r\n",
     v.native_times[0][0],v.native_times[0][1]);
    strcat(bufpoint,line);
    sprintf(line," NATIVE_STOP_TIME = \"%d.%05d\"\r\n\r\n", 
     v.native_times[1][0],v.native_times[1][1]);
    strcat(bufpoint,line);
  }
  else if (!strncmp( v.proj_name, "GAL",3)) {
    sprintf(line," NATIVE_START_TIME = \"%d.%02d.%d\"\r\n",
     v.native_times[0][0],v.native_times[0][1],v.native_times[0][2]);
    strcat(bufpoint,line);
    sprintf(line," NATIVE_STOP_TIME = \"%d.%02d\"\r\n\r\n", 
     v.native_times[1][0],v.native_times[1][1]);
    strcat(bufpoint,line);
  }

	/* write START_TIME after NATIVE_START_TIME, per the
	 * special code in TOVICAR  mode ... */
  if (v.event_start_time[0] != 0) {
    sprintf(line," START_TIME = \"%s\"\r\n",v.event_start_time);	
    strcat(bufpoint,line);
    sprintf(line," STOP_TIME = \"%s\"\r\n",v.event_stop_time);
    strcat(bufpoint,line); 
  }

  sprintf(line," OBSERVATION_NAME = \'%s\'\r\n", v.obsname);
  strcat(bufpoint,line);

  /* OBSNOTE ... VICAR replaces single
   * quotes (') with pairs of same, but we don't need this, so undo it ... */
  x = strlen(v.obsnote);
  y = 0;
  for (i=0; i<x; i++) {
    if (!strncmp( &v.obsnote[i], "''", 2)) {
      strcpy( obuf, &v.obsnote[i+1]);
      strcpy( &v.obsnote[i], obuf);
      x--;  
    }
  }
  y = 0;			/* current position in obsnote */
  if (x < 68) {
    sprintf(line," NOTE = \"%s\"\r\n", v.obsnote);
    strcat(bufpoint,line);
  }
  else {
    strncpy( line," NOTE = \"", 9);
    /* back up if a word is split at EOL: */
    for (ibk=0; ibk<66; ibk++) {
      if (v.obsnote[65-ibk] == ' ' || v.obsnote[66-ibk] == ' ') break;
    }
    if (ibk>65) {
      zifmessage(" error parsing NOTE");
      ibk = 0;
    }
    strncpy( &line[9], v.obsnote, 66-ibk);
    strcpy( &line[75-ibk], "\r\n");
    strcat(bufpoint,line);
    x -= 66-ibk;
    y += 66-ibk;
    for (; x>0;) {
      strncpy( line,"  ", 2);
      if (x <= 74) {
        strncpy( &line[2], &v.obsnote[y], x);
        strcpy( &line[x+2], "\"\r\n");
        x = -1;
      }
      else {
        /* back up if a word is split at EOL: */
        for (ibk=0; ibk<72; ibk++) {
          if (v.obsnote[y+71-ibk] == ' ' || v.obsnote[y+72-ibk] == ' ') break;
        }
        if (ibk>71) {
          zifmessage(" error parsing NOTE");
          ibk = 0;
        }
        strncpy( &line[2], &v.obsnote[y], 72-ibk);
        strcpy( &line[74-ibk], "\r\n");
        x -= 72-ibk;
        y += 72-ibk;
      }
      strcat(bufpoint,line);
    }
  }

  if (!maplabs) goto bandbins;

  sprintf(line," INCIDENCE_ANGLE = %5.2f\r\n",v.inci_angle);
  strcat(bufpoint,line);
  sprintf(line," EMISSION_ANGLE = %5.2f\r\n",v.emis_angle);
  strcat(bufpoint,line);
  sprintf(line," PHASE_ANGLE = %5.2f\r\n",v.phas_angle);
  strcat(bufpoint,line);
  sprintf(line," SOLAR_AZIMUTH = %5.2f\r\n",v.azimuth_sun);
  strcat(bufpoint,line);
  sprintf(line," SUB_SPACECRAFT_AZIMUTH = %5.2f\r\n",v.azimuth_sc);
  strcat(bufpoint,line);
  sprintf(line," START_SUB_SPACECRAFT_LATITUDE = %5.2f\r\n",v.b_ssc_lat);
  strcat(bufpoint,line);
  sprintf(line," START_SUB_SPACECRAFT_LONGITUDE = %5.2f\r\n",v.b_ssc_lon);
  strcat(bufpoint,line);
  sprintf(line," STOP_SUB_SPACECRAFT_LATITUDE = %5.2f\r\n",v.e_ssc_lat);
  strcat(bufpoint,line);
  sprintf(line," STOP_SUB_SPACECRAFT_LONGITUDE = %5.2f\r\n",v.e_ssc_lon);
  strcat(bufpoint,line);
  sprintf(line," START_SUB_SOLAR_LATITUDE = %5.2f\r\n",v.b_ssl_lat);
  strcat(bufpoint,line);
  sprintf(line," START_SUB_SOLAR_LONGITUDE = %5.2f\r\n",v.b_ssl_lon);
  strcat(bufpoint,line);
  sprintf(line," STOP_SUB_SOLAR_LATITUDE = %5.2f\r\n",v.e_ssl_lat);
  strcat(bufpoint,line);
  sprintf(line," STOP_SUB_SOLAR_LONGITUDE = %5.2f\r\n",v.e_ssl_lon);
  strcat(bufpoint,line);
  sprintf(line," MINIMUM_SLANT_DISTANCE = %9.2f\r\n",v.min_range);
  strcat(bufpoint,line);
  sprintf(line," MAXIMUM_SLANT_DISTANCE = %9.2f\r\n",v.max_range);
  strcat(bufpoint,line);
  sprintf(line," MIN_SPACECRAFT_SOLAR_DISTANCE = %g\r\n",v.min_sun_d);
  strcat(bufpoint,line);
  sprintf(line," MAX_SPACECRAFT_SOLAR_DISTANCE = %g\r\n",v.max_sun_d);
  strcat(bufpoint,line);
  sprintf(line," MINIMUM_CENTRAL_BODY_DISTANCE = %9.2f\r\n",v.min_cb_d);
  strcat(bufpoint,line);
  sprintf(line," MAXIMUM_CENTRAL_BODY_DISTANCE = %9.2f\r\n\r\n",v.max_cb_d);
  strcat(bufpoint,line);

bandbins:
  strcat(bufpoint," GROUP = BAND_BIN\r\n\r\n");
  strcat(bufpoint,"/*  Spectral axis description */\r\n\r\n");
  write_PDS_line(bufpoint,'I',"  BAND_BIN_ORIGINAL_BAND",orbands,nb,4);
  write_PDS_line(bufpoint,'F',"  BAND_BIN_CENTER",waves,nb,4);
  strcat(bufpoint,"  BAND_BIN_UNIT = MICROMETER\r\n");
  strcat(bufpoint," END_GROUP = BAND_BIN\r\n\r\n");

map_lab:
  if (!maplabs) goto endqub;

  strcat(bufpoint," GROUP = IMAGE_MAP_PROJECTION\r\n");
  strcat(bufpoint,"/* Projection description */\r\n");
  if (zvptst("FAKEPROJ")) {
    sprintf(line,"  MAP_PROJECTION_TYPE = %s\r\n","MERCATOR");
    strcat(bufpoint,line);
    sprintf(line,"/* Above is a fake to fool ISIS, actual projection is %s */\r\n",v.projection);
    strcat(bufpoint,line);
  }
  else {
    sprintf(line,"  MAP_PROJECTION_TYPE = %s\r\n",v.projection);
    strcat(bufpoint,line);
  }
  sprintf(line,"  MAP_SCALE = %6.3f\r\n",v.scale);/* Map scale in KM/PIXEL  */
  strcat(bufpoint,line);

  if ( v.scale != 0 )
    pixelsperdegree = (2.0 * 3.14159 * v.radii[0])/(360.0 * v.scale);
  else
    pixelsperdegree = 0;
  sprintf(line,"  MAP_RESOLUTION = %6.3f\r\n",pixelsperdegree);
  strcat(bufpoint,line);

	/* any items obtained from the VICAR "MAP00x" labels must be
	 * corrected to planetographic lat. if requested */

 if (strcmp(v.projection,"POINT_PERSPECTIVE")==0) {
    sprintf(line,"  SUB_SPACECRAFT_LATITUDE = %6.2f\r\n",v.ssc_lat);
    strcat(bufpoint,line);
    sprintf(line,"  SUB_SPACECRAFT_LONGITUDE = %6.2f\r\n",v.ssc_lon);
    strcat(bufpoint,line);
	/* subtract 1 because PDS definition is relative to (1,1) &
	 * switch signs for Unix ISIS ...  */
    sav = 1.-v.ssc_line;
    sprintf(line,"  LINE_SUB_SPACECRAFT_OFFSET = %6.2f\r\n",sav);
    strcat(bufpoint,line);
    sav = 1.-v.ssc_samp;
    sprintf(line,"  SAMPLE_SUB_SPACECRAFT_OFFSET = %6.2f\r\n",sav);
    strcat(bufpoint,line);
    sprintf(line,"  TARGET_CENTER_DISTANCE = %.1f\r\n",v.range);
    strcat(bufpoint,line);
    sav = 1.-v.oaline;
    sprintf(line,"  LINE_OPTICAL_AXIS_OFFSET = %6.2f\r\n",sav);
    strcat(bufpoint,line);
    sav = 1.-v.oasamp;
    sprintf(line,"  SAMPLE_OPTICAL_AXIS_OFFSET = %6.2f\r\n",sav);
    strcat(bufpoint,line);

	/* for POV, scale is in pixels/mm in focal plane, and
	 * the focal length in mm is also required: */
    sprintf(line,"  FOCAL_LENGTH = %5.1f\r\n",v.focal);
    strcat(bufpoint,line);
    sprintf(line,"  FOCAL_PLANE_SCALE = %6.3f\r\n",v.cscale);
    strcat(bufpoint,line);
  }
  else {

    sprintf(line,"  CENTER_LATITUDE = %6.2f\r\n",v.center_lat);
    strcat(bufpoint,line);
    sprintf(line,"  CENTER_LONGITUDE = %6.2f\r\n",v.center_lon);
    strcat(bufpoint,line);

    /* subtract 1 because PDS definition is relative to (1,1), and
     * switch signs for Unix ISIS ...  */
    sav = 1. - v.center_line;
    sprintf(line,"  LINE_PROJECTION_OFFSET = %6.2f\r\n",sav);
    strcat(bufpoint,line);
    sav = 1. - v.center_samp;
    sprintf(line,"  SAMPLE_PROJECTION_OFFSET = %6.2f\r\n",sav);
    strcat(bufpoint,line);
  }

  /* need OFFSET_DIRECTION keyword to distinguish new labels from old */
  strcat( bufpoint, "  OFFSET_DIRECTION = TO_ORIGIN\r\n");

  sprintf(line,"  MINIMUM_LATITUDE = %6.2f\r\n",v.min_lat);
  strcat(bufpoint,line);
  sprintf(line,"  MAXIMUM_LATITUDE = %6.2f\r\n",v.max_lat);
  strcat(bufpoint,line);

  if (v.min_lon > v.max_lon) {	/* just in case someone screwed up */
    sav = v.min_lon;
    v.min_lon = v.max_lon;
    v.max_lon = sav;
  }
  sprintf(line,"  MINIMUM_LONGITUDE = %6.2f\r\n",v.min_lon);
  strcat(bufpoint,line);
  sprintf(line,"  MAXIMUM_LONGITUDE = %6.2f\r\n",v.max_lon);
  strcat(bufpoint,line);

  /* West longitudes assumed */
  sprintf(line,"  EASTERNMOST_LONGITUDE = %6.2f\r\n",v.min_lon);
  strcat(bufpoint,line);
  sprintf(line,"  WESTERNMOST_LONGITUDE = %6.2f\r\n",v.max_lon);
  strcat(bufpoint,line);


  strcat(bufpoint,"  COORDINATE_SYSTEM_TYPE = \"BODY-FIXED ROTATING\"\r\n");
  if (pgraphic==1)
    strcat(bufpoint,"  COORDINATE_SYSTEM_NAME = PLANETOGRAPHIC\r\n");
  else if (pgraphic==0)
    strcat(bufpoint,"  COORDINATE_SYSTEM_NAME = PLANETOCENTRIC\r\n");
  if ( strcmp(v.target,"VENUS")==0 )
    strcat(bufpoint,"  POSITIVE_LONGITUDE_DIRECTION = EAST\r\n");
  else
    strcat(bufpoint,"  POSITIVE_LONGITUDE_DIRECTION = WEST\r\n");

  sprintf(line,"  A_AXIS_RADIUS = %.2f\r\n",v.radii[0]); /* A, B, C axes */
  strcat(bufpoint,line);
  sprintf(line,"  B_AXIS_RADIUS = %.2f\r\n",v.radii[1]);
  strcat(bufpoint,line);
  sprintf(line,"  C_AXIS_RADIUS = %.2f\r\n",v.radii[2]);
  strcat(bufpoint,line);

  if( v.projection[0] == 'L' ) {
    sprintf(line,"  FIRST_STANDARD_PARALLEL = %.3f\r\n",v.projitem1);
    strcat(bufpoint,line);
    sprintf(line,"  SECOND_STANDARD_PARALLEL = %.3f\r\n",v.projitem2);
    strcat(bufpoint,line);
  }

  if( v.projection[4] == 'Q' ) {
    sprintf(line,"  REFERENCE_LATITUDE = %.3f\r\n",v.projitem1);
    strcat(bufpoint,line);
    sprintf(line,"  REFERENCE_LONGITUDE = %.3f\r\n",v.projitem2);
    strcat(bufpoint,line);
  }
  if( strcmp(v.projection,"POLAR_ORTHOGRAPHIC")==0 ||
   strcmp(v.projection,"POINT_PERSPECTIVE")==0 ||
   strcmp(v.projection,"OBLIQUE_ORTHOGRAPHIC")==0 ||
   strcmp(v.projection,"OBLIQUE_STEREOGRAPHIC")==0 ) {
    sprintf(line,"  MAP_PROJECTION_ROTATION = %6.2f\r\n",v.rotation);
    strcat(bufpoint,line);
  }
  strcat(bufpoint,"  SAMPLE_FIRST_PIXEL = 1\r\n");
  sprintf(line,"  SAMPLE_LAST_PIXEL = %d\r\n",ns);
  strcat(bufpoint,line);
  strcat(bufpoint,"  LINE_FIRST_PIXEL = 1\r\n");
  sprintf(line,"  LINE_LAST_PIXEL = %d\r\n\r\n",inl);
  strcat(bufpoint,line);

  strcat(bufpoint," END_GROUP = IMAGE_MAP_PROJECTION\r\n\r\n");

endqub:
  strcat(bufpoint,"END_OBJECT = QUBE\r\nEND\r\n");

	/* Generate history obj buff*/
  histpoint = (char *)calloc(HISTMEMORY,sizeof( char )); 
  if( histpoint==NULL )
    zmabend(" Memory allocation error for history object");

  /* check if we need a group for VIMSCMM: */
/*  TBD ...
  if (vcmm_flag) {
*/

  if (histfile) {
    hfile = fopen(hfilnam,"r");
    /* just copy all bytes to our buffer: */
    hpoint = histpoint;
    for (;;) {
      vstat = fscanf(hfile, "%c", &cc);
      if (vstat==EOF) break;
      *(hpoint++) = cc;
    }
    fclose(hfile);
    for (x=0; x<8; x++) {
      if (!strncmp(hpoint-x,"END",3)) break;
    }
    if (x>=7) zmabend(" error reading History file");
    hpoint -= x;	/* remove the final "END" line */
    *hpoint = 0;
    strcat(histpoint,"\r\n\r\nGROUP = VISISX\r\n\r\n");
  }
  else
    strcpy(histpoint," GROUP = VISISX\r\n");
    /* 1st byte seems to get lost, hence the blank ... */

  sprintf(line,"  VERSION_DATE = %s\r\n", verdat);
  strcat(histpoint,line);
  sprintf(line,"  DATE_TIME = %s\r\n",tstr);
  strcat(histpoint,line);
  strcat(histpoint,"  NODE_NAME = \"MIPL\"\r\n");
  if (zvptst("VICLAB")) {
    y = 0;
    for (i=0; i<viclabsize; i++) {
      if (!strncmp( &viclab[i], "TASK=", 5)) {
        y = z = i;
        break;
    } }
    if (y) {
      strcat(histpoint,"  VICAR_HISTORY = (\"");
      /* break up Vicar history label into comma-separated list of
       * keyword = value items -- all spaces are interpreted as item
       * delimiters and removed, except for when inside a '-delimited
       * string */
      x=strlen(histpoint);
      y1 = 0;		/* length of substring to write out */
      quoted = 0;	/* flag that we're in a quoted string */
      delim = 0;	/* flag that we're in a delimiter space */
      for (i=z; i<viclabsize; i++) {
	if (viclab[i]=='\'') quoted = 1-quoted;
	if (quoted && delim) {	/* this should never happen */
	  zmabend(" error processing vicar label!");
	}
	if (quoted) {
	  y1++;
	  continue;
	}
	if (viclab[i]==' ') delim = 1;
	else {
	  if (delim) {	/* at end of delimiter area -- write out item, start next */
            strncpy(&histpoint[x],&viclab[y],y1);
            strncpy(&histpoint[x+y1],"\",\r\n    \"",9);
	    x += y1+9;
	    y = i;
	    y1 = 0;
	    delim = 0;
	  }
	  /* special case:  if a comma occurs and the item is >60 bytes long,
	   * write it out and start new line */
	  else if (viclab[i]==',' && y1>60) {
            strncpy(&histpoint[x],&viclab[y],y1);
            strncpy(&histpoint[x+y1],",\r\n      ",9);
	    x += y1+9;
	    y = i+1;
	    y1 = -1;	/* skip the comma itself */
	  }
	  y1++;
      } }
      /* last item */
      strncpy(&histpoint[x],&viclab[y],y1);
      strncpy(&histpoint[x+y1],"\")\r\n\0",4);
  } }

  if (!histfile) {
    strcat(histpoint,"  GROUP = PARAMETERS\r\n");

    sprintf(line,"    VICAR_FILE_NAME = \"%s\"\r\n", cubefile);
    strcat(histpoint,line);
    strcat(histpoint,"  END_GROUP = PARAMETERS\r\n");
  }
  strcat(histpoint,"END_GROUP = VISISX\r\n\r\nEND\r\n");

  x=strlen(histpoint);
  additionalbytes = BUFFERSIZE - x%BUFFERSIZE + HST_BLANK_RECORDS*BUFFERSIZE;
  for (y=0;y<additionalbytes;y++)
    strcat(histpoint," ");
  x=strlen(histpoint);
  if (x > HISTMEMORY) zmabend("*** history label too big ***");

  *nhistrecs = x/BUFFERSIZE;		/* Calc # of history records */

  labelbytes = strlen(bufpoint);
  additionalbytes = BUFFERSIZE - labelbytes%BUFFERSIZE 
		  + LBL_BLANK_RECORDS*BUFFERSIZE;
  for (y=0;y<additionalbytes;y++)
    strcat(bufpoint," ");
  labelbytes = strlen(bufpoint);	/* Calc # of label records   	*/
  if (labelbytes > LABELMEMORY) zmabend("*** PDS label too big ***");

  *nlabrecs  = labelbytes/BUFFERSIZE;

  /* now that we know the header size, fill in the missing XXXXX items
   * in the label ... */

  *nrecs += (*nlabrecs + *nhistrecs);	/* Update file record total	*/

					/* OBJECT FILE POINTERS    	*/
  sprintf(value[0],"%6d",*nrecs);	/* Number of records in file	*/
  sprintf(value[1],"%6d",*nlabrecs);	/* Number of label records	*/
  sprintf(value[2],"%6d",*nlabrecs+1);	/* Pointer to history object	*/
					/* Pointer to cube:	*/
  sprintf(value[3],"%6d",*nlabrecs+*nhistrecs+1);
  z = NUMOFPTRS;

  count = bufptr = 0;			/* Place POINTERS and above 	*/
  while( count < z ) {			/* values in PDS label		*/
    if (bufpoint[bufptr]=='\r')
      if (strncmp(&bufpoint[bufptr-6],"XXXXXX",6)==0 )
        strncpy(&bufpoint[bufptr-6],value[count++],6);
    bufptr++;
  }
  /* Replace FILE DIRTY with FILE CLEAN	*/
  count = bufptr = 0;
  while (count == 0 && bufptr < 200 ) {
    if (bufpoint[bufptr]=='\r')
      if(strncmp(&bufpoint[bufptr-5],"DIRTY",5)==0) {
        strncpy(&bufpoint[bufptr-5],"CLEAN",5);
        count = 1;
      }
    bufptr++;
  }
}


/****************************************************************************/
FUNCTION determine_month( month, string )
/*
 * Determine month number string from month name.
 */
char	month[],
	string[];
{

switch( string[0] )	
	{
	case 'J': 	if( string[1] == 'A' )
				strcpy(month,"01");
			else
				if( string[2] == 'N' )
					strcpy(month,"06");
				else
					strcpy(month,"07");
			break;

	case 'F':	strcpy(month,"02");
			break;	

	case 'M':	if( string[2] == 'R' )
				strcpy(month,"03");
			else
				strcpy(month,"05");
			break;	

	case 'A':	if( string[1] == 'P' )
				strcpy(month,"04");
			else
				strcpy(month,"08");
			break;	

	case 'S':	strcpy(month,"09");
			break;	

	case 'O':	strcpy(month,"10");
			break;	

	case 'N':	strcpy(month,"11");
			break;	

	case 'D':	strcpy(month,"12");
			break;	

	/* otherwise, assume input is numeric */
	default:	strncpy(month,&string[1],2);
			month[2] = '\0';
			break;	
	}
}


/****************************************************************************/
FUNCTION fill_if_error(string,type)
/*
 * Replace string with " " if status of VICAR zlget is < 1; check for
 * blank strings and substitute ' ' or " ".
 */
char string[],type[];
{
  int x, y;

  if (vstat<=0)
    strcpy(string," ");
  else {
    x = strlen(string);
    for( y=0; y<x; y++ )
      if( string[y] != ' ' )
	y=x+1;
    if(y==x) {
      if ( type[0] == 's' || type[0] == 'S' )
	strcpy(string," ");
      else {
        if ( type[0] == 'l' || type[0] == 'L' )
	  strcpy(string,"' '");
	else
	  strcpy(string,"xxxxxx");
      }
    }
  }
}


/*****************************************************************************/
int FUNCTION find_keyword(keyword,buf,labptr,endptr)
/*
 * Find the keyword and prepare for reading its value 
 */
char	keyword[],buf[];			/* Item to be found          */
int	*labptr;				/* Pointer to label element  */
int	*endptr;				/* end of label to search    */
{						/* Local Variables           */
int	count,					/* Counter/index             */
	found,					/* Item found indicator      */
	keylength,				/* Item name length	     */	
	startptr;				/* Value passed as labptr  */

startptr  = *labptr;				/* Record starting pointer   */
found     = FALSE;				/* Item not found yet        */
keylength = strlen(keyword);			/* Determine itemname length */
while( !found && *labptr < *endptr ) {
  count  = 0;
  while( buf[*labptr] != keyword[count] )
    if ( *labptr >= *endptr || incrm_check_buffer( labptr ) == 0 ) return 0;
  if ( *labptr >= *endptr || incrm_check_buffer( labptr ) == 0 ) return 0;
  count++;

  while( buf[*labptr] == keyword[count] && count < keylength )
    if ( *labptr >= *endptr || incrm_check_buffer( labptr ) == 0 ) return 0;
    else count++;

  if( count == keylength && ( buf[*labptr] == ' ' || buf[*labptr] == '=' ||
   buf[*labptr] == '\r' ) )
    found = TRUE;
}

return 1;
}


/*****************************************************************************/
int FUNCTION find_keyword_str(keyword,buf,labptr,endptr)
/*
 * A special version of find_keyword for cases where the starting quote (")
 * has been included in the string in order to guarantee that a character
 * string value is found (for cases of ambiguous keywords such as START_TIME)
 */
char	keyword[],buf[];			/* Item to be found          */
int	*labptr;				/* Pointer to label element  */
int	*endptr;				/* end of label to search    */
{						/* Local Variables           */
int	count,					/* Counter/index             */
	found,					/* Item found indicator      */
	keylength,				/* Item name length	     */	
	startptr;				/* Value passed as labptr  */

startptr  = *labptr;				/* Record starting pointer   */
found     = FALSE;				/* Item not found yet        */
keylength = strlen(keyword);			/* Determine itemname length */
while( !found && *labptr < *endptr ) {
  count  = 0;
  while( buf[*labptr] != keyword[count] )
    if ( *labptr >= *endptr || incrm_check_buffer( labptr ) == 0 ) return 0;
  if ( *labptr >= *endptr || incrm_check_buffer( labptr ) == 0 ) return 0;
  count++;

  while( buf[*labptr] == keyword[count] && count < keylength )
    if ( *labptr >= *endptr || incrm_check_buffer( labptr ) == 0 ) return 0;
    else count++;

  if( count == keylength && buf[(*labptr)-1] == '\"' ) {
    found = TRUE;
    (*labptr)--;
  }
}

return 1;
}


/****************************************************************************/
int FUNCTION get_integer_value(intitem,buf,labptr)
/*
 * Get integer value.	
 */
int	*intitem,				/* Integer value pointer     */
	*labptr;				/* Label element pointer     */
char 	buf[];					/* PDS label buffer	     */

{						/* Local Variables           */
  char	integer[12];				/* String for integer        */
  int 	count, sign;

  count = 0;
  sign = 1;		/* initialize as positive */
  memset( integer, 0, 12 );

	/* Find first digit/sign */
  while (!isdigit(buf[*labptr]) && buf[*labptr] != '+' &&
   buf[*labptr] != '-') {
    lsef = incrm_check_buffer(labptr);
    if (lsef == 0) return 0;
  }
  if (buf[*labptr]=='-') sign = -1;

	/* space past sign: */
  if (buf[*labptr]=='-' || buf[*labptr]=='+') {
    lsef = incrm_check_buffer(labptr);
    if (lsef == 0) return 0;
  }
 
	/* Continue until last digit is found */
  while (isdigit(buf[*labptr])) {
    integer[count++]=buf[*labptr];
    lsef = incrm_check_buffer(labptr);
    if (lsef == 0) return 0;
  }

	/* Convert string into INT */
  *intitem = sign*atoi(integer);
  return 1;
}

/****************************************************************************/
FUNCTION get_label_items()
/*
 * Get all VICAR label items needed for PDS label and complete all 
 * necessary format conversions.
 */
{
#define TSTR_LEN 80
  char monthstr[3], tstr[TSTR_LEN],lattyp[15], pos_lon[6];
  char borg[8];			/* backplane (cocube) org */
  float dumflt, latlon[2], matbuf[9], sav;
  double xlin, xsam, xlat, xlon;
  int area[4], bnl, bns, cnt, dummy, i, j, x, y, z, nret, nbnd, integers[3];
  MP mpo;
  char task[20][8];  /* VICAR Task names */
  int instances[20], nb1, numtasks;

  /* first open the cube file */
  vstat = zvopen(inunit,"OP","READ","OPEN_ACT","SA","IO_ACT","SA", NULL);

  /* put the entire label into a buffer for the Vicar history item: */
  viclabsize = 0;	/* first check to see how big it is */
  zlgetlabel( inunit, viclab, &viclabsize);
  if (viclabsize > 20000) viclabsize = 20000; 
  zlgetlabel( inunit, viclab, &viclabsize);

  zvget(inunit,"NB",&nb, NULL);

  /* assume map labels present, revise if MP call fails */
  maplabs = 1;

  /* look for map labels, set flag */
  lsef = mpInit( &mpo);
  if (lsef!=mpSUCCESS) zmabend(" error initializing MP object");
  lsef = mpLabelRead( mpo, inunit);
  if (lsef!=mpSUCCESS) {
    maplabs = 0;
    zifmessage(" no map labels found in cube");
    if (!htask) goto closfil;
    goto nonmap;
  }

  lsef = mpGetValues( mpo, mpPOSITIVE_LONGITUDE_DIRECTION, pos_lon, "");

  /* common to all projections: */
  lsef = mpGetValues( mpo, mpA_AXIS_RADIUS, &v.radii[0], "");
  lsef = mpGetValues( mpo, mpB_AXIS_RADIUS, &v.radii[1], "");
  lsef = mpGetValues( mpo, mpC_AXIS_RADIUS, &v.radii[2], "");
  lsef = mpGetValues( mpo, mpMAP_PROJECTION_TYPE, v.projection, "");

  /* ISIS wants Sinusoidal expanded: */
  if (strcmp(v.projection,"SINUSOIDAL") == 0) 
    strcpy( v.projection, "'SINUSOIDAL_EQUAL-AREA'");

  /* projection-specfic stuff: */

  if (strcmp(v.projection,"POINT_PERSPECTIVE")==0) {
    lsef = mpGetValues( mpo, mpSUB_SPACECRAFT_LATITUDE, &v.ssc_lat, "");
    lsef = mpGetValues( mpo, mpSUB_SPACECRAFT_LONGITUDE, &v.ssc_lon, "");
    lsef = mpGetValues( mpo, mpPLANET_CENTER_LINE, &v.ssc_line, "");
    lsef = mpGetValues( mpo, mpPLANET_CENTER_SAMPLE, &v.ssc_samp, "");
    lsef = mpGetValues( mpo, mpSPACECRAFT_DISTANCE, &v.range, "");
    lsef = mpGetValues( mpo, mpOPT_AXIS_INTERCEPT_LINE, &v.oaline, "");
    lsef = mpGetValues( mpo, mpOPT_AXIS_INTERCEPT_SAMPLE, &v.oasamp, "");
    lsef = mpGetValues( mpo, mpFOCAL_LENGTH, &v.focal, "");
    lsef = mpGetValues( mpo, mpFOCAL_PLANE_SCALE, &v.cscale, "");
    lsef = mpGetValues( mpo, mpNORTH_ANGLE, &v.rotation, "");
    if ((v.cscale*v.focal)==0.0) 
      v.scale = 0.0;
    else
      v.scale = (v.range-v.radii[0])/(v.cscale*v.focal);
    /* don't add 1 because the MP items are NOT relative to (1,1) -- in
     * contrast to the label items! */
    /*v.ssc_line++;
    v.ssc_samp++;
    v.oaline++;
    v.oasamp++;*/
  }
  else {
    lsef = mpGetValues( mpo, mpMAP_SCALE, &v.scale, "");
    lsef = mpGetValues( mpo, mpCENTER_LATITUDE, &v.center_lat, "");
    lsef = mpGetValues( mpo, mpCENTER_LONGITUDE, &v.center_lon, "");
    lsef = mpGetValues( mpo, mpLINE_PROJECTION_OFFSET, &v.center_line, "");
    lsef = mpGetValues( mpo, mpSAMPLE_PROJECTION_OFFSET, &v.center_samp, "");
    /* add 1, because these MP items are relative to (1,1) ... will get
     * subtracted out again when we write the ISIS item, but at least we
     * are consistent! */
    v.center_line++;
    v.center_samp++;

    if (!strcmp(v.projection,"OBLIQUE_ORTHOGRAPHIC") ||
        !strcmp(v.projection,"OBLIQUE_STEREOGRAPHIC") ||
        !strcmp(v.projection,"OBLIQUE_SINUSOIDAL")) {
      lsef = mpGetValues( mpo, mpCARTESIAN_AZIMUTH, &v.rotation, "");
    }
    else if (!strcmp(v.projection,"LAMBERT_CONFORMAL"))
      lsef = mpGetValues( mpo, mpFIRST_STANDARD_PARALLEL, &v.projitem1, "");
      lsef = mpGetValues( mpo, mpSECOND_STANDARD_PARALLEL, &v.projitem2, "");
  }

nonmap:

  /* check if VIMSCMM is in the history labels -- normally this should be
   * equivalent to checking TASKNAM, but it's possible for these to differ,
   * so let's be careful;
   * (also, note that currently VIMSCMM output always has a map label, so we
   * could put this code before "nonmap", but this may not always be the case) */
  numtasks = 20;
  vstat = zlhinfo(inunit,(char *)task,instances,&numtasks,"ERR_ACT","SA", NULL);

  for (i=0,vcmm_flag=0;i<numtasks;i++) if (!strncmp(task[i],"VIMSCMM",7)) vcmm_flag=1;

  if (vcmm_flag) { /* gather some useful parameters for PDS History ... */

    vstat = zlget(inunit,"HISTORY","SPKERNLS",spkerns,"HIST","VIMSCMM",
     "ULEN", 101, "NELEMENT", -1, "NRET", &nspk, NULL);

    vstat = zlget(inunit,"HISTORY","CKERNEL",ckern,"HIST","VIMSCMM", NULL);
    icker = (vstat>0);

    vstat = zlget(inunit,"HISTORY","DPOINT",dpoint,"HIST","VIMSCMM",
     "NELEMENT", -1, "NRET", &ndpnt, NULL);

    nepherr = 0;
    vstat = zlget(inunit,"HISTORY","EPHERRSC",&epherr[0],"HIST","VIMSCMM", NULL);
    if (vstat>0) nepherr++;
    vstat = zlget(inunit,"HISTORY","EPHERRTG",&epherr[1],"HIST","VIMSCMM", NULL);
    if (vstat>0) nepherr++;
  }

  if (!htask) goto closfil;

  if (!uwaves) {
    vstat = zlget(inunit,"HISTORY","WAVLNTHS",waves,"NRET",&nb1,
     "NELEMENT",-1,"HIST",tasknam, NULL);
    if (vstat>0 && nb!=nb1) zmabend(" incorrect # of wavelengths!");
  }

  vstat = zlget(inunit,"HISTORY","WAVUNIT",wavunit,"HIST",tasknam, NULL);
  if (vstat<=0) strcpy( wavunit, "MICROMETER");

  /* some ISIS programs require this keyword: */
  vstat = zlget(inunit,"HISTORY","ORIGBANDS",orbands,"NELEMENT",nb,
   "HIST",tasknam, NULL);
  if (vstat<=0) for( j=0; j<nb; j++) orbands[j] = j+1;

  vstat = zlget(inunit,"HISTORY","OBSNOTE",v.obsnote,"HIST",tasknam, NULL);
  fill_if_error(v.obsnote,"String");

  vstat = zlget(inunit,"HISTORY","DATANAME",v.dataname,"HIST",tasknam, NULL);
  if (vstat<=0) strcpy( v.dataname, "UNKNOWN");

  vstat = zlget(inunit,"HISTORY","DATAUNIT",v.dataunits,"HIST",tasknam, NULL);
  if (vstat<=0) strcpy( v.dataunits, "UNKNOWN");

  vstat = zlget(inunit,"HISTORY","MULTIPLIER",&core_mult,"HIST",tasknam, NULL);
  if (vstat<=0) core_mult = 1.0;

  vstat = zlget(inunit,"HISTORY","OBSNAME",v.obsname,"HIST",tasknam, NULL);
  fill_if_error(v.obsname,"String");
  /* ensure that string is zero-terminated: */
  v.obsname[12] = 0; 

  vstat = zlget(inunit,"HISTORY","PROD_ID",v.prod_id,"HIST",tasknam, NULL);
  fill_if_error(v.prod_id,"String");

  vstat = zlget(inunit,"HISTORY","PHASE",v.phase_name,"HIST",tasknam, NULL);
  if (vstat<=0) strcpy( v.phase_name, "UNKNOWN");

  vstat = zlget(inunit,"HISTORY","PROJECT",v.proj_name,"HIST",tasknam, NULL);
  if (vstat<=0) strcpy( v.proj_name, "UNKNOWN");

  vstat = zlget(inunit,"HISTORY","INSTRMNT",v.ins_name,"HIST",tasknam, NULL);
  if (vstat<=0) strcpy( v.ins_name, "UNKNOWN");

  vstat = zlget(inunit,"HISTORY","TARGET",v.target,"HIST",tasknam, NULL);
  if (vstat<=0) strcpy( v.target, "UNKNOWN");

  vstat = zlget(inunit,"HISTORY","PHOT_FNC",v.photofunc,"HIST",tasknam, NULL);
  fill_if_error(v.photofunc,"String"); 
  if( v.photofunc[0]=='L' && v.photofunc[1]=='A' ) {
    strcpy( v.photofunc,"LAMBERT");
  }
  else if( v.photofunc[0]=='L' && v.photofunc[1]=='O' ) {
    strcpy( v.photofunc,"\'LOMMEL-SEELIGER\'");
  }
  else if( v.photofunc[0] == 'M' ) {
    strcpy( v.photofunc,"MINNAERT");
    vstat = zlget(inunit,"HISTORY","MINN_EXP",&v.minn_exp,"HIST",tasknam, NULL);
  }

  if (v.radii[0]==v.radii[2] || v.radii[0]<0.0 ) pgraphic = -1;
  else {
    vstat = zlget(inunit,"HISTORY","LATITUDE_TYPE",lattyp,"HIST",tasknam, NULL);
    if (vstat<=0)
     vstat = zlget(inunit,"HISTORY","LAT_TYPE",lattyp,"HIST",tasknam, NULL);
    if (!strcmp(lattyp,"PLANETOGRAPHIC")) pgraphic = 1;
    else pgraphic = 0;
  }

  vstat = zlget(inunit,"HISTORY","MAXRANGE",&v.max_range,"HIST",tasknam, NULL);
  zero_if_error(&v.max_range);

  vstat = zlget(inunit,"HISTORY","MINRANGE",&v.min_range,"HIST",tasknam, NULL);
  zero_if_error(&v.min_range);

  vstat = zlget(inunit,"HISTORY","MAXLAT",&v.max_lat,"HIST",tasknam, NULL);
  zero_if_error(&v.max_lat);

  vstat = zlget(inunit,"HISTORY","MAXLON",&v.max_lon,"HIST",tasknam, NULL);
  zero_if_error(&v.max_lon);

  vstat = zlget(inunit,"HISTORY","MINLAT",&v.min_lat,"HIST",tasknam, NULL);
  zero_if_error(&v.min_lat);

  vstat = zlget(inunit,"HISTORY","MINLON",&v.min_lon,"HIST",tasknam, NULL);
  zero_if_error(&v.min_lon);

  vstat = zlget(inunit,"HISTORY","MAXSUN_D",&v.max_sun_d,"HIST",tasknam, NULL);
  zero_if_error(&v.max_sun_d);

  vstat = zlget(inunit,"HISTORY","MINSUN_D",&v.min_sun_d,"HIST",tasknam, NULL);
  zero_if_error(&v.min_sun_d);

  vstat = zlget(inunit,"HISTORY","MAX_CB_D",&v.max_cb_d,"HIST",tasknam, NULL);
  zero_if_error(&v.max_cb_d);

  vstat = zlget(inunit,"HISTORY","MIN_CB_D",&v.min_cb_d,"HIST",tasknam, NULL);
  zero_if_error(&v.min_cb_d);

  /* Calculate slant_dist_mult & slant_dist_base  */
  v.slant_dist_mult = (v.max_range - v.min_range)/(32767+32752);
  v.slant_dist_base = v.max_range - (v.slant_dist_mult*32767);

  vstat = zlget(inunit,"HISTORY","SUNAZ",&v.azimuth_sun,"HIST",tasknam, NULL);
  zero_if_error(&v.azimuth_sun);
  if (v.azimuth_sun<0.0) v.azimuth_sun += 360.;  /* PDS standard 0-360 */

  vstat = zlget(inunit,"HISTORY","SCAZ",&v.azimuth_sc,"HIST",tasknam, NULL);
  zero_if_error(&v.azimuth_sc);
  if (v.azimuth_sc<0.0) v.azimuth_sc += 360.;  /* PDS standard 0-360 */

  vstat = zlget(inunit,"HISTORY","B_SSCLAT",&v.b_ssc_lat,"HIST",tasknam, NULL);
  zero_if_error(&v.b_ssc_lat);

  vstat = zlget(inunit,"HISTORY","B_SSCLON",&v.b_ssc_lon,"HIST",tasknam, NULL);
  zero_if_error(&v.b_ssc_lon);
  if (v.b_ssc_lon<0.0) v.b_ssc_lon += 360.;  /* PDS standard 0-360 */

  vstat = zlget(inunit,"HISTORY","E_SSCLAT",&v.e_ssc_lat,"HIST",tasknam, NULL);
  zero_if_error(&v.e_ssc_lat);

  vstat = zlget(inunit,"HISTORY","E_SSCLON",&v.e_ssc_lon,"HIST",tasknam, NULL);
  zero_if_error(&v.e_ssc_lon);
  if (v.e_ssc_lon<0.0) v.e_ssc_lon += 360.;  /* PDS standard 0-360 */

  vstat = zlget(inunit,"HISTORY","B_SSLLAT",&v.b_ssl_lat,"HIST",tasknam, NULL);
  zero_if_error(&v.b_ssl_lat);

  vstat = zlget(inunit,"HISTORY","B_SSLLON",&v.b_ssl_lon,"HIST",tasknam, NULL);
  zero_if_error(&v.b_ssl_lon);
  if (v.b_ssl_lon<0.0) v.b_ssl_lon += 360.;  /* PDS standard 0-360 */

  vstat = zlget(inunit,"HISTORY","E_SSLLAT",&v.e_ssl_lat,"HIST",tasknam, NULL);
  zero_if_error(&v.e_ssl_lat);

  vstat = zlget(inunit,"HISTORY","E_SSLLON",&v.e_ssl_lon,"HIST",tasknam, NULL);
  zero_if_error(&v.e_ssl_lon);
  if (v.e_ssl_lon<0.0) v.e_ssl_lon += 360.;  /* PDS standard 0-360 */

  vstat = zlget(inunit,"HISTORY","INCI_ANG",&v.inci_angle,"HIST",tasknam, NULL);
  zero_if_error(&v.inci_angle);

  vstat = zlget(inunit,"HISTORY","PHAS_ANG",&v.phas_angle,"HIST",tasknam, NULL);
  zero_if_error(&v.phas_angle);

  vstat = zlget(inunit,"HISTORY","EMIS_ANG",&v.emis_angle,"HIST",tasknam, NULL);
  zero_if_error(&v.emis_angle);

  memset(tstr, '\0', TSTR_LEN);
  vstat = zlget(inunit,"HISTORY","BEG_SCET",tstr,"HIST",tasknam, NULL);
  if (vstat<=0) vstat = zlget(inunit,"HISTORY","BEG_UTC",tstr,"HIST",tasknam, NULL);
  fill_if_error(tstr,"N");      /* event time         */
  for( j=0; j<25; j++) v.event_start_time[j] = 0;		/* initialize */
  if( tstr[0] != ' ' ) {
    for( j=0; j<4; j++) v.event_start_time[j] = tstr[j];	/* Year */
    strcat( v.event_start_time,"-");
    determine_month( monthstr, &tstr[5] );
    strcat( v.event_start_time,monthstr);
    strcat( v.event_start_time,"-");
    v.event_start_time[8] = tstr[9];
    v.event_start_time[9] = tstr[10];
    strcat( v.event_start_time,"T");
    strcat( v.event_start_time,&tstr[12]);
    v.event_start_time[13] = ':';
    v.event_start_time[16] = ':';
    v.event_start_time[19] = 'Z';
  }

  vstat = zlget(inunit,"HISTORY","END_SCET",tstr,"HIST",tasknam, NULL);
  if (vstat<=0) vstat = zlget(inunit,"HISTORY","END_UTC",tstr,"HIST",tasknam, NULL);
  fill_if_error(tstr,"N");      /* event time         */
  if( tstr[0] != 'x' ) {
    for( j=0; j<25; j++) v.event_stop_time[j] = 0;
    for( j=0; j<4; j++) v.event_stop_time[j] = tstr[j];
    determine_month( monthstr, &tstr[5] );
    strcat( v.event_stop_time,"-");
    strcat( v.event_stop_time,monthstr);
    strcat( v.event_stop_time,"-");
    v.event_stop_time[8] = tstr[9];
    v.event_stop_time[9] = tstr[10];
    strcat( v.event_stop_time,"T");
    strcat( v.event_stop_time,&tstr[12]);
    v.event_stop_time[13] = ':';
    v.event_stop_time[16] = ':';
    v.event_stop_time[19] = 'Z';
  }

  vstat = zlget(inunit,"HISTORY","BEG_SCLK",integers,"HIST",tasknam,
      "NELEMENT",3, NULL);
  if( vstat == 1 ) {
    v.native_times[0][0] = integers[0];
    v.native_times[0][1] = integers[1];
    v.native_times[0][2] = integers[2];
  }
  else {
    v.native_times[0][0] = 0;
    v.native_times[0][1] = 0;
    v.native_times[0][2] = 0;
  }

  vstat = zlget(inunit,"HISTORY","END_SCLK",integers,"HIST",tasknam,
      "NELEMENT",2, NULL);
  if( vstat == 1 ) {
    v.native_times[1][0] = integers[0];
    v.native_times[1][1] = integers[1];
  }
  else {
    v.native_times[1][0] = 0;
    v.native_times[1][1] = 0;
  }

  vstat = zlget(inunit,"HISTORY","DN_SDEV",&v.sdband,"HIST",tasknam, NULL);
  zero_if_error(&v.sdband);

  vstat = zlget(inunit,"HISTORY","GEO_SDEV",&v.sdgeo,"HIST",tasknam, NULL);
  zero_if_error(&v.sdgeo);

  /* abbreviated target code for DATA_SET_ID: */
  if( strcmp(v.target,"VENUS")==0 )
    strcpy(v.targetcode,"V");
  else if( strcmp(v.target,"EARTH")==0 )
    strcpy(v.targetcode,"E");
  else if( strcmp(v.target,"MOON")==0 )
    strcpy(v.targetcode,"L");
  else if( strcmp(v.target,"JUPITER")==0 )
    strcpy(v.targetcode,"J");
  else if( strcmp(v.target,"IO")==0 || strcmp(v.target,"CALLISTO")==0 || 
           strcmp(v.target,"EUROPA")==0 || strcmp(v.target,"THEBE")==0 ||
           strcmp(v.target,"ADRASTEA")==0 || strcmp(v.target,"AMALTHEA")==0 ||
           strcmp(v.target,"LEDA")==0 || strcmp(v.target,"GANYMEDE")==0 ||
           strcmp(v.target,"HIMALIA")==0 || strcmp(v.target,"CARME")==0 ||
           strcmp(v.target,"LYSITHEA")==0 || strcmp(v.target,"ANANKE")==0 ||
           strcmp(v.target,"PASIPHAE")==0 || strcmp(v.target,"SINOPE")==0 ||
           strcmp(v.target,"METIS")==0 || strcmp(v.target,"ELARA")==0 )
    strcpy(v.targetcode,"J");
    /*strcpy(v.targetcode,"JSA");  /* JSA has been disapproved */
  else if( strcmp(v.target,"GASPRA")==0 || strcmp(v.target,"IDA")==0 )
    strcpy(v.targetcode,"A");
  else if( strcmp(v.target,"RING")==0 )
    strcpy(v.targetcode,"J");
    /*strcpy(v.targetcode,"JR");*/
  else    
    strcpy(v.targetcode,"J");  /* includes JR & JSA & CAL */

closfil:
  zvclose(inunit, NULL);

  /* check if backplanes needed: */
  if (sideunit>0 || bottunit>0) {
    sideunit = -1;
    bottunit = -1;
    zifmessage(" Sideplanes or Bottomplanes not supported in TOISIS mode");
  }
  n2suf = n3suf = 0;
  s2unit = s3unit = -1;

  if (backunit<0) return;

  vstat = zvopen(backunit,"OP","READ","OPEN_ACT","SA","IO_ACT","SA", NULL);
  vstat = zvget(backunit,"NL",&bnl,"NS",&bns,"NB",&nbkpln,"FORMAT",format,"ORG",
   borg, NULL);
  if (strncmp(format,"REAL",4) || strcmp(borg,"BSQ") || bnl!=inl || bns!=ns) {
    zifmessage(" Co-cube must be REAL BSQ and with same NL,NS as cube!");
    backunit = -1;
  }
  else {
    if (!strcmp(forg,"BSQ")) {
      n3suf = nbkpln;
      s3unit = backunit;
    }
    else {
      n2suf = nbkpln;
      s2unit = backunit;
      /* do the malloc required for BIL here to ensure enough memory: */
      i = BUFFERSIZE/(4*ns);
      if (4*ns*i != BUFFERSIZE) i++;
      /* allocate twice as much as needed, just to be safe ... */
      fdata = (float *)malloc(2*i*ns*sizeof(float));
  } }
  zvclose(backunit, NULL);
}


/****************************************************************************/
int FUNCTION get_real_value(realitem,buf,labptr)
/*
 * Get a fixed-point or floating point value from an ASCII string
 */
float	*realitem;				/* Real value pointer        */
int	*labptr;				/* Label element pointer     */
char	buf[];					/* PDS label buffer   	     */
{
  char	number[33];
  int	a,b;

  memset( number, 0, 32 );

  /* Find first digit/sign */
  while (!isdigit(buf[*labptr]) && buf[*labptr] != '+' && buf[*labptr] != '-') {
    lsef = incrm_check_buffer(labptr);
    if (lsef == 0) return 0;
  }

  /* read the number into buffer: */
  a=0;
  while (isdigit(buf[*labptr]) || buf[*labptr]=='-' || buf[*labptr]=='+' 
   || buf[*labptr]=='.' || buf[*labptr]=='e' || buf[*labptr]=='E' ) {
    number[a++] = buf[*labptr];
    lsef = incrm_check_buffer(labptr);
    if (lsef == 0 || a>=32) return 0;
  }
  (*realitem) = (float)atof(number);
  return 1;
}

/****************************************************************************/
int FUNCTION get_double_value(doubleitem,buf,labptr)
/*
 * Get a double precision point value from an ASCII string
 */
double	*doubleitem;				/* Real value pointer        */
int	*labptr;				/* Label element pointer     */
char	buf[];					/* PDS label buffer   	     */
{
  char	number[33];
  int	a,b;

  memset( number, 0, 32 );

  /* Find first digit/sign */
  while (!isdigit(buf[*labptr]) && buf[*labptr] != '+' && buf[*labptr] != '-') {
    lsef = incrm_check_buffer(labptr);
    if (lsef == 0) return 0;
  }

  /* read the number into buffer: */
  a=0;
  while (isdigit(buf[*labptr]) || buf[*labptr]=='-' || buf[*labptr]=='+' 
   || buf[*labptr]=='.' || buf[*labptr]=='e' || buf[*labptr]=='E' ) {
    number[a++] = buf[*labptr];
    lsef = incrm_check_buffer(labptr);
    if (lsef == 0 || a>=32) return 0;
  }
  (*doubleitem) = atof(number);
  return 1;
}


/****************************************************************************/
FUNCTION get_recinfo(nlabs,nhrecs,nrecs)
/*
 * Get pointer to object files and dimension of cube.
 * Also reads the ISIS cube label and history objects into
 * memory buffers.
 */
int	*nlabs,					/* Number of label records   */
	*nhrecs,				/* Number of history records */
	*nrecs;					/* Number of file records    */
{
  char 	buffer[BUFFERSIZE];			/* First 512 characters      */
						/* of the PDS label	     */
  int 	x,y,z;					/* Loop control variables    */
  int	labptr, lptr, lptr1;			/* Pointers to label element */
  int 	object, index, nsidp, nbotp;
  int	nsi,nbp;				/* Number of spectral index  */
  int i;
						/* images and backplanes     */
  /* Open ISIS cube file */
  vstat = zvopen( inunit, "OPEN_ACT", "SA", "IO_ACT", "SA", "OP", "READ",
   "U_NS", BUFFERSIZE, "U_NL", 100, "COND", "NOLABELS", NULL);

  /* Read first 512 bytes of the PDS label */
  zvread(inunit,buffer,"NSAMPS",BUFFERSIZE, NULL);
  numofbytes = BUFFERSIZE;			/* Buffer has BUFFERSIZE     */
						/* number of samples.        */
  labptr = 0;					/* Point to first character  */
  labend = 9999999;

  find_keyword("FILE_RECORDS",buffer,&labptr,&labend);
  get_integer_value(nrecs,buffer,&labptr);	/* Get FILE_RECORDS value    */

  find_keyword("LABEL_RECORDS",buffer,&labptr,&labend);
  get_integer_value(nlabs,buffer,&labptr);	/* Get LABEL_RECORDS value   */

  zvclose(inunit, NULL);

  /* Reopen input file with the ADDRESS option */
  vstat = zvopen(inunit,"OPEN_ACT","SA","IO_ACT","SA","OP","READ",
   "U_NS", BUFFERSIZE, "U_NL", *nrecs, "COND","NOLABELS","ADDRESS",&inptr, NULL);

	/* first allocate enough buffer to hold label -- then
	 * search for end of history object and re-allocate 
	 * enough to hold both */

  numofbytes = (*nlabs) * BUFFERSIZE;	/* # bytes in PDS label object */
  bufpoint = (char *)calloc(numofbytes,sizeof(char));	
  if( bufpoint == NULL ) zmabend(" *** Memory allocation error - label ***");

  zmve( 1, numofbytes, inptr, bufpoint, 1, 1);

  labptr = index = 0;
  labend = numofbytes;

  /* find where cube object starts */
  vstat = find_keyword("^QUBE =",bufpoint,&labptr,&labend);
  get_integer_value(nhrecs,bufpoint,&labptr);
  objptr[index++] = *nhrecs;

  *nhrecs -= 1;
  free( bufpoint );				/* Deallocate memory */
  numofbytes = (*nhrecs) * BUFFERSIZE;	/* # bytes in label + history objects */
  bufpoint = (char *)calloc(numofbytes,sizeof(char));	
  if( bufpoint == NULL ) zmabend(" *** Memory allocation error - history ***");
  zmve( 1, numofbytes, inptr, bufpoint, 1, 1);
  *nhrecs -= *nlabs;				/* save # history rec's only */

  labend = numofbytes;
  labptr = 0;
  find_keyword("CORE_ITEMS",bufpoint,&labptr,&labend);
  /* Get dimensions in storage order */
  get_integer_value(&n1,bufpoint,&labptr);
  get_integer_value(&n2,bufpoint,&labptr);
  get_integer_value(&n3,bufpoint,&labptr);

  /* check AXIS_NAME for file organization: */
  labend = numofbytes;
  labptr = 0;
  i = find_keyword("AXIS_NAME",bufpoint,&labptr,&labend);
  if (!i) zmabend(" AXIS_NAME not found!");
  labptr += 4;
  if (!strncmp(&bufpoint[labptr],"SAMPLE,LINE,REGION",18)) {
    /* we're in the specplot part of a NIMS label ... skip over it! */
    i = find_keyword("AXIS_NAME",bufpoint,&labptr,&labend);
    if (!i) zmabend(" AXIS_NAME not found!");
    labptr += 4;
  }
  if (!strncmp(&bufpoint[labptr],"SAMPLE,LINE,BAND",16)) {
    strcpy(forg,"BSQ");
    ns = n1;
    inl = n2;
    nb = n3;
  }
  else if (!strncmp(&bufpoint[labptr],"SAMPLE,BAND,LINE",16) ||
           !strncmp(&bufpoint[labptr],"SAMPLE, BAND, LINE",18)) {
    strcpy(forg,"BIL");
    ns = n1;
    nb = n2;
    inl = n3;
  }
  /* (so far, this is the only alternative needed) */
  else zmabend(" non-supported file organization: must be BSQ or BIL");

  /* get CORE pixel size and format*/

  find_keyword("CORE_ITEM_BYTES",bufpoint,&labptr,&labend);
  get_integer_value(&bytes,bufpoint,&labptr);

  i = find_keyword( "CORE_ITEM_TYPE", bufpoint, &labptr, &labend);
  if(!i) {
    zifmessage(" Keyword CORE_ITEM_TYPE not found in cube label");
    ihost = 0;
  }
  else {
    ihost = 1;
    get_string_value( buffer, 8, &labptr);
    if (!strncmp(buffer,"VAX",3)) strcpy(hostname,"VAX-VMS");
    else if (!strncmp(buffer,"SUN",3)) strcpy(hostname,"SUN-SOLR");
    else if (!strncmp(buffer,"PC_",3)) strcpy(hostname,"X86-LINUX");
    else {
      zifmessage(" unsupported CORE_ITEM_TYPE found in cube label");
      ihost = 0;
    }
  }
  /* when 4-byte, check for float/int cases */
  flint = 0;
  if (ihost>0 && bytes==4) {
    if (!strncmp(&buffer[3],"INT",3) || !strncmp(&buffer[4],"INT",3))
     flint = 1;
  }

  /* SUFFIX planes: */
  n1suf = n2suf = n3suf = 0;
  labptr = 0;
  i = find_keyword("SUFFIX_ITEMS",bufpoint,&labptr,&labend);
  if (i) {
    get_integer_value(&n1suf,bufpoint,&labptr);
    get_integer_value(&n2suf,bufpoint,&labptr);
    get_integer_value(&n3suf,bufpoint,&labptr);
    if (n1suf || n2suf || n3suf) {
      i = find_keyword("SUFFIX_BYTES",bufpoint,&labptr,&labend);
      if (!i) zifmessage(" Keyword SUFFIX_BYTES not found in cube label");
      else get_integer_value(&i,bufpoint,&labptr);
      if (i!=4) zmabend(" Suffixes must be 4-byte format!");
      labptr = 0;
      i = find_keyword("SUFFIX_ITEM_TYPE",bufpoint,&labptr,&labend);
      if (!i) zifmessage(" Keyword SUFFIX_ITEM_TYPE not found in cube label");
      else {
        get_string_value( buffer, 8, &labptr);
        if (!strncmp(&buffer[3],"INT",3) || !strncmp(&buffer[4],"INT",3))
	  suftyp = 0;
        else if (!strncmp(&buffer[3],"REAL",3) || !strncmp(&buffer[4],"REAL",3))
	  suftyp = 1;
        else zifmessage(" unsupported SUFFIX_ITEM_TYPE");
  } } }

  /* check with VICAR suffix-plane images and assign units to
   * dimensions: */
  s1unit = s2unit = s3unit = -1;
  if (sideunit>0) {
    if (!n1suf) {
      zifmessage("No sideplane found in cube, SIDE image ignored");
      s1unit = -1;
    }
    else s1unit = sideunit;
  }
  if (backunit>0) {
    if (!strcmp(forg,"BSQ")) {
      if (!n3suf) {
        zifmessage("No backplane found in cube, BACK image ignored");
        s3unit = -1;
      }
      else s3unit = backunit;
    }
    else {	/* BIL */
      if (!n2suf) {
        zifmessage("No backplane found in cube, BACK image ignored");
        s2unit = -1;
      }
      else s2unit = backunit;
    }
  }
  if (bottunit>0) {
    if (!strcmp(forg,"BIL")) {
      if (!n3suf) {
        zifmessage("No bottomplane found in cube, BOTT image ignored");
        s3unit = -1;
      }
      else s3unit = bottunit;
    }
    else {	/* BSQ */
      if (!n2suf) {
        zifmessage("No backplane found in cube, BACK image ignored");
        s2unit = -1;
      }
      else s2unit = bottunit;
    }
  }

  zvclose(inunit, NULL);
}


/****************************************************************************/
int FUNCTION get_string_value( buf, maxlen, lptr)
/*
 * return the first string in bufpoint following 'lptr'
 *
 * a "string" is defined here as composed of any character outside the set 
 * { "\r", "\n", "/", "(" (initial only), ")" (final only), " ", "=" (initial
 * only), "," }
 * -- the first character encountered that is not in this set begins the
 * string, and the first character thereafter encountered *in* that set
 * ends it;  note that this definition is NOT the same as that used for
 * strings in function keyword_value()!
 */
char *buf;
int maxlen, *lptr;
{
  int i;

  for (i=0; i<maxlen; i++) buf[i] = 0;		/* intialize */

	/* find beginning of string: */
  while (bufpoint[*lptr] == '\r' || bufpoint[*lptr] == '\n' ||
         bufpoint[*lptr] == '/' || bufpoint[*lptr] == ',' ||
         bufpoint[*lptr] == '"' || bufpoint[*lptr] == '=' || 
         bufpoint[*lptr] == '(' || bufpoint[*lptr] == ' ' ) {
    lsef = incrm_check_buffer( lptr);
    if (lsef == 0) return 0;
  }

	/* find end  of string: */
  i = 0;
  while (bufpoint[*lptr] != '\r' && bufpoint[*lptr] != '\n' &&
         bufpoint[*lptr] != '/'  && bufpoint[*lptr] != ',' &&
         bufpoint[*lptr] != '"' &&
         bufpoint[*lptr] != ')' && bufpoint[*lptr] != ' ' ) {
    if (i>maxlen) return 0;		/* item is too long for buffer */
    buf[i++] = bufpoint[*lptr];
    lsef = incrm_check_buffer( lptr);
    if (lsef == 0) return 0;
  }
  if (i<2) return 0;		/* string must be > 1 character */
  if (bufpoint[(*lptr)-1] == '/') buf[i-1] = ' ';

  return 1;
}


/****************************************************************************/
int FUNCTION get_qstring_value( buf, maxlen, lptr)
/*
 * return the first string in bufpoint following 'lptr'
 *
 * similar to get_string_value, but the string here is delimited only
 * by quotation marks ("), which are not themselves returned as part
 * of the string
 */
char *buf;
int maxlen, *lptr;
{
  int i;

  for (i=0; i<maxlen; i++) buf[i] = 0;		/* intialize */

	/* find beginning of string: */
  while (bufpoint[*lptr] != '"') {
    lsef = incrm_check_buffer( lptr);
    if (lsef == 0) return 0;
  }
  lsef = incrm_check_buffer( lptr);	/* move past the " */
  if (lsef == 0) return 0;

  i = 0;
  while (bufpoint[*lptr] != '"') {
    if (i>maxlen) return 0;		/* item is too long for buffer */
    buf[i++] = bufpoint[*lptr];
    lsef = incrm_check_buffer( lptr);
    if (lsef == 0) return 0;
  }
  if (i<2) return 0;		/* string must be > 1 character */
  if (bufpoint[(*lptr)-1] == '/') buf[i-1] = ' ';

  return 1;
}


/****************************************************************************/
int FUNCTION incrm_check_buffer(a)
/*
 * Increment buffer index and check against maximum number of bytes in 
 * the PDS label
 */
int *a;
{
  (*a)++;
  if (*a < numofbytes)
    return 1;
  else
    return 0;
}	


/****************************************************************************/
FUNCTION ISIStoVICAR()
/*
 * Generate VICAR file from ISIS cube 
 */
{
  int nlabrecs, nhistrecs, nrecs;

  zifmessage("   ");
  zifmessage("Inverse mode:  VICAR files will be generated from ISIS cube");
  zifmessage("   ");
  VICARlabels(&nlabrecs,&nhistrecs,&nrecs);	
  write_object(TOVICAR,nlabrecs,nhistrecs,nrecs);
}


/****************************************************************************/
int FUNCTION keyword_value(unit,pdsitem,type,vicaritem)
/*
 * Copy keyword/value pairs of PDS label to VICAR label.
 */
char 	pdsitem[],				  /* PDS item name           */
	type[],				  /* Type of label item: R/I/T/C     */
	vicaritem[];				  /* VICAR item name         */
int	unit;					  /* Unit number of output   */

{					/* Local Variables                   */
int	count,				/* Counter/index		     */
	vlen,
	found,				/* Item found indicator              */
	intitem[MAXBANDS],		/* Array of integer values           */
	itemlength,			/* Item name length 		     */
	labptr,			/* Pointer to label character        */
	nlines;				/* Number of lines in input file     */
float	realitem[MAXBANDS];		/* Array of real values 	     */
double	doubleitem[MAXBANDS];		/* Array of double values 	     */
char 	prevval,			/* Previous buffer character	     */
	stritem[MAXBANDS][101];

labptr = count = 0;
prevval = 0;
memset( line, 0, 120);
vlen = strlen(vicaritem)-1;

lsef = find_keyword("^QUBE",bufpoint,&labptr,&labend);
if (lsef == 0) goto notfound;

lsef = find_keyword(pdsitem,bufpoint,&labptr,&labend);
if (lsef == 0) goto notfound;

	/* (need to check specially for "-" sign, else this does
	 * not get passed on to get_real_value:) */
while(bufpoint[labptr]!='(' && bufpoint[labptr]!='\"' &&
  bufpoint[labptr]!='-' && isalnum(bufpoint[labptr])==0)
  {
  lsef = incrm_check_buffer(&labptr);
  if (lsef == 0) goto notfound;
  }
switch(bufpoint[labptr])
{
case '(':    /* Array of values - Real, Integer, or String */  
  if (type[0]=='D') {
    while ( bufpoint[ labptr]!=')' ) {
       lsef = get_double_value( &doubleitem[count++], bufpoint, &labptr);
       if (lsef == 0) goto notfound;
    }
    zladd( unit, "HISTORY", vicaritem, doubleitem,  "FORMAT", "DOUB",
     "NELEMENT", count, NULL);
  }  
  else if (type[0]=='R') {
    while ( bufpoint[ labptr]!=')' ) {
       lsef = get_real_value( &realitem[count++], bufpoint, &labptr);
       if (lsef == 0) goto notfound;
    }
    zladd( unit, "HISTORY", vicaritem, realitem,  "FORMAT", "REAL",
     "NELEMENT", count, NULL);
  }
  else if (type[0]=='I') {
    while ( bufpoint[labptr]!=')' ) {
      lsef = get_integer_value( &intitem[count++], bufpoint, &labptr);
      if (lsef == 0) goto notfound;
    }
    zladd( unit, "HISTORY", vicaritem, intitem,  "FORMAT", "INT",
     "NELEMENT", count, NULL);
  }
  else {	/* default type = 'S' */
    lsef = incrm_check_buffer(&labptr);	/* step past the '(' */
    if (lsef == 0) goto notfound;
    while ( bufpoint[labptr] != ')' ) {
      lsef = get_string_value( stritem[count++], 100, &labptr);
      if (lsef == 0) goto notfound;
      if ( bufpoint[labptr] != ')' ) 
	lsef = incrm_check_buffer(&labptr);	/* step past terminator */
      if (lsef == 0) goto notfound;
    }
    zladd( unit, "HISTORY", vicaritem, stritem,  "FORMAT", "STRING",
     "NELEMENT", count, "ULEN", 101, NULL);
  }
  break;

case '\"':    /* Value is a line comment 'Now is the time . . .'   */
  lsef = incrm_check_buffer(&labptr);
  if (lsef == 0) goto notfound;
  while(bufpoint[labptr]!='\"')
    {
    if(bufpoint[labptr]!='\'')
      line[count++]=bufpoint[labptr];
    lsef = incrm_check_buffer(&labptr);  
    if (lsef == 0) goto notfound;
    }
  zladd(unit,"HISTORY",vicaritem,line,"FORMAT","STRING", NULL);
  break;

default:    /* Value is not a line comment or array of values    */

  switch(type[0]) {    /* Check for REAL, INT, TIME, or STRING type  */

  case 'D':
    lsef = get_double_value(&doubleitem[0],bufpoint,&labptr);
    if (lsef == 0) goto notfound;
    zladd(unit,"HISTORY",vicaritem,realitem,"FORMAT","DOUB", NULL);
    break;

  case 'R':
    lsef = get_real_value(&realitem[0],bufpoint,&labptr);
    if (lsef == 0) goto notfound;
    zladd(unit,"HISTORY",vicaritem,realitem,"FORMAT","REAL", NULL);
    break;

  case 'I': 
    lsef = get_integer_value(&intitem[0],bufpoint,&labptr);
    if (lsef == 0) goto notfound;
    zladd(unit,"HISTORY",vicaritem,intitem,"FORMAT","INT", NULL);
    break;

  case 'T': 
    prevval=' ';
    while(bufpoint[labptr]!='\r' &&
        (bufpoint[labptr]!='*' && prevval!='/')) {
      if(isdigit(bufpoint[labptr])!=FALSE || 
          bufpoint[labptr] == '.') {
	if(count>1 && vicaritem[vlen]!='K' && vicaritem[vlen]!='T')
	  line[count-2]=bufpoint[labptr];
	else
	  line[count]=bufpoint[labptr];
      }
      else {		/* punctuation becomes spaces, Month gets extra 0 */
	if (count==4) {		/* start of Month string */
	  line[count++] = ' ';
	  line[count] = '0';
	}
	else line[count] = ' ';
      }
      count++;
      prevval=bufpoint[labptr];
      lsef = incrm_check_buffer(&labptr);
      if (lsef == 0) goto notfound;
    }
    zladd(unit,"HISTORY",vicaritem,line,"FORMAT","STRING", NULL);
    break;

  default:  	/* type = 'S' assumed */
    while (bufpoint[labptr]!='\r' &&
           bufpoint[labptr]!=' '  && bufpoint[labptr]!='*' &&
           bufpoint[labptr]!='\'' && prevval!='/') {
      line[count++]=bufpoint[labptr];
      prevval=bufpoint[labptr];
      lsef = incrm_check_buffer(&labptr);
       if (lsef == 0) goto notfound; 
    }
    if(prevval=='/')
      line[count-1]=' ';
    zladd(unit,"HISTORY",vicaritem,line,"FORMAT","STRING", NULL);
    break;
  }
  break;
}
memset( line, 0, 120);
return 1;

notfound:
  sprintf( message, " Keyword %s not found in cube label", pdsitem);
  zifmessage( message);
  return 0;
}


/****************************************************************************/
FUNCTION process_parms(direction,cubefile)
/*
 * Process all user parameters (except for a few which are used in ToVicar
 * mode only) and determine direction of transformation.
 */
int *direction;				/* Direction of transform    */
char cubefile[100];			/* Input files to CUBE	     */
{
  int i, iflag, j;
  char file[100];

  /* principal input/output files:  on VICAR side, this is just the
   * Core, on ISIS it is the whole cube;  VICAR INP needs to be a parameter 
   * in order to pass it to the PDS label writer */
  zvp("INP",cubefile,&i);
  zvunit(&inunit,"INP",1, NULL);
  zvp("OUT",file,&i);
  zvunit(&outunit,"OUT",1, NULL);

  /* suffix-plane images: */
  sideunit = backunit = bottunit = -1;	/* initialize flags */
  zvparm( "SIDE", file, &iflag, &j, 1, 0);
  if (iflag) zvunit( &sideunit, "NONE1", 1, "U_NAME", file, NULL);
  zvparm( "BACK", file, &iflag, &j, 1, 0);
  if (iflag) zvunit( &backunit, "NONE2", 1, "U_NAME", file, NULL);
  zvparm( "BOTT", file, &iflag, &j, 1, 0);
  if (iflag) zvunit( &bottunit, "NONE3", 1, "U_NAME", file, NULL);

  *direction = zvptst("TOISIS");

  /* Get task name for labels */
  zvp( "TASKNAME", tasknam, &htask);

  /* Check if history file specified: */
  zvp( "HISTFILE", hfilnam, &histfile);

  if( *direction == TOVICAR ) return;

  vstat = zvopen(inunit,"OP","READ","OPEN_ACT","SA","IO_ACT","SA", NULL);
  vstat = zvget(inunit,"NL",&inl,"NS",&ns,"NB",&nb,"PIX_SIZE",&bytes,
   "FORMAT",format,"ORG",forg,"HOST",hostname, NULL);
  if (strcmp(forg,"BSQ") && strcmp(forg,"BIL")) 
    zmabend("Only BSQ and BIL formats supported");
  vstat = zvclose(inunit, NULL);

  /* check for use of CMM co-cube */
  cocub = 0;
  if (backunit>=0) {
    zvp( "BACK", file, &i);
    j = strlen( file);
    if (!strncmp( &file[j-3],"COC",3) || !strncmp( &file[j-3],"coc",3))
     cocub = 1;
  }

  /* option to specify new wavelengths: */
  uwaves = 0;
  zvp( "WAVES", waves, &i);
  if (i) { 
    if (i!=nb) zifmessage(" WAVES has incorrect number of items -- ignored\n");
    else uwaves = 1;
} }


/***************************************************************************/
FUNCTION VICARlabels(nlabrecs,nhistrecs,nrecs)    
/*
 * Open VICAR outputs, generate format labels, and get object pointers.
 */
int	*nlabrecs,			/* Number of label records	     */
	*nhistrecs,			/* Number of history records	     */
	*nrecs;				/* Number of cube file records	     */
{
int count, i, jnb, jnl, radtyp;
char file[100];

get_recinfo(nlabrecs,nhistrecs,nrecs);	

/* open the Core file: */
if (bytes==1) 
  vstat = zvopen(outunit,"OP","WRITE","U_FORMAT","HALF","O_FORMAT",
	"BYTE","U_NL",inl,"U_NS",ns,"U_NB",nb,"U_ORG",forg,
	"OPEN_ACT","SA","IO_ACT","SA", NULL);
else if (bytes==2) 
  vstat = zvopen(outunit,"OP","WRITE","U_FORMAT","HALF","O_FORMAT",
	"HALF","U_NL",inl,"U_NS",ns,"U_NB",nb,"U_ORG",forg,
	"OPEN_ACT","SA","IO_ACT","SA", NULL);
else { 			/* (bytes==4) */
  if (flint) vstat = zvopen(outunit,"OP","WRITE","U_FORMAT","REAL",
   "O_FORMAT","FULL","U_NL",inl,"U_NS",ns,"U_NB",nb,"U_ORG",forg,
   "OPEN_ACT","SA","IO_ACT","SA", NULL);
  else vstat = zvopen(outunit,"OP","WRITE","U_FORMAT","REAL",
   "O_FORMAT","REAL","U_NL",inl,"U_NS",ns,"U_NB",nb,"U_ORG",forg,
   "OPEN_ACT","SA","IO_ACT","SA", NULL);
}

/* first gather map projection data and write a standard
 * Vicar MAPLABV2-format label:
 * (this call also writes other label items that cannot be
 * processed by keyword_value) */

lsef = write_vlab( outunit, &radtyp);
if (lsef == 0) zifmessage("*** Unable to write Vicar MAP label ***");

/* other label items of possible interest */

keyword_value(outunit,"SPACECRAFT_NAME","C","PROJECT");
keyword_value(outunit,"MISSION_NAME","C","MISSION");
keyword_value(outunit,"INSTRUMENT_ID","C","INSTRMNT");
keyword_value(outunit,"MISSION_PHASE_NAME","C","PHASE");
keyword_value(outunit,"TARGET_NAME","C","TARGET");

keyword_value(outunit,"CORE_NAME","C","DATANAME");
keyword_value(outunit,"CORE_UNIT","C","DATAUNIT");

keyword_value(outunit,"EVENT_START_TIME","T","BEG_SCET");
keyword_value(outunit,"EVENT_STOP_TIME","T","END_SCET");
keyword_value(outunit,"EVENT_END_TIME","T","END_SCET");

keyword_value( outunit, "MINIMUM_LATITUDE", "R", "MINLAT");
keyword_value( outunit, "MAXIMUM_LATITUDE", "R", "MAXLAT");

keyword_value( outunit, "MINIMUM_LONGITUDE", "R", "MINLON");
keyword_value( outunit, "MAXIMUM_LONGITUDE", "R", "MAXLON");
keyword_value( outunit, "EASTERNMOST_LONGITUDE", "R", "MINLON");
keyword_value( outunit, "WESTERNMOST_LONGITUDE", "R", "MAXLON");

keyword_value( outunit, "START_SUB_SPACECRAFT_LATITUDE", "R", "B_SSCLAT");
keyword_value( outunit, "STOP_SUB_SPACECRAFT_LATITUDE", "R", "E_SSCLAT");
keyword_value( outunit, "START_SUB_SOLAR_LATITUDE", "R", "B_SSLLAT");
keyword_value( outunit, "STOP_SUB_SOLAR_LATITUDE", "R", "E_SSLLAT");

keyword_value( outunit, "PHOTOMETRIC_CORRECTION_TYPE", "C","PHOT_FNC");
keyword_value( outunit, "MINNAERT_EXPONENT", "R", "MINN_EXP");

keyword_value( outunit, "INCIDENCE_ANGLE", "R", "INCI_ANG");
keyword_value( outunit, "EMISSION_ANGLE", "R", "EMIS_ANG");
keyword_value( outunit, "PHASE_ANGLE", "R", "PHAS_ANG");
keyword_value( outunit, "SOLAR_AZIMUTH", "R", "SUN_AZI");
keyword_value( outunit, "SUB_SPACECRAFT_AZIMUTH", "R", "SC_AZI");
keyword_value( outunit, "MINIMUM_SLANT_DISTANCE", "R", "MINRANGE");
keyword_value( outunit, "MAXIMUM_SLANT_DISTANCE", "R", "MAXRANGE");
keyword_value( outunit, "MINIMUM_CENTRAL_BODY_DISTANCE", "R", "MIN_CB_D");
keyword_value( outunit, "MAXIMUM_CENTRAL_BODY_DISTANCE", "R", "MAX_CB_D");

	/* Cassini VIMS: */
keyword_value( outunit, "X_OFFSET", "I", "X_OFFSET");
keyword_value( outunit, "Z_OFFSET", "I", "Z_OFFSET");
keyword_value( outunit, "INTERLINE_DELAY_DURATION", "R", "LINDELAY");
keyword_value( outunit, "SAMPLING_MODE_ID", "C", "SPAT_RES");
keyword_value( outunit, "SCAN_MODE_ID", "C", "SCANMODE");
keyword_value( outunit, "INSTRUMENT_MODE_ID", "C", "INS_MODE");
keyword_value( outunit, "SWATH_WIDTH", "I", "SWTH_WID");
keyword_value( outunit, "SWATH_LENGTH", "I", "SWTH_LEN");
keyword_value( outunit, "POWER_STATE_FLAG", "C", "PWR_STAT");
keyword_value( outunit, "OVERWRITTEN_CHANNEL_FLAG", "C", "PWR_STAT");
keyword_value( outunit, "SPECTRAL_EDITING_FLAG", "C", "SPEC_EDT");
keyword_value( outunit, "SPECTRAL_SUMMING_FLAG", "C", "SPEC_SUM");
keyword_value( outunit, "STAR_TRACKING", "C", "STAR_TRK");
keyword_value( outunit, "PACKING", "C", "PACKING");

        /* for mgs - bam 6/00 */

keyword_value( outunit, "FIRST_LINE_SAMPLE", "I", "FIRST_LS");
keyword_value( outunit, "LINE_EXPOSURE_DURATION", "R", "LINERate");
keyword_value( outunit, "ORIGINAL_SPACECRAFT_CLOCK_COUNT", "C", "OSCCC");

	/* read these as strings to avoid problem with floating-point
	 * format (they are the only items written with "%g" format
	 * -- needs to be fixed at some point -- TBD */
keyword_value( outunit, "MIN_SPACECRAFT_SOLAR_DISTANCE", "C",
 "MINSUN_D");
keyword_value( outunit, "MAX_SPACECRAFT_SOLAR_DISTANCE", "C",
 "MAXSUN_D");

keyword_value( outunit, "BAND_BIN_UNIT", "C", "WAVUNIT");
keyword_value( outunit, "BAND_BIN_CENTER", "R", "WAVLNTHS");
keyword_value( outunit, "BAND_BIN_ORIGINAL_BAND", "I", "ORIGBANDS");

zvclose(outunit, NULL);

/* also, any suffix-plane files requested: */

if (s1unit>0) {
  if (!suftyp) 
    vstat = zvopen(s1unit,"OP","WRITE","U_FORMAT","FULL","O_FORMAT",
     "FULL","U_NS",n1suf,"U_NL",inl,"U_NB",nb,"U_ORG","BSQ",
     "OPEN_ACT","SA","IO_ACT","SA", NULL);
  else
    vstat = zvopen(s1unit,"OP","WRITE","U_FORMAT","FULL","O_FORMAT",
     "REAL","U_NS",n1suf,"U_NL",inl,"U_NB",nb,"U_ORG","BSQ",
     "OPEN_ACT","SA","IO_ACT","SA", NULL);
  zvclose(s1unit, NULL);
}

if (s2unit>0) {
  if (!strcmp(forg,"BSQ")) {
    jnl = n2suf;
    jnb = nb;
  }
  else {	/* BIL */
    jnb = n2suf;
    jnl = inl;
  }
  if (!suftyp) 
    vstat = zvopen(s2unit,"OP","WRITE","U_FORMAT","FULL","O_FORMAT",
     "FULL","U_NS",ns+n1suf,"U_NL",jnl,"U_NB",jnb,"U_ORG","BSQ",
     "OPEN_ACT","SA","IO_ACT","SA", NULL);
  else
    vstat = zvopen(s2unit,"OP","WRITE","U_FORMAT","FULL","O_FORMAT",
     "REAL","U_NS",ns+n1suf,"U_NL",jnl,"U_NB",jnb,"U_ORG","BSQ",
    "OPEN_ACT","SA","IO_ACT","SA", NULL);
  zvclose(s2unit, NULL);
}

if (s3unit>0) {
  if (!strcmp(forg,"BSQ")) {
    jnl = inl+n2suf;
    jnb = n3suf;
  }
  else {	/* BIL */
    jnb = nb+n2suf;
    jnl = inl;
  }
  if (!suftyp) 
    vstat = zvopen(s3unit,"OP","WRITE","U_FORMAT","FULL","O_FORMAT",
     "FULL","U_NS",n1+n1suf,"U_NL",jnl,"U_NB",jnb,"U_ORG","BSQ",
     "OPEN_ACT","SA","IO_ACT","SA", NULL);
  else
    vstat = zvopen(s3unit,"OP","WRITE","U_FORMAT","FULL","O_FORMAT",
     "REAL","U_NS",n1+n1suf,"U_NL",jnl,"U_NB",jnb,"U_ORG","BSQ",
     "OPEN_ACT","SA","IO_ACT","SA", NULL);
  zvclose(s3unit, NULL);
}

}


/****************************************************************************/
FUNCTION VICARtoISIS(cubefile)
/*
 *Purpose: Generate ISIS Cube file
 */
char 	cubefile[100];		/* Input file names		     */
{					/* 	LOCAL VARIABLES		     */
  int band, ln, nhistrecs, nlabrecs, nrecs, object, stat1, stat2, x, y, z,
    instances[20], file_indices[6], numtasks;
  char task[20][8];

  zifmessage("   ");
  zifmessage(" Forward mode:  ISIS cube will be generated from VICAR file");
  zifmessage("   ");

  get_label_items();      /* Get all VICAR label items    */

  x = inl*ns*(nb+nbkpln)*bytes;	/* only backplanes allowed in TOISIS mode */
  y = x/BUFFERSIZE;
  if( y*BUFFERSIZE != x ) y += 1;
  nrecs = y;
  objptr[0] = nrecs;

  /* Write PDS label to cube file */
  create_PDSlabel(&nrecs,&nlabrecs,&nhistrecs,cubefile);             

  /* Write objects to cube file */
  write_object(TOISIS,nlabrecs,nhistrecs,nrecs);       
}


/****************************************************************************/
FUNCTION write_object(dir,nlrecs,nhrecs,nrecs) 
/*
 * Copies object files to ISIS cube file or appropriate 
 * VICAR files, depending on direction of processing requested.
 */
int dir,	/* Direction of processing */
  nlrecs,	/* Number of label records */
  nhrecs,	/* Number of history records */
  nrecs; 	/* Number of file records */

{
  int bnd, extrasamples, i, j, lin, ln, nbck, ncor, nr0, record, sample, size1,
   x, x2, x3, y, y1, z;
  unsigned char buf[1000];  /* Object file buffer           */
  unsigned char outbuf[BUFFERSIZE];
  unsigned char *obuf;
  char *bufptr, *s1ptr, *s2ptr, *s3ptr;
  char intfmt[30], realfmt[30];

  if (dir==TOISIS) {  /* Copy object files to ISIS cube file    */

    /* Open ISIS cube file */
    vstat = zvopen(outunit,"U_FORMAT","BYTE","O_FORMAT","BYTE","OP","WRITE",
     "OPEN_ACT","SA","IO_ACT","SA","U_NL",nrecs,"U_NS",BUFFERSIZE,"U_NB",1,
     "U_ORG","BSQ","COND","NOLABELS", NULL);

    for(x=0,ln=1;x<nlrecs;x++,ln++)  /* Write PDS label    */
      zvwrit(outunit,&bufpoint[x*BUFFERSIZE],"NSAMPS",BUFFERSIZE,
       "LINE",ln, NULL);
    free( bufpoint );    /* Deallocate memory    */

    for( x=0;x<nhrecs;x++,ln++ )  /* Write History object    */
      zvwrit(outunit,&histpoint[x*BUFFERSIZE],"NSAMPS",BUFFERSIZE,
       "LINE",ln, NULL);
    free( histpoint );    /* Deallocate memory    */

    /* if there's no cocube or ORG is BSQ, just copy input file contents
     * to ISIS CUBE object */
    if (!strcmp(forg,"BSQ") || backunit<0) {
      vstat = zvopen(inunit,"OP","READ","OPEN_ACT","SA","IO_ACT","SA",
       "ADDRESS",&inptr, NULL);
      size = inl*ns*nb*bytes;
      nr0 = size/BUFFERSIZE; 
      for (x=0; x<nr0; x++,ln++,inptr+=BUFFERSIZE) {
        zmve( 1, BUFFERSIZE, inptr, outbuf, 1, 1);
        zvwrit(outunit,outbuf,"NSAMPS",BUFFERSIZE,"LINE",ln, NULL);
      }
      size -= nr0*BUFFERSIZE;
      if (size > 0) {
        zmve( 1, size, inptr, outbuf, 1, 1);
	if (s3unit<0) {
          memset( &outbuf[size], 0, BUFFERSIZE-size );
          zvwrit(outunit,outbuf,"NSAMPS",size,"LINE",ln, NULL);
      } }
      if (s3unit>0) {
        vstat = zvopen(s3unit,"OP","READ","OPEN_ACT","SA","IO_ACT","SA",
         "ADDRESS",&inptr, NULL);
        size1 = inl*ns*n3suf*bytes;
        extrasamples = 0;
        if (size > 0) {
          extrasamples = BUFFERSIZE - size;
          if (size1 > extrasamples) {
            zmve( 1, extrasamples, inptr, &outbuf[size], 1, 1);
            inptr += extrasamples;
          }
          else {
            zmve( 1, size1, inptr, &outbuf[size], 1, 1);
            memset( &outbuf[size+size1], 0, BUFFERSIZE-size-size1 );
          }
          zvwrit(outunit,outbuf,"NSAMPS",BUFFERSIZE,"LINE",ln, NULL);
          ln++;
        }
        zvclose(inunit, NULL);
        size = size1 - extrasamples;
        nr0 = size/BUFFERSIZE; 
        for (x=0; x<nr0;  x++,ln++,inptr+=BUFFERSIZE) {
          zmve( 1, BUFFERSIZE, inptr, outbuf, 1, 1);
          zvwrit(outunit,outbuf,"NSAMPS",BUFFERSIZE,"LINE",ln, NULL);
        }
        size -= nr0*BUFFERSIZE;
        if (size > 0) {
          zmve( 1, size, inptr, outbuf, 1, 1);
          memset( &outbuf[size], 0, BUFFERSIZE-size);
          zvwrit(outunit,outbuf,"NSAMPS",BUFFERSIZE,"LINE",ln, NULL);
        }
        zvclose(s3unit, NULL);
      }
      else zvclose(inunit, NULL);
      zvclose(outunit, NULL);
      return;
    }
    /* otherwise, things get more complicated ... */

    vstat = zvopen(inunit,"OP","READ","OPEN_ACT","SA","IO_ACT","SA", NULL);
    vstat = zvopen(s2unit,"OP","READ","OPEN_ACT","SA","IO_ACT","SA", NULL);

    /* cube is BIL, but cocube is BSQ -- read both one line at a time,
     * then transfer to output buffer and write it out when a block
     * is full */
    ncor = nbck = 0;
    i = size = 0;
    for (lin=0; lin<inl; lin++) {
      for (bnd=0; bnd<nb; bnd++) {
	zvread( inunit, &fdata[i], "LINE", lin+1, "SAMP", 1, "NSAMPS", ns,
	 "BAND", bnd+1, NULL);
	i += ns;
	size += ns*4;
	if (size>=BUFFERSIZE) {
	  j = 0;
	  y = size/BUFFERSIZE;
          for (x=0; x<y;  x++) {
	    zmve( 1, BUFFERSIZE, &fdata[j], outbuf, 1, 1);
	    j += BUFFERSIZE/4;
	    zvwrit(outunit,outbuf,"NSAMPS",BUFFERSIZE,"LINE",ln++, NULL);
	    ncor += BUFFERSIZE/4;
	  }
	  size -= y*BUFFERSIZE;
	  i = size/4;
	  if (size>0)
	    for (x=0; x<i; x++) fdata[x] = fdata[j+x];
      } }
      ncor += i;
      nbck -= i;
      for (bnd=0; bnd<n2suf; bnd++) {
	zvread( s2unit, &fdata[i], "LINE", lin+1, "SAMP", 1, "NSAMPS", ns,
	 "BAND", bnd+1, NULL);
	i += ns;
	size += ns*4;
	if (size>=BUFFERSIZE) {
	  j = 0;
	  y = size/BUFFERSIZE;
          for (x=0; x<y;  x++) {
	    zmve( 1, BUFFERSIZE, &fdata[j], outbuf, 1, 1);
	    j += BUFFERSIZE/4;
	    zvwrit(outunit,outbuf,"NSAMPS",BUFFERSIZE,"LINE",ln++, NULL);
	    nbck += BUFFERSIZE/4;
	  }
	  size -= y*BUFFERSIZE;
	  i = size/4;
	  if (size>0)
	    for (x=0; x<i; x++) fdata[x] = fdata[j+x];
      } }
      ncor -= i;
      nbck += i;
    }
    ncor += i;
    if (size > 0) {
      zmve( 1, size, &fdata, outbuf, 1, 1);
      memset( &outbuf[size], 0, BUFFERSIZE-size);
      zvwrit(outunit,outbuf,"NSAMPS",BUFFERSIZE,"LINE",ln++, NULL);
    }
    sprintf( message, " %d words written to Core, %d to Backplanes\r\n",
     ncor, nbck);
    zifmessage( message);
    zvclose(inunit, NULL);
    zvclose(s2unit, NULL);
    zvclose(outunit, NULL);
  }
  else {   /* Copy ISIS cube file objects to VICAR file  */

    vstat = zvopen(inunit,"OPEN_ACT","SA","IO_ACT","SA","ADDRESS",&inptr,
      "U_NS", BUFFERSIZE, "U_NL", nrecs,
      "U_FORMAT","BYTE","O_FORMAT","BYTE","COND","NOLABELS", NULL);

    bufptr = inptr;  

    if (histfile) {      /* write out all history records to History file */
      hfile = fopen(hfilnam,"w");
      /* first determine where blank area starts, as we don't need to
       * copy this ... */
      inptr = bufptr+(nlrecs+nhrecs-1)*BUFFERSIZE;
      y1 = 0;
      for (x=0; x<nhrecs; x++) {
	zmve( 1, BUFFERSIZE, inptr, buf, 1, 1);
	for (y=BUFFERSIZE-1; y>=0; y--) {
	  if (buf[y] != ' ') {
	    y1 = y+1;
	    break;
	  }
	}
	if (y1>0) break;
	inptr -= BUFFERSIZE;
      }
      inptr = bufptr+nlrecs*BUFFERSIZE;
      for (z=0; z<nhrecs-x-1; z++) {
	zmve( 1, BUFFERSIZE, inptr, buf, 1, 1);
	inptr += BUFFERSIZE;
	for (y=0; y<BUFFERSIZE; y++)
	  vstat = fputc( buf[y], hfile);
      }
      zmve( 1, BUFFERSIZE, inptr, buf, 1, 1);
      for (y=0; y<y1-1; y++)
	vstat = fputc( buf[y], hfile);
      fclose(hfile);  
    }

    inptr = bufptr;    /* Calculate first cube record */
    inptr += (objptr[0]-1)*BUFFERSIZE;

    vstat = zvopen(outunit,"OP","UPDATE","OPEN_ACT","SA","IO_ACT","SA",
       "LAB_ACT","SA","CLOS_ACT","SA","ADDRESS",&outptr, NULL);

    if (s1unit>0) vstat = zvopen(s1unit,"OP","UPDATE","OPEN_ACT","SA",
     "IO_ACT","SA","LAB_ACT","SA","CLOS_ACT","SA","ADDRESS",&s1ptr, NULL);

    if (s2unit>0) vstat = zvopen(s2unit,"OP","UPDATE","OPEN_ACT","SA",
     "IO_ACT","SA","LAB_ACT","SA","CLOS_ACT","SA","ADDRESS",&s2ptr, NULL);

    if (!n1suf && !n2suf) {
      size = n1*n2*n3*bytes;
      if (size<=0) zmabend(" cube dimensions error ...");
      zmve( 1, size, inptr, outptr, 1, 1);
      inptr += size;
    }
    else {
      /* remove SUFFIX planes other than in last dimension (which is
       * simply not read) */
      size = n1*bytes;
      for (x3=0; x3<n3; x3++) {
        for (x2=0; x2<n2; x2++) {
	  zmve( 1, size, inptr, outptr, 1, 1);
	  inptr += size;
	  if (s1unit>0) {
	    zmve( 1, n1suf*4, inptr, s1ptr, 1, 1);
	    s1ptr += n1suf*4;
	  }
	  inptr += n1suf*4;
	  outptr += size;
	}
	if (s2unit>0) {
	  zmve( 1, (n1+n1suf)*4*n2suf, inptr, s2ptr, 1, 1);
	  s2ptr += (n1+n1suf)*4*n2suf;
	}
	inptr += (n1+n1suf)*4*n2suf;
      }
    }
    zvclose(outunit, NULL);
    if (s1unit>0) zvclose(s1unit, NULL);
    if (s2unit>0) zvclose(s2unit, NULL);

    /* 3rd dimension can be done at the end ... */
    if (s3unit>0) {
      vstat = zvopen(s3unit,"OP","UPDATE","OPEN_ACT","SA","IO_ACT","SA",
       "LAB_ACT","SA","CLOS_ACT","SA","ADDRESS",&s3ptr, NULL);
      zmve( 1, (n1+n1suf)*(n2+n2suf)*n3suf*4, inptr, s3ptr, 1, 1);
      zvclose(s3unit, NULL);
    }

    zvclose(inunit, NULL);

    /* update label if needed -- if not specified, assume that host is the
     * current machine
     * (do this after writing data to avoid confusing Vicar) */
    if (!ihost) {
      zvp("HOST",hostname,&x);
      if (!x) return;
    }

    /* (NATIVE_HOST_LABEL is defined in v2$inc:xvmaininc.h) */
    if (strcmp(hostname,NATIVE_HOST_LABEL)) {
      vstat = zvopen(outunit,"OP","UPDATE","OPEN_ACT","SA","IO_ACT","SA", NULL);
      vstat =zvhost(hostname,intfmt,realfmt);
      if (vstat<=0) zmabend(" invalid HOST!");
      zldel( outunit, "SYSTEM", "HOST", NULL);
      zladd( outunit, "SYSTEM", "HOST", hostname, "FORMAT", "STRING",
       "NELEMENT", 1, "MODE", "REPLACE", NULL); 
      zldel( outunit, "SYSTEM", "INTFMT", NULL);
      zladd( outunit, "SYSTEM", "INTFMT", intfmt, "FORMAT", "STRING",
       "NELEMENT", 1, "MODE", "REPLACE", NULL); 
      zldel( outunit, "SYSTEM", "REALFMT", NULL);
      zladd( outunit, "SYSTEM", "REALFMT", realfmt, "FORMAT", "STRING",
       "NELEMENT", 1, "MODE", "REPLACE", NULL); 
      zldel( outunit, "SYSTEM", "BHOST", NULL);
      zladd( outunit, "SYSTEM", "BHOST", hostname, "FORMAT", "STRING",
       "NELEMENT", 1, "MODE", "REPLACE", NULL); 
      zldel( outunit, "SYSTEM", "BINTFMT", NULL);
      zladd( outunit, "SYSTEM", "BINTFMT", intfmt, "FORMAT", "STRING",
       "NELEMENT", 1, "MODE", "REPLACE", NULL); 
      zldel( outunit, "SYSTEM", "BREALFMT", NULL);
      zladd( outunit, "SYSTEM", "BREALFMT", realfmt, "FORMAT", "STRING",
       "NELEMENT", 1, "MODE", "REPLACE", NULL); 
    }
  }
}


/*****************************************************************************/
FUNCTION write_PDS_aline(buf,type,labelitem,value,number)
/*
 * Write PDS label line to a given buffer for all data types.
 * This differs from write_PDS_line in that value points to a
 * single item, which is replicated in the label array.
 */
char 	buf[],
	type,
	labelitem[];
void	*value;
int 	number;
{
int	x,y,z;
char	strng[50],
	ln[80];
int	*ivalue;
float	*fvalue;
char 	*cvalue;

switch(type)	{

case 'C':	/* String constant 	*/
case 'c':	cvalue = value;
		sprintf(ln,"%s = (",labelitem);
		for( x = 0; x < number-1 ; x++ )
			{
			sprintf(strng,"%s,",cvalue);
			strcat(ln,strng);
			if( strlen(ln) > 60 )
				{
				strcat(ln,"\r\n");
				strcat(buf,ln);
				strcpy(ln,"     ");
				}
			}
		sprintf(strng,"%s)\r\n",cvalue);
		strcat(ln,strng);
		strcat(buf,ln);
		break;

case 'I': 	/* Integer value 	*/
case 'i':	ivalue = value;
		sprintf(ln,"%s = (",labelitem);
		for( x = 0; x < number-1 ; x++ )
				{
				sprintf(strng,"%d,",ivalue);
				strcat(ln,strng);
				if( strlen(ln) > 60 )
					{
					strcat(ln,"\r\n");
					strcat(buf,ln);
					strcpy(ln,"     ");
					}
				}
		sprintf(strng,"%d)\r\n",ivalue);
		strcat(ln,strng);
		strcat(buf,ln);
		break;

default:
case 'F': 	/* floating point value 	*/
case 'f':	fvalue = value;
		sprintf(ln,"%s = (",labelitem);
		for( x = 0; x < number-1 ; x++ )
				{
				sprintf(strng,"%f,",*fvalue);
				strcat(ln,strng);
				if( strlen(ln) > 60 )
					{
					strcat(ln,"\r\n");
					strcat(buf,ln);
					strcpy(ln,"     ");
					}
				}
		sprintf(strng,"%f)\r\n",*fvalue);
		strcat(ln,strng);
		strcat(buf,ln);
		break;	}
}


/*****************************************************************************/
FUNCTION write_PDS_cline( buf, item, value, num, slen)
/*
 * Write PDS label line to a given buffer for character data.
 * Because of the lengths of some filenames, the buffers need
 * to exceed the normal PDS line length.
 */
char buf[], item[], *value;
int num, slen;
{
  int x,y,z;
  char strng[101], ln[101];

  if (num==1) {
    sprintf( ln, "%s = ", item);
    sprintf( strng, "%s,", value);
    if( (strlen(ln)+strlen(strng)) > 60 ) {
      strcat(ln,"\r\n");
      strcat(buf,ln);
      strcpy(ln,"     ");
    }
    strcat(ln,strng);
    strcat(buf,ln);
    return;
  }

  sprintf( ln, "%s = (", item);

  for ( x = 0; x < num-1 ; x++ ) {
    sprintf( strng, "\"%s\",", value+x*slen);
    if ( (strlen(ln)+strlen(strng)) > 60 ) {
      strcat(ln,"\r\n");
      strcat(buf,ln);
      strcpy(ln,"     ");
    }
    strcat(ln,strng);
  }
  sprintf( strng, "\"%s\")\r\n", value+(num-1)*slen);
  if ( (strlen(ln)+strlen(strng)) > 60 ) {
    strcat(ln,"\r\n");
    strcat(buf,ln);
    strcpy(ln,"     ");
  }
  strcat(ln,strng);
  strcat(buf,ln);
}


/***************************************************************************/
FUNCTION write_PDS_line(buf,type,labelitem,value,number,prec)
/*
 * Write PDS label line to a given buffer for data types other than
 * character
 */
char 	buf[],
	type,
	labelitem[];
void	*value;
int 	number,prec;
{
  int x, y, z, *ivalue;
  char strng[50], ln[80];
  float	*fvalue;

  switch(type) {

    case 'I': 	/* Integer value 	*/
    case 'i':
      ivalue = value;
      if( number == 1 ) {
	sprintf(strng,"%s = %d\r\n",labelitem,(*ivalue));
	strcat(buf,strng);
      }
      else {
	sprintf(ln,"%s = (",labelitem);
	for( x = 0; x < number-1 ; x++ ) {
	  sprintf(strng,"%d,",(*(ivalue+x)));
	  strcat(ln,strng);
	  if( strlen(ln) > 60 ) {
	    strcat(ln,"\r\n");
	    strcat(buf,ln);
	    strcpy(ln,"     ");
	  }
	}
	sprintf(strng,"%d)\r\n",(*(ivalue+x)));
	strcat(ln,strng);
	strcat(buf,ln);
      }
      break;

    default:
    case 'F': 	/* floating point value 	*/
    case 'f':	fvalue = value;
      if( number == 1 ) {
	if (prec<0)
	  sprintf(strng,"%s = %e\r\n",labelitem,(*fvalue));
	else
	  sprintf(strng,"%s = %.*f\r\n",labelitem,prec,(*fvalue));
	strcat(buf,strng);
      }
      else {
	sprintf(ln,"%s = (",labelitem);
	for( x = 0; x < number-1 ; x++ ) {
	  if (prec<0)
	    sprintf(strng,"%e,",(*(fvalue+x)));
	  else
	    sprintf(strng,"%.*f,",prec,(*(fvalue+x)));
	strcat(ln,strng);
	if( strlen(ln) > 60 ) {
	  strcat(ln,"\r\n");
	  strcat(buf,ln);
	  strcpy(ln,"     ");
	}
      }
      if (prec<0)
	sprintf(strng,"%e)\r\n",(*(fvalue+x)));
      else
	sprintf(strng,"%.*f)\r\n",prec,(*(fvalue+x)));
      strcat(ln,strng);
      strcat(buf,ln);
    }
    break;
  }
}


/*****************************************************************************/
int FUNCTION write_vlab( unit, radtyp)
/*
 * write MAPLABV2-format label to Vicar output cube;
 * also process a few other labels here that cannot be directly
 * copied from PDS label using 'keyword_value'
 */
int unit, *radtyp;
{
  int count, i, ival[3], lptr, lptr0, lptr1, lptr2, lptre = 0, lptrq, 
   conv_lon, sflag, to_orig, x;
  float ritem, sav;
  double ditem;
  double temp;
  char buf[332], lattyp[15], projnam[15];
  struct { float rr[38]; int ii[2];} mdat;	/* for MAPLABV2 */
  MP mpo;

	/* zero the map buffer just to be safe: */
  for (i=0; i<38; i++) mdat.rr[i] = 0.0;
  for (i=0; i<2; i++) mdat.ii[i] = 0;

  lptr = lptrq = 0;

	/* first set the pointer to start of map group in qube object: */
  lsef = find_keyword( "^QUBE", bufpoint, &lptrq, &labend);
  if (lsef == 0) zmabend(" QUBE object not found!");
  lptr0 = lptrq;

	/* find end of label so we don't search thru entire file: */
  lsef = find_keyword( "END_OBJECT = QUBE", bufpoint, &lptre, &labend);

	/* find start of the map projection group: */
  lsef = find_keyword( "IMAGE_MAP_PROJECTION", bufpoint, &lptr0, &lptre);
  if (lsef == 0) {
    vstat = 0;
    zifmessage(" Map labels not found");
    goto post_map;
  }

  lptr = lptr0;
  lsef = find_keyword( "MAP_PROJECTION_TYPE", bufpoint, &lptr, &lptre);
  if (lsef == 0) {
    vstat = 0;
    zifmessage(" Map labels not found");
    goto post_map;
  }
  lsef = get_string_value( v.projection, 24, &lptr);

  /* check for ORTHOGRAPHIC alone, and fix: */
  if (strcmp(v.projection,"ORTHOGRAPHIC")==0) 
    strcpy(v.projection,"OBLIQUE_ORTHOGRAPHIC");

	/* check if it's planetographic or planetocentric */
  lptr = lptr0;
  lsef = find_keyword( "COORDINATE_SYSTEM_NAME", bufpoint, &lptr, &lptre);
  if (lsef == 0) pgraphic = -1;
  else {
    lsef = get_string_value( lattyp, 15, &lptr);
    if (lsef == 0) {
      vstat = 0;
      zifmessage(" invalid COORDINATE_SYSTEM_NAME");
      goto post_map;
    }
    if (!strcmp( lattyp, "PLANETOGRAPHIC")) pgraphic = 1;
    else pgraphic = 0;
  }

  /* check for Longitude direction:  if East change to West for mpbuf2mpo */
  conv_lon = 0;
  lptr = lptr0;
  lsef = find_keyword( "POSITIVE_LONGITUDE_DIRECTION", bufpoint, &lptr, &lptre);
  if (lsef) {
    lsef = get_string_value( buf, 5, &lptr);
    if (lsef && !strcmp(buf,"EAST")) conv_lon = 1;
  }

	/* for the following keywords, find end of map proj. group: */
  lptr2 = lptr0;
  lsef = find_keyword( "END_GROUP = IMAGE_MAP_PROJECTION", bufpoint, &lptr2,
   &lptre);

	/* then find all required keywords: */

	/* all projections need radii: */
  lptr = lptr0;
  lsef = find_keyword( "A_AXIS_RADIUS", bufpoint, &lptr, &lptr2);
  if (lsef) get_double_value( &v.radii[0], bufpoint, &lptr);
  lptr = lptr0;
  lsef = find_keyword( "B_AXIS_RADIUS", bufpoint, &lptr, &lptr2);
  if (lsef) get_double_value( &v.radii[1], bufpoint, &lptr);
  lptr = lptr0;
  lsef = find_keyword( "C_AXIS_RADIUS", bufpoint, &lptr, &lptr2);
  if (lsef) get_double_value( &v.radii[2], bufpoint, &lptr);
  mdat.rr[24] = v.radii[2];	/* polar radius */
  mdat.rr[36] = v.radii[1];	/* short equatorial radius (POV only) */
  mdat.rr[25] = v.radii[0];	/* equatorial radius */

  /* check for OFFSET_DIRECTION keyword -- if absent, assume old
   * label, hence FROM_ORIGIN */
  lptr = lptr0;
  lsef = find_keyword( "TO_ORIGIN", bufpoint, &lptr, &lptr2);
  if (lsef) to_orig = 1;
  else {
    if (zvptst("TO_ORI")) to_orig = 1;	/* allow user override */
    else to_orig = 0;
  }

  if (strcmp(v.projection,"POINT_PERSPECTIVE")==0) {

    lptr = lptr0;
    lsef = find_keyword( "SUB_SPACECRAFT_LATITUDE", bufpoint, &lptr, &lptr2);
    if (lsef) get_double_value( &v.ssc_lat, bufpoint, &lptr);

    lptr = lptr0;
    lsef = find_keyword( "SUB_SPACECRAFT_LONGITUDE", bufpoint, &lptr, &lptr2);
    if (lsef) {
      get_double_value( &v.ssc_lon, bufpoint, &lptr);
      if (conv_lon) v.ssc_lon = 360.-v.ssc_lon;
    }

    lptr = lptr0;
    lsef = find_keyword( "LINE_SUB_SPACECRAFT_OFFSET", bufpoint, &lptr,
     &lptr2);
    if (lsef) {
      get_double_value( &v.ssc_line, bufpoint, &lptr);
      if (to_orig) v.ssc_line = -v.ssc_line;
      v.ssc_line++;	/* relative to (1,1) */
    }

    lptr = lptr0;
    lsef = find_keyword( "SAMPLE_SUB_SPACECRAFT_OFFSET", bufpoint, &lptr,
     &lptr2);
    if (lsef) {
      get_double_value( &v.ssc_samp, bufpoint, &lptr);
      if (to_orig) v.ssc_samp = -v.ssc_samp;
      v.ssc_samp++;	/* relative to (1,1) */
    }

	/* allow old aliases for the above 2: */
    lptr = lptr0;
    lsef = find_keyword( "X_AXIS_SUB_SPACECRAFT_OFFSET", bufpoint, &lptr,
     &lptr2);
    if (lsef) {
      get_double_value( &v.ssc_line, bufpoint, &lptr);
      v.ssc_line++;	/* relative to (1,1) */
    }

    lptr = lptr0;
    lsef = find_keyword( "Y_AXIS_SUB_SPACECRAFT_OFFSET", bufpoint, &lptr,
     &lptr2);
    if (lsef) {
      get_double_value( &v.ssc_samp, bufpoint, &lptr);
      v.ssc_samp++;	/* relative to (1,1) */
    }

    lptr = lptr0;
    lsef = find_keyword( "TARGET_CENTER_DISTANCE", bufpoint, &lptr, &lptr2);
    if (lsef) get_double_value( &v.range, bufpoint, &lptr);

    lptr = lptr0;
    lsef = find_keyword( "LINE_OPTICAL_AXIS_OFFSET", bufpoint, &lptr, &lptr2);
    if (lsef) {
      get_double_value( &v.oaline, bufpoint, &lptr);
      if (to_orig) v.oaline = -v.oaline;
      v.oaline++;	/* relative to (1,1) */
    }

    lptr = lptr0;
    lsef = find_keyword( "SAMPLE_OPTICAL_AXIS_OFFSET", bufpoint, &lptr,
     &lptr2);
    if (lsef) {
      get_double_value( &v.oasamp, bufpoint, &lptr);
      if (to_orig) v.oasamp = -v.oasamp;
      v.oasamp++;	/* relative to (1,1) */
    }

	/* allow old aliases for the above 2: */
    lptr = lptr0;
    lsef = find_keyword( "X_AXIS_OPTICAL_AXIS_OFFSET", bufpoint, &lptr,
     &lptr2);
    if (lsef) {
      get_double_value( &v.oaline, bufpoint, &lptr);
      v.oaline++;	/* relative to (1,1) */
    }

    lptr = lptr0;
    lsef = find_keyword( "Y_AXIS_OPTICAL_AXIS_OFFSET", bufpoint, &lptr, 
     &lptr2);
    if (lsef) {
      get_double_value( &v.oasamp, bufpoint, &lptr);
      v.oasamp++;	/* relative to (1,1) */
    }

    lptr = lptr0;
    lsef = find_keyword( "FOCAL_LENGTH", bufpoint, &lptr, &lptr2);
    if (lsef) get_double_value( &v.focal, bufpoint, &lptr);

    lptr = lptr0;
    lsef = find_keyword( "FOCAL_PLANE_SCALE", bufpoint, &lptr, &lptr2);
    if (lsef) get_double_value( &v.cscale, bufpoint, &lptr);

    lptr = lptr0;
    lsef = find_keyword( "MAP_PROJECTION_ROTATION", bufpoint, &lptr, &lptr2);
    if (lsef) get_double_value( &v.rotation, bufpoint, &lptr);

	/* stuff values into maplab buffer: */
    mdat.ii[0] = 16;
    mdat.rr[26] = v.focal;
    mdat.rr[27] = v.oaline;
    mdat.rr[28] = v.oasamp;
    mdat.rr[29] = v.cscale;
    mdat.rr[30] = v.ssc_lat;
    mdat.rr[31] = v.ssc_lon;
    mdat.rr[32] = v.ssc_line;
    mdat.rr[33] = v.ssc_samp;
    mdat.rr[34] = v.rotation;
    mdat.rr[37] = v.range;
  }
  else {

    lptr = lptr0;
    lsef = find_keyword( "MAP_SCALE", bufpoint, &lptr, &lptr2);
    if (lsef) get_double_value( &v.scale, bufpoint, &lptr);

    lptr = lptr0;
    lsef = find_keyword( "CENTER_LATITUDE", bufpoint, &lptr, &lptr2);
    if (lsef) get_double_value( &v.center_lat, bufpoint, &lptr);

    lptr = lptr0;
    lsef = find_keyword( "CENTER_LONGITUDE", bufpoint, &lptr, &lptr2);
    if (lsef) {
      get_double_value( &v.center_lon, bufpoint, &lptr);
      if (conv_lon) v.center_lon = 360.-v.center_lon;
    }

    /* add 1 to LINE/SAMP OFFSETs to convert to absolute line/sample
     * (mpbuf2mpo will subtract 1) */
    lptr = lptr0;
    lsef = find_keyword( "LINE_PROJECTION_OFFSET", bufpoint, &lptr, &lptr2);
    if (lsef) {
      get_double_value( &v.center_line, bufpoint, &lptr);
      if (to_orig) v.center_line = -v.center_line;
      v.center_line++;
    }
    lptr = lptr0;
    lsef = find_keyword( "SAMPLE_PROJECTION_OFFSET", bufpoint, &lptr, &lptr2);
    if (lsef) {
      get_double_value( &v.center_samp, bufpoint, &lptr);
      if (to_orig) v.center_samp = -v.center_samp;
      v.center_samp++;
    }

	/* allow old aliases for the above 2: */
    lptr = lptr0;
    lsef = find_keyword( "X_AXIS_PROJECTION_OFFSET", bufpoint, &lptr, &lptr2);
    if (lsef) get_double_value( &v.center_line, bufpoint, &lptr);

    lptr = lptr0;
    lsef = find_keyword( "Y_AXIS_PROJECTION_OFFSET", bufpoint, &lptr, &lptr2);
    if (lsef) get_double_value( &v.center_samp, bufpoint, &lptr);

    v.projitem1 = 0.0;
    v.projitem2 = 0.0;
    if( v.projection[0] == 'L' ) {	/* Lambert */
      lptr = lptr0;
      lsef = find_keyword( "FIRST_STANDARD_PARALLEL", bufpoint, &lptr,
       &lptr2);
      if (lsef) get_double_value( &v.projitem1, bufpoint, &lptr);
      lptr = lptr0;
      lsef = find_keyword( "SECOND_STANDARD_PARALLEL", bufpoint, &lptr,
       &lptr2);
      if (lsef) get_double_value( &v.projitem2, bufpoint, &lptr);
    }

    v.rotation = 0.0;
    if( strcmp(v.projection,"POLAR_ORTHOGRAPHIC")==0 ||
        strcmp(v.projection,"POINT_PERSPECTIVE")==0 ||
        strcmp(v.projection,"OBLIQUE_ORTHOGRAPHIC")==0 ||
        strcmp(v.projection,"OBLIQUE_STEREOGRAPHIC")==0 ) {
      lptr = lptr0;
      lsef = find_keyword( "MAP_PROJECTION_ROTATION", bufpoint, &lptr,
       &lptr2);
      if (lsef) get_double_value( &v.rotation, bufpoint, &lptr);
    }

    /* translate ISIS back to Vicarese ... */
    if (!strcmp(v.projection,"'SINUSOIDAL_EQUAL-AREA'")) 
      strcpy(v.projection,"SINUSOIDAL");
    if (!strcmp(v.projection,"'CYLINDRICAL_EQUAL-AREA'"))
      strcpy(v.projection,"NORMAL_CYLINDRICAL");
    if (!strcmp(v.projection,"'CYLINDRICAL_EQUAL-AREA'"))
      strcpy(v.projection,"NORMAL_CYLINDRICAL");

	/* stuff values into maplab buffer: */
    if  (!strcmp( v.projection, "POLAR_ORTHOGRAPHIC")) mdat.ii[0] = 1;
    else if (!strcmp(v.projection,"OBLIQUE_ORTHOGRAPHIC")) mdat.ii[0]=2;
    else if (!strcmp(v.projection,"POLAR_STEREOGRAPHIC")) mdat.ii[0]=3;
    else if (!strcmp(v.projection,"OBLIQUE_STEREOGRAPHIC")) mdat.ii[0]=4;
    else if (!strcmp(v.projection,"LAMBERT_CONFORMAL")) mdat.ii[0]=5;
    else if (!strcmp(v.projection,"MERCATOR")) mdat.ii[0]=6;
    else if (!strcmp(v.projection,"NORMAL_CYLINDRICAL")) mdat.ii[0]=9;
    else if (!strcmp(v.projection,"SIMPLE_CYLINDRICAL")) mdat.ii[0]=10;
    else if (!strcmp(v.projection,"OBLIQUE_SIMPLE_CYLINDRICAL")) mdat.ii[0]=11;
    else if (!strcmp(v.projection,"SINUSOIDAL")) mdat.ii[0]=12;
    else if (!strcmp(v.projection,"OBLIQUE_SINUSOIDAL")) mdat.ii[0]=13;
    else if (!strcmp(v.projection,"MOLLWEIDE")) mdat.ii[0]=14;
    else if (!strcmp(v.projection,"TRANSVERSE_MERCATOR")) mdat.ii[0]=15;
    else if (!strcmp(v.projection,"POINT_PERSPECTIVE")) mdat.ii[0]=16;
    mdat.rr[0] = v.center_samp;
    mdat.rr[1] = v.center_line;
    mdat.rr[2] = v.center_lat;
    mdat.rr[3] = v.projitem1;
    mdat.rr[4] = v.projitem2;
    mdat.rr[5] = v.center_lon;
    mdat.rr[6] = v.scale;
    if (v.center_lat < 0.) mdat.rr[7] = -1.0;
    else mdat.rr[7] = 1.0;
    mdat.rr[8] = v.rotation;
  }

	/* write the Vicar map labels: */
  lsef = mpInit( &mpo);
  if (lsef!=mpSUCCESS) zmabend(" error initializing MP object");
  lsef = mpBuf2Mpo( &mdat, mpo);
  if (lsef!=mpSUCCESS) zmabend(" error in MP translation");
  /* mpBuf2Mpo always writes "planetocentric", so fix this if we passed
   * it 'graphic' latitudes: */
  if (pgraphic==1) {
    vstat = mpSetValues( mpo, mpCOORDINATE_SYSTEM_NAME, "PLANETOGRAPHIC", "");
    if (vstat!=mpSUCCESS) zmabend(" error setting Coord.system");
  }
  lsef = mpLabelWrite( mpo, unit, "HISTORY");
  if (lsef!=mpSUCCESS) zmabend(" error writing map labels");
  lsef = mpLabelWrite( mpo, unit, "PROPERTY");
  if (lsef!=mpSUCCESS) zmabend(" error writing map labels");

post_map:

	/* process other deviant label items: */

  lptr = lptrq;
  lsef = find_keyword( "OBSERVATION_NAME", bufpoint, &lptr, &lptre);
  if (lsef==0) {
    lptr = lptrq;
    lsef = find_keyword( "OBSERVATION_ID", bufpoint, &lptr, &lptre);
  }
  if (lsef>0) {
    /* following VISIS2, we strip quotes and any characters after 12th */
    get_string_value( buf, 14, &lptr);
    if (buf[0] == '\'') strncpy( buf, &buf[1], 12);
    buf[12] = '\0';
    zladd( unit, "HISTORY", "OBSNAME", buf, "FORMAT", "STRING", "NELEMENT",
     1, NULL);
  }

  lptr = lptrq;
  lsef = find_keyword( "OBSERVATION_NOTE", bufpoint, &lptr, &lptre);
  if (lsef>0) keyword_value( unit, "OBSERVATION_NOTE","C","OBSNOTE");

  /* get project name (again) so we know how to deal with SCLKs and any other
   * mission-specific keywords: */
  strcpy(projnam,"UNK");
  lptr = lptrq;
  lsef = find_keyword( "SPACECRAFT_NAME", bufpoint, &lptr, &lptre);
  if (lsef == 0) {
    lptr = lptrq;
    lsef = find_keyword( "MISSION_NAME", bufpoint, &lptr, &lptre);
    if (lsef == 0) zifmessage(" Project name not found ...");
    else get_string_value( projnam, 16, &lptr);
  }
 
  /* look for NATIVE_TIMEs -- these can occur both as a quoted string
   * and as a float, so need to check for both
   * (must then do special checks for plain START/STOP_TIMEs) */
  lptr = lptrq;
  sflag = 1;
  lsef = find_keyword_str( "NATIVE_START_TIME = \"", bufpoint, &lptr, &lptre);
  if (lsef == 0) {
    lptr = lptrq;
    lsef = find_keyword( "NATIVE_START_TIME", bufpoint, &lptr, &lptre);
    if (lsef == 0) zifmessage(" Keyword NATIVE_START_TIME not found in cube label");
    else sflag = 0;
  }
  if (lsef) {
    get_string_value( buf, 16, &lptr);
    if (!strcmp( projnam, "GALILEO")) {
      i = sscanf( buf, "%d.%d", &ival[0], &ival[1]); 	/* RIM.MF */
      zladd( unit, "HISTORY", "BEG_SCLK", ival, "FORMAT", "INT", "NELEMENT",
       2, NULL);
    }
    else zladd( unit, "HISTORY", "BEG_SCLK", buf, "FORMAT", "STRING", "NELEMENT",
     1, NULL);
    /* only if we found a start time ... start after preceding item */
    if (sflag) lsef = find_keyword_str( "NATIVE_STOP_TIME = \"", bufpoint,
     &lptr, &lptre);
    else lsef = find_keyword( "NATIVE_STOP_TIME", bufpoint, &lptr, &lptre);
    if (lsef == 0) {
      lptr = lptrq;
      lsef = find_keyword( "NATIVE_END_TIME", bufpoint, &lptr, &lptre);
      if (lsef == 0) 
        zifmessage(" Keyword NATIVE_STOP_TIME not found in cube label");
    }
    if (lsef != 0) {
      get_string_value( buf, 16, &lptr);
      if (!strcmp( projnam, "GALILEO")) {
        i = sscanf( buf, "%d.%d", &ival[0], &ival[1]); 	/* RIM.MF */
        zladd( unit, "HISTORY", "END_SCLK", ival, "FORMAT", "INT", "NELEMENT",
         2, NULL);
      }
      else
        zladd( unit, "HISTORY", "END_SCLK", buf, "FORMAT", "STRING", "NELEMENT",
         1, NULL);
    }
  }

  /* look for START/STOP_TIME followed by a quoted string to get Cassini UTC times;
   * be careful to distinguish from NATIVE_TIMEs */
  lptr = lptrq;
  lsef = find_keyword_str( "START_TIME = \"", bufpoint, &lptr, &lptre);
  if (!strncmp(&bufpoint[lptr-20],"NATIVE",6))
    lsef = find_keyword_str( "START_TIME = \"", bufpoint, &lptr, &lptre);
  if (lsef == 0) 
    zifmessage(" Keyword START_TIME not found in cube label");
  else {
    get_string_value( buf, 25, &lptr);
    zladd( unit, "HISTORY", "BEG_UTC", buf, "FORMAT", "STRING", "NELEMENT",
     1, NULL);
    /* start looking after previous keywd: */
    lsef = find_keyword_str( "STOP_TIME = \"", bufpoint, &lptr, &lptre);
    if (lsef == 0) 
      zifmessage(" Keyword STOP_TIME not found in cube label");
    else {
      get_string_value( buf, 25, &lptr);
      zladd( unit, "HISTORY", "END_UTC", buf, "FORMAT", "STRING", "NELEMENT",
       1, NULL);
  } }

  /* need to restrict to Cassini because of the LINE_EXPOSURE keyword: */ 
  if (!strncmp( projnam, "CASSINI",7))
   keyword_value( outunit, "EXPOSURE_DURATION", "R", "EXPOSURE");

  /* these all depend on Pos.Long.Direction: */

  lptr = lptrq;
  lsef = find_keyword( "START_SUB_SPACECRAFT_LONGITUDE",bufpoint,&lptr,&lptre);
  if (lsef) {
    get_real_value( &ritem, bufpoint, &lptr);
    if (conv_lon) ritem = 360.-ritem;
    zladd( unit, "HISTORY", "B_SSCLON", &ritem, "FORMAT", "REAL", NULL);
  }
  lptr = lptrq;
  lsef = find_keyword( "STOP_SUB_SPACECRAFT_LONGITUDE",bufpoint,&lptr,&lptre);
  if (lsef) {
    get_real_value( &ritem, bufpoint, &lptr);
    if (conv_lon) ritem = 360.-ritem;
    zladd( unit, "HISTORY", "E_SSCLON", &ritem, "FORMAT", "REAL", NULL);
  }
  lptr = lptrq;
  lsef = find_keyword( "START_SUB_SOLAR_LONGITUDE",bufpoint,&lptr,&lptre);
  if (lsef) {
    get_real_value( &ritem, bufpoint, &lptr);
    if (conv_lon) ritem = 360.-ritem;
    zladd( unit, "HISTORY", "B_SSLLON", &ritem, "FORMAT", "REAL", NULL);
  }
  lptr = lptrq;
  lsef = find_keyword( "STOP_SUB_SOLAR_LONGITUDE",bufpoint,&lptr,&lptre);
  if (lsef) {
    get_real_value( &ritem, bufpoint, &lptr);
    if (conv_lon) ritem = 360.-ritem;
    zladd( unit, "HISTORY", "E_SSLLON", &ritem, "FORMAT", "REAL", NULL);
  }
  return 1;
}
