$!****************************************************************************
$!
$! Build proc for MIPL module isislab
$! VPACK Version 1.9, Monday, December 07, 2009, 16:33:20
$!
$! Execute by entering:		$ @isislab
$!
$! The primary option controls how much is to be built.  It must be in
$! the first parameter.  Only the capitalized letters below are necessary.
$!
$! Primary options are:
$!   COMPile     Compile the program modules
$!   ALL         Build a private version, and unpack the PDF and DOC files.
$!   STD         Build a private version, and unpack the PDF file(s).
$!   SYStem      Build the system version with the CLEAN option, and
$!               unpack the PDF and DOC files.
$!   CLEAN       Clean (delete/purge) parts of the code, see secondary options
$!   UNPACK      All files are created.
$!   REPACK      Only the repack file is created.
$!   SOURCE      Only the source files are created.
$!   SORC        Only the source files are created.
$!               (This parameter is left in for backward compatibility).
$!   PDF         Only the PDF file is created.
$!   TEST        Only the test files are created.
$!   IMAKE       Only the IMAKE file (used with the VIMAKE program) is created.
$!
$!   The default is to use the STD parameter if none is provided.
$!
$!****************************************************************************
$!
$! The secondary options modify how the primary option is performed.
$! Note that secondary options apply to particular primary options,
$! listed below.  If more than one secondary is desired, separate them by
$! commas so the entire list is in a single parameter.
$!
$! Secondary options are:
$! COMPile,ALL:
$!   DEBug      Compile for debug               (/debug/noopt)
$!   PROfile    Compile for PCA                 (/debug)
$!   LISt       Generate a list file            (/list)
$!   LISTALL    Generate a full list            (/show=all)   (implies LIST)
$! CLEAN:
$!   OBJ        Delete object and list files, and purge executable (default)
$!   SRC        Delete source and make files
$!
$!****************************************************************************
$!
$ write sys$output "*** module isislab ***"
$!
$ Create_Source = ""
$ Create_Repack =""
$ Create_PDF = ""
$ Create_Test = ""
$ Create_Imake = ""
$ Do_Make = ""
$!
$! Parse the primary option, which must be in p1.
$ primary = f$edit(p1,"UPCASE,TRIM")
$ if (primary.eqs."") then primary = " "
$ secondary = f$edit(p2,"UPCASE,TRIM")
$!
$ if primary .eqs. "UNPACK" then gosub Set_Unpack_Options
$ if (f$locate("COMP", primary) .eqs. 0) then gosub Set_Exe_Options
$ if (f$locate("ALL", primary) .eqs. 0) then gosub Set_All_Options
$ if (f$locate("STD", primary) .eqs. 0) then gosub Set_Default_Options
$ if (f$locate("SYS", primary) .eqs. 0) then gosub Set_Sys_Options
$ if primary .eqs. " " then gosub Set_Default_Options
$ if primary .eqs. "REPACK" then Create_Repack = "Y"
$ if primary .eqs. "SORC" .or. primary .eqs. "SOURCE" then Create_Source = "Y"
$ if primary .eqs. "PDF" then Create_PDF = "Y"
$ if primary .eqs. "TEST" then Create_Test = "Y"
$ if primary .eqs. "IMAKE" then Create_Imake = "Y"
$ if (f$locate("CLEAN", primary) .eqs. 0) then Do_Make = "Y"
$!
$ if (Create_Source .or. Create_Repack .or. Create_PDF .or. Create_Test .or -
        Create_Imake .or. Do_Make) -
        then goto Parameter_Okay
$ write sys$output "Invalid argument given to isislab.com file -- ", primary
$ write sys$output "For a list of valid arguments, please see the header of"
$ write sys$output "of this .com file."
$ exit
$!
$Parameter_Okay:
$ if Create_Repack then gosub Repack_File
$ if Create_Source then gosub Source_File
$ if Create_PDF then gosub PDF_File
$ if Create_Test then gosub Test_File
$ if Create_Imake then gosub Imake_File
$ if Do_Make then gosub Run_Make_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_Source = "Y"
$   Create_PDF = "Y"
$   Create_Test = "Y"
$   Create_Imake = "Y"
$ Return
$!
$ Set_EXE_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_Default_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$   Create_PDF = "Y"
$ Return
$!
$ Set_All_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$   Create_PDF = "Y"
$ Return
$!
$ Set_Sys_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Create_PDF = "Y"
$   Do_Make = "Y"
$ Return
$!
$Run_Make_File:
$   if F$SEARCH("isislab.imake") .nes. ""
$   then
$      vimake isislab
$      purge isislab.bld
$   else
$      if F$SEARCH("isislab.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake isislab
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @isislab.bld "STD"
$   else
$      @isislab.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create isislab.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack isislab.com -mixed -
	-s isislab.c -
	-p isislab.pdf -
	-i isislab.imake -
	-t tstisislab.pdf tstisislab.log_solos tstisislab.log_linux
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create isislab.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/****************************************************************************
 * Program ISISLAB prints the ISIS label and history objects of an ISIS cube.
 *
 * 26feb92 --lwk-- initial version
 * 20sep95 --lwk-- ported to Alpha
 * 25jun96 --lwk-- don't make 2-D Histo object required, so it can read any
 *		ISIS label, not just NIMS cubes
 * 03nov96 --lwk-- allow tabs as separators (so can read EDR labels)
 * 29sep98 --lwk-- recognize DATA_TABLE for EDRs
 * 25jan99 --lwk-- made portable to Unix:  fixed some function arguments &
 *		initializations, removed ZIA, added size to zvopen
 *  1sep00 --lwk-- cleaned up code for non-NIMS-cube cases
 *  2nov05 --lwk-- removed references to specific objects (except for HISTORY)
 */

#include <string.h>
#include <stdio.h>
#include <time.h>
#include <ctype.h>
#include <math.h>
#include "xvmaininc.h"
#include "vicmain_c"

#define FUNCTION 
#define MAXLEN 133	/* max. length of a label line */
#define RECL 512	/* record length of PDS file */

char msg[MAXLEN];
char buf[RECL];
int iun, recno;

/****************************************************************************/
void main44(void)

{
  int bptr=0, i, j, k, hstart, nhist, nlab, nrec, stat;
  char opt[8];

	/* tell user the version */
  zvmessage("*** ISISLAB version 3-Nov-05 ***",0);

  stat = zvunit( &iun, "INP", 1, NULL);

  /* Unix vicar needs size parameters ... make it big enough to
   * handle any label */
  stat = zvopen( iun, "OPEN_ACT", "SA", "IO_ACT", "SA", "OP", "READ",
   "COND","NOLABELS", "U_NL", 1000, "U_NS", RECL, NULL);

	/* Read first record of the PDS label */
  stat = zvread( iun, buf, "NSAMPS", RECL, NULL);

	/* find the number of label */
  stat = find_keyword( "LABEL_RECORDS", &bptr);
  if (!stat) zmabend(" *** cannot find LABEL_RECORDS item ***");
  stat = get_ival( &nlab, &bptr);
  if (!stat) zmabend(" *** cannot find LABEL_RECORDS value ***");

	/* check for option requested */
  k = 1;
  zvparm(  "OPTION", opt, &i, &j, k, 0);
  if (!strcmp( opt, "LABEL")) goto listl;

  stat = find_keyword( "^HISTORY", &bptr);
  if (stat) {
    stat = get_ival( &hstart, &bptr);
    if (!stat) zmabend(" *** cannot find HISTORY address ***");
  }
  else {
    if (!strcmp( opt, "HISTORY")) zmabend(" no History item present");
    else zvmessage(" no History item present",0);
    strcpy( opt, "LABEL");
	/* Reload first label record */
    stat = zvread( iun, buf, "LINE", 1, "NSAMPS", RECL, NULL);
    goto listl;
  }

	/* History length is defined by start of next item, so we
	 * need to find its address -- we assume that:
	 * 1.  all objects are signalled by a "^" in column 1;
	 * 2. this is unique;
	 * 3. HISTORY is always the first object (if present). */

  stat = find_char( "^", &bptr);
  if (stat) {
    stat = get_ival( &nhist, &bptr);
    if (!stat) zmabend(" *** error getting object address ***");
  }
  nhist -= hstart;

listl:
  if (!strcmp( opt, "HISTORY")) {
    for (i=0; i<nlab; i++)	/* skip the label */
      stat = zvread( iun, buf, "NSAMPS", RECL, NULL);
    bigprnt("********* HISTORY OBJECT **********");
    nrec = nhist;
  }
  else {
    bigprnt("********** LABEL OBJECT **********");
    nrec = nlab;
  }

	/* go read the assigned labels: */
  recno = 0;		/* record # currently being read */
  read_recs( nrec);

  if (!strcmp( opt, "HISTORY")) {
    bigprnt("********** END OF HISTORY OBJECT **********");
    return;
  }
  else {
    bigprnt("********** END OF LABEL OBJECT **********");
    if (!strcmp( opt, "LABEL")) return;
  }

	/* here if OPT = BOTH */
  for (i=0; i < nlab-recno; i++)	/* skip the blank label records */
    stat = zvread( iun, buf, "NSAMPS", RECL, NULL);
  bigprnt("********** HISTORY OBJECT **********");
  recno = 0;
  read_recs( nhist);
  bigprnt("********** END OF HISTORY OBJECT **********");
  return;
}


/***************************************************************************/
FUNCTION read_recs( nrec)
/*
 * read 'nrec' records and print them out
 */
int nrec;
{
  int bptr, i, stat;

  bptr = 1;		/* initialize buffer pointer */
  i = 0;
  stat = 1;		/* non-empty record status */
  while (recno < nrec) {

    if ( i >= MAXLEN-1 ) {
      msg[MAXLEN-1] = 0;
      zvmessage( msg,0);
      i = 0;
    }
    else if (i>1 && msg[i-2] == '\r' && msg[i-1] == '\n' ) {
      msg[i-2] = 0;
      zvmessage( msg,0);
      i = 0;
    }
    msg[i++] = buf[bptr];
    stat = incr(&bptr);
    if (!stat) break;
  }
}


/***************************************************************************/
FUNCTION bigprnt( strng)
/*
 * print a header
 */
char *strng;
{
  zvmessage(" ",0);
  zvmessage( strng,0);
  zvmessage(" ",0);
}


/***************************************************************************/
int FUNCTION find_keyword( keyword, bptr)
/*
 * find keyword and prepare for value reading
 */
char keyword[];
int *bptr;
{
  int count, found, keylength, startptr, stat;

  startptr  = *bptr;			/* Record starting pointer   */
  found     = 0;				/* Item not found yet        */
  keylength = strlen(keyword);			/* Determine itemname length */

  while ( !found ) {
    count = 0;
    stat = 1;
    while ( buf[*bptr] != keyword[count] )
      if ( incr( bptr ) == 0 ) return 0;
    if ( incr( bptr ) == 0 ) return 0;
    count++;

    while ( buf[*bptr] == keyword[count] && count < keylength ) {
      if ( incr( bptr ) == 0 ) return 0;
      else count++;
    }
    if ( ( buf[*bptr]==' ' || buf[*bptr] == '=' || buf[*bptr] == '\t' ) &&
     count == keylength )
      found = 1;
  }

  return 1;
}


/***************************************************************************/
int FUNCTION find_char( keyword, bptr)
/*
 * find a 1-character keyword
 */
char keyword[];
int *bptr;
{
  int found, keylength, startptr, stat;

  startptr  = *bptr;			/* Record starting pointer   */
  found     = 0;			/* Item not found yet        */
  keylength = strlen(keyword);
  if (keylength>1) zmabend(" illegal call to find_char");
  while ( !found ) {
    stat = 1;
    while ( buf[*bptr] != keyword[0] )
      if ( incr( bptr ) == 0 ) return 0;
    if ( incr( bptr ) == 0 ) return 0;
    found = 1;
  }
  return 1;
}


/*****************************************************************************/
int FUNCTION get_ival( intitem, bptr)
/*
 * get the next integer value after bptr
 */
int *intitem, *bptr;

{
  char integer[10];
  int count, stat;

  memset(integer,0,10);
  count = 0;

	/* find first digit */
  while (!isdigit( buf[ *bptr])) {
    stat = incr(bptr);
    if( stat == 0 ) return 0;
  }

	/* continue until last digit is found: */
  while (isdigit( buf[ *bptr]) != 0) {
    integer[count++] = buf[*bptr];
    stat = incr(bptr);
    if( stat == 0 ) return 0;
  }

  *intitem = atoi(integer);		/* convert string into INT   */
  return 1;
}


/*****************************************************************************/
int FUNCTION incr( a)
/*
 * increment pointer to buffer -- when full, read in a new record
 * and increment recno
 *
 * check if new record is all-blank or -zero, if so declare end
 */
int *a;
{
  int blnk, i, stat;

  (*a)++;
  if ( *a < RECL ) return 1;
  else {
    stat = zvread( iun, buf, "NSAMPS", RECL, NULL);
    *a = 0;
    recno++;
	/* check for empty record: */
    blnk = 1;
    for (i=0; i<RECL; i++)
      if (buf[i] != 0 && buf[i] != ' ') blnk = 0;
    if (blnk) return 0;
    return 1;
  }
}	

$ VOKAGLEVE
$ Return
$!#############################################################################
$PDF_File:
$ create isislab.pdf
process help=*
PARM INP TYPE=(STRING,100)
PARM OPTION KEYWORD VALID=(LABEL,HISTORY,BOTH) DEFAULT=BOTH
END-PROC
.title
VICAR PROGRAM ISISLAB
.help
PURPOSE:

 ISISLAB prints the PDS label and history objects of an ISIS cube to
 the terminal or Vicar log file.

EXECUTION:

  ISISLAB input-file-name OPTION

 where OPTION determines whether the LABEL object, the HISTORY object,
 or both, are printed.

Cognizant programmer: L.W.Kamp
Original Version:  26-Feb-1992
.LEVEL1
.VARI INP
input ISIS cube file
.VARI OPTION
select what is to be
printed
.end
$ Return
$!#############################################################################
$Imake_File:
$ create isislab.imake
#define PROGRAM isislab
#define MODULE_LIST isislab.c 
#define MAIN_LANG_C
#define USES_C
#define LIB_P2SUB
/*#define LIB_LOCAL		/* remove on delivery */
#define DEBUG			/* remove on delivery */
#define LIB_RTL
#define LIB_TAE
#define LIB_FORTRAN
$ Return
$!#############################################################################
$Test_File:
$ create tstisislab.pdf
procedure

refgbl $echo
refgbl $syschar
LOCAL GDIR TYPE=STRING
LOCAL CDIR TYPE=STRING
LOCAL PIC TYPE=STRING

body

let _onfail="continue"
let $echo="yes"     

if ($syschar(1) = "UNIX")
  LET GDIR = "/project/test_work/testdata/gll/"
  LET CDIR = "/project/test_work/testdata/cassini/vims/"
else
  LET GDIR = "WMS_TEST_WORK:[TESTDATA.GLL]"
  LET CDIR = "WMS_TEST_WORK:[TESTDATA.CASSINI.VIMS]"
end-if

! list the label for a NIMS qube, exercising the OPTION parameter:
LET PIC = "&GDIR"//"venus.qub"
ISISLAB &PIC 'both
ISISLAB &PIC 'label
ISISLAB &PIC 'history

! try it for non-NIMS qubes:

! Cassini:
LET PIC = "&CDIR"//"1294648727.013-154945" 
ISISLAB &PIC 

! cube from USGS Flagstaff:
LET PIC = "&GDIR"//"EUROPA_SINU.CUB" 
ISISLAB &PIC 

! and for a NIMS EDR:
! first remove VICAR label, preserving binary headers which
! contain PDS label ...
LET PIC = "&GDIR"//"g7jnfea15003a.15"
label-remove &PIC edr1 'bin
ISISLAB edr1

end-proc
$!-----------------------------------------------------------------------------
$ create tstisislab.log_solos
tstisislab
if ($syschar(1) = "UNIX")
  LET GDIR = "/project/test_work/testdata/gll/"
  LET CDIR = "/project/test_work/testdata/cassini/vims/"
else
end-if
LET PIC = "/project/test_work/testdata/gll/"//"venus.qub"
ISISLAB /project/test_work/testdata/gll/venus.qub 'both
Beginning VICAR task ISISLAB
*** ISISLAB version 3-Nov-05 ***
 
********** LABEL OBJECT **********
 
CSD3ZF0000100000001NJPL3IF0PDS200000001 = SFDU_LABEL

/* File Structure */

RECORD_TYPE = FIXED_LENGTH
RECORD_BYTES = 512
FILE_RECORDS =   6800
LABEL_RECORDS =     26
FILE_STATE = CLEAN

^HISTORY =     27
OBJECT = HISTORY
END_OBJECT = HISTORY

^HISTOGRAM_IMAGE =     50
OBJECT = HISTOGRAM_IMAGE
/* Two dim histogram image structure */
 LINES = 256
 LINE_SAMPLES = 408
 SAMPLE_TYPE = UNSIGNED_INTEGER
 SAMPLE_BITS = 8
 SAMPLE_NAME = BAND
 LINE_NAME = RAW_DATA_NUMBER
 NOTE = "Unannotated two-dimensional histogram 'image'"
END_OBJECT = HISTOGRAM_IMAGE

^QUBE =    254
OBJECT = QUBE

/*  Qube structure: Standard ISIS Cube of NIMS Data */

 AXES = 3
 AXIS_NAME = (SAMPLE,LINE,BAND)

/*  Core description */

 CORE_ITEMS = (266,180,17)
 CORE_ITEM_BYTES = 2
 CORE_ITEM_TYPE = VAX_INTEGER
 CORE_BASE = 0.0
 CORE_MULTIPLIER = 1.0
 CORE_SCALING_NOTE = "True_value = base + (multiplier * stored_value)"
 CORE_VALID_MINIMUM = -32752
 CORE_NULL = -32768
 CORE_LOW_REPR_SATURATION = -32767
 CORE_LOW_INSTR_SATURATION = -32766
 CORE_HIGH_INSTR_SATURATION = -32765
 CORE_HIGH_REPR_SATURATION = -32764
 CORE_BELOW_THRESHOLD = -32762
 CORE_MISSING_SENSITIVITY = -32754
 CORE_NAME = RAW_DATA_NUMBER
 CORE_UNIT = DIMENSIONLESS

 SPATIAL_BINNING_TYPE = NEAREST_AVERAGE
 DARK_UPDATE_TYPE = NOUPDAT
 FILL_BOX_SIZE = 5
 FILL_MIN_VALID_PIXELS = 7
 PHOTOMETRIC_CORRECTION_TYPE = NONE

/*  Suffix description  */

 SUFFIX_BYTES = 4
 SUFFIX_ITEMS = (0,0,9)
 BAND_SUFFIX_NAME = (LATITUDE,LONGITUDE,INCIDENCE_ANGLE,
  EMISSION_ANGLE,PHASE_ANGLE,SLANT_DISTANCE,INTERCEPT_ALTITUDE,
  PHASE_ANGLE_STD_DEV,RAW_DATA_NUMBER_STD_DEV)
 BAND_SUFFIX_UNIT = (DEGREE,DEGREE,DEGREE,DEGREE,DEGREE,KILOMETER,
  KILOMETER,DEGREE,DIMENSIONLESS)
 BAND_SUFFIX_ITEM_BYTES = (4,4,4,4,4,4,4,4,4)
 BAND_SUFFIX_ITEM_TYPE = (VAX_REAL,VAX_REAL,VAX_REAL,VAX_REAL,
     VAX_REAL,VAX_REAL,VAX_REAL,VAX_REAL,VAX_REAL)
 BAND_SUFFIX_BASE = (0.000000,0.000000,0.000000,0.000000,0.000000,
     0.000000,0.000000,0.000000,0.000000)
 BAND_SUFFIX_MULTIPLIER = (1.000000,1.000000,1.000000,1.000000,
     1.000000,1.000000,1.000000,1.000000,1.000000)
 BAND_SUFFIX_VALID_MINIMUM = (16#FFEFFFFF#,16#FFEFFFFF#,16#FFEFFFFF#,
     16#FFEFFFFF#,16#FFEFFFFF#,16#FFEFFFFF#,16#FFEFFFFF#,16#FFEFFFFF#,
     16#FFEFFFFF#)
 BAND_SUFFIX_NULL = (16#FFFFFFFF#,16#FFFFFFFF#,16#FFFFFFFF#,16#FFFFFFFF#,
     16#FFFFFFFF#,16#FFFFFFFF#,16#FFFFFFFF#,16#FFFFFFFF#,16#FFFFFFFF#)
 BAND_SUFFIX_LOW_REPR_SAT = (16#FFFEFFFF#,16#FFFEFFFF#,16#FFFEFFFF#,
     16#FFFEFFFF#,16#FFFEFFFF#,16#FFFEFFFF#,16#FFFEFFFF#,16#FFFEFFFF#,
     16#FFFEFFFF#)
 BAND_SUFFIX_LOW_INSTR_SAT = (16#FFFDFFFF#,16#FFFDFFFF#,16#FFFDFFFF#,
     16#FFFDFFFF#,16#FFFDFFFF#,16#FFFDFFFF#,16#FFFDFFFF#,16#FFFDFFFF#,
     16#FFFDFFFF#)
 BAND_SUFFIX_HIGH_INSTR_SAT = (16#FFFCFFFF#,16#FFFCFFFF#,16#FFFCFFFF#,
     16#FFFCFFFF#,16#FFFCFFFF#,16#FFFCFFFF#,16#FFFCFFFF#,16#FFFCFFFF#,
     16#FFFCFFFF#)
 BAND_SUFFIX_HIGH_REPR_SAT = (16#FFFBFFFF#,16#FFFBFFFF#,16#FFFBFFFF#,
     16#FFFBFFFF#,16#FFFBFFFF#,16#FFFBFFFF#,16#FFFBFFFF#,16#FFFBFFFF#,
     16#FFFBFFFF#)

 BAND_SUFFIX_NOTE          = "
  The backplanes contain 7 geometric parameters, the standard deviation
  of one of them, the standard deviation of a selected data band, and
  0 to 10 'spectral index' bands, each a user-specified function of the
  data bands.  (See the BAND_SUFFIX_NAME values.)

  Longitude ranges from 0 to 360 degrees, with positive direction
  specified by POSITIVE_LONGITUDE_DIRECTION in the IMAGE_MAP_PROJECTION
  group.

  INTERCEPT_ALTITUDE contains values for the DIFFERENCE between
  the length of the normal from the center of the target body to the
  line of sight AND the radius of the target body.  On-target points
  have zero values.  Points beyond the maximum expanded radius have
  null values.  This plane thus also serves as a set of 'off-limb'
  flags.  It is meaningful only for the ORTHOGRAPHIC and 
  POINT_PERSPECTIVE projections; otherwise all values are zero.

  The geometric standard deviation backplane contains the standard
  deviation of the geometry backplane indicated in its NAME, except
  that the special value 16#FFF9FFFF replaces the standard deviation
  where the corresponding core pixels have been 'filled'.

  The data band standard deviation plane is computed for the NIMS data
  band specified by STD_DEV_SELECTED_BAND_NUMBER.  This may be either
  a raw data number, or spectral radiance, whichever is indicated by
  CORE_NAME."

 STD_DEV_SELECTED_BAND_NUMBER = 9

/*  Data description: general */

 DATA_SET_ID = 'GO-V-NIMS-4-MOSAIC-V1.0'
 SPACECRAFT_NAME = GALILEO_ORBITER
 MISSION_PHASE_NAME = VENUS_ENCOUNTER
 INSTRUMENT_NAME = 'NEAR_INFRARED_MAPPING_SPECTROMETER'
 INSTRUMENT_ID = NIMS
 ^INSTRUMENT_DESCRIPTION = "NIMSINST.TXT"

 TARGET_NAME = VENUS
 START_TIME = 1990-02-10T02:11:42Z
 STOP_TIME = 1990-02-10T02:26:13Z
 NATIVE_START_TIME =   180447.39
 NATIVE_STOP_TIME =   180461.71

 OBSERVATION_NAME = 'tstmos'
 OBSERVATION_NOTE = "Venus Partial Disk Imaging #1"
 PRODUCT_ID = "tstmos_LWK01"
 PRODUCT_CREATION_DATE = 1996-06-18
 PRODUCT_NOTE = "standard test case for NIMSCMM"

 IMAGE_ID = (34428,'...',34437)

 INCIDENCE_ANGLE = 163.95
 EMISSION_ANGLE = 27.44
 PHASE_ANGLE = 145.71
 SUB_SOLAR_AZIMUTH = 184.96
 SUB_SPACECRAFT_AZIMUTH = 66.37
 START_SUB_SPACECRAFT_LATITUDE = 12.71
 START_SUB_SPACECRAFT_LONGITUDE =  9.38
 STOP_SUB_SPACECRAFT_LATITUDE = 12.01
 STOP_SUB_SPACECRAFT_LONGITUDE =  8.61
 START_SUB_SOLAR_LATITUDE = -2.64
 START_SUB_SOLAR_LONGITUDE = 139.26
 STOP_SUB_SOLAR_LATITUDE = -2.64
 STOP_SUB_SOLAR_LONGITUDE = 139.23
 MINIMUM_TARGET_CENTER_DISTANCE =  86064.70
 MAXIMUM_TARGET_CENTER_DISTANCE =  93884.90
 MIN_SPACECRAFT_SOLAR_DISTANCE = 1.07612e+08
 MAX_SPACECRAFT_SOLAR_DISTANCE = 1.07617e+08
 SCAN_RATE_TOLERANCE = 3.000000
 MEAN_SCAN_RATE = 1.755030
 SCAN_RATE_NOTE = "The unit of SCAN_RATE_TOLERANCE is mrad/s; the unit
  of MEAN_SCAN_RATE is the Nyquist scanning rate, which depends on the
  instrument mode: it is one-half FOV (0.5 mrad) per grating cycle."

/*  Data description: instrument status  */

 INSTRUMENT_MODE_ID = FIXED_MAP
 GAIN_MODE_ID = 2
 CHOPPER_MODE_ID = REFERENCE
 START_GRATING_POSITION = 16
 OFFSET_GRATING_POSITION = 04
 GRATING_POSITION_INCREMENT = 00
 GRATING_POSITIONS = 01

 MEAN_FOCAL_PLANE_TEMPERATURE = 84.53
 MEAN_RAD_SHIELD_TEMPERATURE = 120.95
 MEAN_TELESCOPE_TEMPERATURE = 132.42
 MEAN_GRATING_TEMPERATURE = 134.65
 MEAN_CHOPPER_TEMPERATURE = 135.43
 MEAN_ELECTRONICS_TEMPERATURE = 277.14

 GROUP = BAND_BIN

/*  Spectral axis description */

  BAND_BIN_CENTER = (0.7989,0.9380,1.1801,1.4583,1.7369,2.0175,
     2.2991,2.5793,2.8648,3.1445,3.4281,3.7109,3.9942,4.2776,
     4.5617,4.8438,5.1264)
  BAND_BIN_UNIT = MICROMETER
  BAND_BIN_ORIGINAL_BAND = (1,2,3,4,5,6,7,8,9,10,11,12,13,14,
     15,16,17)
  BAND_BIN_GRATING_POSITION = (0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
     0,0)
  BAND_BIN_DETECTOR = (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,
     17)
 END_GROUP = BAND_BIN

 GROUP = IMAGE_MAP_PROJECTION
/* Projection description */
  MAP_PROJECTION_TYPE = MERCATOR
  MAP_SCALE = 45.000
  MAP_RESOLUTION =  2.347
  CENTER_LATITUDE =  30.00
  CENTER_LONGITUDE = 330.00
  LINE_PROJECTION_OFFSET =   0.00
  SAMPLE_PROJECTION_OFFSET =   0.00
  MINIMUM_LATITUDE = -45.27
  MAXIMUM_LATITUDE =  15.52
  MINIMUM_LONGITUDE = 277.10
  MAXIMUM_LONGITUDE =  30.00
  COORDINATE_SYSTEM_TYPE = "BODY-FIXED ROTATING"
  POSITIVE_LONGITUDE_DIRECTION = EAST
  A_AXIS_RADIUS = 6051.90
  B_AXIS_RADIUS = 6051.90
  C_AXIS_RADIUS = 6051.90
  SAMPLE_FIRST_PIXEL = 1
  SAMPLE_LAST_PIXEL = 266
  LINE_FIRST_PIXEL = 1
  LINE_LAST_PIXEL = 180

 END_GROUP = IMAGE_MAP_PROJECTION

END_OBJECT = QUBE
END
                                                                                                                                    
                                                                                                                                    
 
********** END OF LABEL OBJECT **********
 
 
********** HISTORY OBJECT **********
 
ROUP = VISIS2

  VERSION_DATE = 1994-04-30
  DATE_TIME = 1996-06-18T16:40:23
  NODE_NAME = "MIPL"
  USER_NAME = "mcneill"
  SOFTWARE_DESC = "ISIS cube file with PDS label has been generated as
    systematic product by MIPL using the following programs:
      NIMSMERGE to create EDRs;
      NIMSCMM to create the merged mosaic & geometry cube;
      HIST2D to create a two-dimensional histogram;
      VISIS2 to create the ISIS cube."

  USERNOTE = "standard test case for NIMSCMM"

  GROUP = PARAMETERS

    EDR_FILE_NAME = "ndat:n0018044737.e"
    POINTING_SOURCE = "AACS"
    SP_KERNEL_FILE_NAME = " "
    I_KERNEL_FILE_NAME = "SPICE$NIMS:NIMS_IKERNEL_MAB.DAT"
    SPIKE_FILE_NAME = "DUMMY_DSPK.DAT"
    BOOM_FILE_NAME = "DUMMY_DBM.DAT"
    DARK_VALUE_FILE_NAME = " "
    CALIBRATION_FILE_NAME = "ndat:venus.calx"
    SOLAR_FLUX_FILE_NAME = " "
    MERGED_MOSAIC_FILE_NAME = "venus.cub"
    STD_DEV_SELECTED_BACKPLANE = 5

    SUMMARY_IMAGE_RED_ID = 20
    SUMMARY_IMAGE_GREEN_ID = 139
    SUMMARY_IMAGE_BLUE_ID = 94
    ADAPT_STRETCH_SAT_FRAC = 0.0500
    ADAPT_STRETCH_SAMP_FRAC = 1.0000
    RED_STRETCH_RANGE = (   10, 1000)
    GREEN_STRETCH_RANGE = (   10, 1000)
    BLUE_STRETCH_RANGE = (   10, 1000)
  END_GROUP = PARAMETERS

END_GROUP = VISIS2

END
                                                                                                                                    
 
********** END OF HISTORY OBJECT **********
 
ISISLAB /project/test_work/testdata/gll/venus.qub 'label
Beginning VICAR task ISISLAB
*** ISISLAB version 3-Nov-05 ***
 
********** LABEL OBJECT **********
 
CSD3ZF0000100000001NJPL3IF0PDS200000001 = SFDU_LABEL

/* File Structure */

RECORD_TYPE = FIXED_LENGTH
RECORD_BYTES = 512
FILE_RECORDS =   6800
LABEL_RECORDS =     26
FILE_STATE = CLEAN

^HISTORY =     27
OBJECT = HISTORY
END_OBJECT = HISTORY

^HISTOGRAM_IMAGE =     50
OBJECT = HISTOGRAM_IMAGE
/* Two dim histogram image structure */
 LINES = 256
 LINE_SAMPLES = 408
 SAMPLE_TYPE = UNSIGNED_INTEGER
 SAMPLE_BITS = 8
 SAMPLE_NAME = BAND
 LINE_NAME = RAW_DATA_NUMBER
 NOTE = "Unannotated two-dimensional histogram 'image'"
END_OBJECT = HISTOGRAM_IMAGE

^QUBE =    254
OBJECT = QUBE

/*  Qube structure: Standard ISIS Cube of NIMS Data */

 AXES = 3
 AXIS_NAME = (SAMPLE,LINE,BAND)

/*  Core description */

 CORE_ITEMS = (266,180,17)
 CORE_ITEM_BYTES = 2
 CORE_ITEM_TYPE = VAX_INTEGER
 CORE_BASE = 0.0
 CORE_MULTIPLIER = 1.0
 CORE_SCALING_NOTE = "True_value = base + (multiplier * stored_value)"
 CORE_VALID_MINIMUM = -32752
 CORE_NULL = -32768
 CORE_LOW_REPR_SATURATION = -32767
 CORE_LOW_INSTR_SATURATION = -32766
 CORE_HIGH_INSTR_SATURATION = -32765
 CORE_HIGH_REPR_SATURATION = -32764
 CORE_BELOW_THRESHOLD = -32762
 CORE_MISSING_SENSITIVITY = -32754
 CORE_NAME = RAW_DATA_NUMBER
 CORE_UNIT = DIMENSIONLESS

 SPATIAL_BINNING_TYPE = NEAREST_AVERAGE
 DARK_UPDATE_TYPE = NOUPDAT
 FILL_BOX_SIZE = 5
 FILL_MIN_VALID_PIXELS = 7
 PHOTOMETRIC_CORRECTION_TYPE = NONE

/*  Suffix description  */

 SUFFIX_BYTES = 4
 SUFFIX_ITEMS = (0,0,9)
 BAND_SUFFIX_NAME = (LATITUDE,LONGITUDE,INCIDENCE_ANGLE,
  EMISSION_ANGLE,PHASE_ANGLE,SLANT_DISTANCE,INTERCEPT_ALTITUDE,
  PHASE_ANGLE_STD_DEV,RAW_DATA_NUMBER_STD_DEV)
 BAND_SUFFIX_UNIT = (DEGREE,DEGREE,DEGREE,DEGREE,DEGREE,KILOMETER,
  KILOMETER,DEGREE,DIMENSIONLESS)
 BAND_SUFFIX_ITEM_BYTES = (4,4,4,4,4,4,4,4,4)
 BAND_SUFFIX_ITEM_TYPE = (VAX_REAL,VAX_REAL,VAX_REAL,VAX_REAL,
     VAX_REAL,VAX_REAL,VAX_REAL,VAX_REAL,VAX_REAL)
 BAND_SUFFIX_BASE = (0.000000,0.000000,0.000000,0.000000,0.000000,
     0.000000,0.000000,0.000000,0.000000)
 BAND_SUFFIX_MULTIPLIER = (1.000000,1.000000,1.000000,1.000000,
     1.000000,1.000000,1.000000,1.000000,1.000000)
 BAND_SUFFIX_VALID_MINIMUM = (16#FFEFFFFF#,16#FFEFFFFF#,16#FFEFFFFF#,
     16#FFEFFFFF#,16#FFEFFFFF#,16#FFEFFFFF#,16#FFEFFFFF#,16#FFEFFFFF#,
     16#FFEFFFFF#)
 BAND_SUFFIX_NULL = (16#FFFFFFFF#,16#FFFFFFFF#,16#FFFFFFFF#,16#FFFFFFFF#,
     16#FFFFFFFF#,16#FFFFFFFF#,16#FFFFFFFF#,16#FFFFFFFF#,16#FFFFFFFF#)
 BAND_SUFFIX_LOW_REPR_SAT = (16#FFFEFFFF#,16#FFFEFFFF#,16#FFFEFFFF#,
     16#FFFEFFFF#,16#FFFEFFFF#,16#FFFEFFFF#,16#FFFEFFFF#,16#FFFEFFFF#,
     16#FFFEFFFF#)
 BAND_SUFFIX_LOW_INSTR_SAT = (16#FFFDFFFF#,16#FFFDFFFF#,16#FFFDFFFF#,
     16#FFFDFFFF#,16#FFFDFFFF#,16#FFFDFFFF#,16#FFFDFFFF#,16#FFFDFFFF#,
     16#FFFDFFFF#)
 BAND_SUFFIX_HIGH_INSTR_SAT = (16#FFFCFFFF#,16#FFFCFFFF#,16#FFFCFFFF#,
     16#FFFCFFFF#,16#FFFCFFFF#,16#FFFCFFFF#,16#FFFCFFFF#,16#FFFCFFFF#,
     16#FFFCFFFF#)
 BAND_SUFFIX_HIGH_REPR_SAT = (16#FFFBFFFF#,16#FFFBFFFF#,16#FFFBFFFF#,
     16#FFFBFFFF#,16#FFFBFFFF#,16#FFFBFFFF#,16#FFFBFFFF#,16#FFFBFFFF#,
     16#FFFBFFFF#)

 BAND_SUFFIX_NOTE          = "
  The backplanes contain 7 geometric parameters, the standard deviation
  of one of them, the standard deviation of a selected data band, and
  0 to 10 'spectral index' bands, each a user-specified function of the
  data bands.  (See the BAND_SUFFIX_NAME values.)

  Longitude ranges from 0 to 360 degrees, with positive direction
  specified by POSITIVE_LONGITUDE_DIRECTION in the IMAGE_MAP_PROJECTION
  group.

  INTERCEPT_ALTITUDE contains values for the DIFFERENCE between
  the length of the normal from the center of the target body to the
  line of sight AND the radius of the target body.  On-target points
  have zero values.  Points beyond the maximum expanded radius have
  null values.  This plane thus also serves as a set of 'off-limb'
  flags.  It is meaningful only for the ORTHOGRAPHIC and 
  POINT_PERSPECTIVE projections; otherwise all values are zero.

  The geometric standard deviation backplane contains the standard
  deviation of the geometry backplane indicated in its NAME, except
  that the special value 16#FFF9FFFF replaces the standard deviation
  where the corresponding core pixels have been 'filled'.

  The data band standard deviation plane is computed for the NIMS data
  band specified by STD_DEV_SELECTED_BAND_NUMBER.  This may be either
  a raw data number, or spectral radiance, whichever is indicated by
  CORE_NAME."

 STD_DEV_SELECTED_BAND_NUMBER = 9

/*  Data description: general */

 DATA_SET_ID = 'GO-V-NIMS-4-MOSAIC-V1.0'
 SPACECRAFT_NAME = GALILEO_ORBITER
 MISSION_PHASE_NAME = VENUS_ENCOUNTER
 INSTRUMENT_NAME = 'NEAR_INFRARED_MAPPING_SPECTROMETER'
 INSTRUMENT_ID = NIMS
 ^INSTRUMENT_DESCRIPTION = "NIMSINST.TXT"

 TARGET_NAME = VENUS
 START_TIME = 1990-02-10T02:11:42Z
 STOP_TIME = 1990-02-10T02:26:13Z
 NATIVE_START_TIME =   180447.39
 NATIVE_STOP_TIME =   180461.71

 OBSERVATION_NAME = 'tstmos'
 OBSERVATION_NOTE = "Venus Partial Disk Imaging #1"
 PRODUCT_ID = "tstmos_LWK01"
 PRODUCT_CREATION_DATE = 1996-06-18
 PRODUCT_NOTE = "standard test case for NIMSCMM"

 IMAGE_ID = (34428,'...',34437)

 INCIDENCE_ANGLE = 163.95
 EMISSION_ANGLE = 27.44
 PHASE_ANGLE = 145.71
 SUB_SOLAR_AZIMUTH = 184.96
 SUB_SPACECRAFT_AZIMUTH = 66.37
 START_SUB_SPACECRAFT_LATITUDE = 12.71
 START_SUB_SPACECRAFT_LONGITUDE =  9.38
 STOP_SUB_SPACECRAFT_LATITUDE = 12.01
 STOP_SUB_SPACECRAFT_LONGITUDE =  8.61
 START_SUB_SOLAR_LATITUDE = -2.64
 START_SUB_SOLAR_LONGITUDE = 139.26
 STOP_SUB_SOLAR_LATITUDE = -2.64
 STOP_SUB_SOLAR_LONGITUDE = 139.23
 MINIMUM_TARGET_CENTER_DISTANCE =  86064.70
 MAXIMUM_TARGET_CENTER_DISTANCE =  93884.90
 MIN_SPACECRAFT_SOLAR_DISTANCE = 1.07612e+08
 MAX_SPACECRAFT_SOLAR_DISTANCE = 1.07617e+08
 SCAN_RATE_TOLERANCE = 3.000000
 MEAN_SCAN_RATE = 1.755030
 SCAN_RATE_NOTE = "The unit of SCAN_RATE_TOLERANCE is mrad/s; the unit
  of MEAN_SCAN_RATE is the Nyquist scanning rate, which depends on the
  instrument mode: it is one-half FOV (0.5 mrad) per grating cycle."

/*  Data description: instrument status  */

 INSTRUMENT_MODE_ID = FIXED_MAP
 GAIN_MODE_ID = 2
 CHOPPER_MODE_ID = REFERENCE
 START_GRATING_POSITION = 16
 OFFSET_GRATING_POSITION = 04
 GRATING_POSITION_INCREMENT = 00
 GRATING_POSITIONS = 01

 MEAN_FOCAL_PLANE_TEMPERATURE = 84.53
 MEAN_RAD_SHIELD_TEMPERATURE = 120.95
 MEAN_TELESCOPE_TEMPERATURE = 132.42
 MEAN_GRATING_TEMPERATURE = 134.65
 MEAN_CHOPPER_TEMPERATURE = 135.43
 MEAN_ELECTRONICS_TEMPERATURE = 277.14

 GROUP = BAND_BIN

/*  Spectral axis description */

  BAND_BIN_CENTER = (0.7989,0.9380,1.1801,1.4583,1.7369,2.0175,
     2.2991,2.5793,2.8648,3.1445,3.4281,3.7109,3.9942,4.2776,
     4.5617,4.8438,5.1264)
  BAND_BIN_UNIT = MICROMETER
  BAND_BIN_ORIGINAL_BAND = (1,2,3,4,5,6,7,8,9,10,11,12,13,14,
     15,16,17)
  BAND_BIN_GRATING_POSITION = (0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
     0,0)
  BAND_BIN_DETECTOR = (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,
     17)
 END_GROUP = BAND_BIN

 GROUP = IMAGE_MAP_PROJECTION
/* Projection description */
  MAP_PROJECTION_TYPE = MERCATOR
  MAP_SCALE = 45.000
  MAP_RESOLUTION =  2.347
  CENTER_LATITUDE =  30.00
  CENTER_LONGITUDE = 330.00
  LINE_PROJECTION_OFFSET =   0.00
  SAMPLE_PROJECTION_OFFSET =   0.00
  MINIMUM_LATITUDE = -45.27
  MAXIMUM_LATITUDE =  15.52
  MINIMUM_LONGITUDE = 277.10
  MAXIMUM_LONGITUDE =  30.00
  COORDINATE_SYSTEM_TYPE = "BODY-FIXED ROTATING"
  POSITIVE_LONGITUDE_DIRECTION = EAST
  A_AXIS_RADIUS = 6051.90
  B_AXIS_RADIUS = 6051.90
  C_AXIS_RADIUS = 6051.90
  SAMPLE_FIRST_PIXEL = 1
  SAMPLE_LAST_PIXEL = 266
  LINE_FIRST_PIXEL = 1
  LINE_LAST_PIXEL = 180

 END_GROUP = IMAGE_MAP_PROJECTION

END_OBJECT = QUBE
END
                                                                                                                                    
                                                                                                                                    
 
********** END OF LABEL OBJECT **********
 
ISISLAB /project/test_work/testdata/gll/venus.qub 'history
Beginning VICAR task ISISLAB
*** ISISLAB version 3-Nov-05 ***
 
********* HISTORY OBJECT **********
 
ROUP = VISIS2

  VERSION_DATE = 1994-04-30
  DATE_TIME = 1996-06-18T16:40:23
  NODE_NAME = "MIPL"
  USER_NAME = "mcneill"
  SOFTWARE_DESC = "ISIS cube file with PDS label has been generated as
    systematic product by MIPL using the following programs:
      NIMSMERGE to create EDRs;
      NIMSCMM to create the merged mosaic & geometry cube;
      HIST2D to create a two-dimensional histogram;
      VISIS2 to create the ISIS cube."

  USERNOTE = "standard test case for NIMSCMM"

  GROUP = PARAMETERS

    EDR_FILE_NAME = "ndat:n0018044737.e"
    POINTING_SOURCE = "AACS"
    SP_KERNEL_FILE_NAME = " "
    I_KERNEL_FILE_NAME = "SPICE$NIMS:NIMS_IKERNEL_MAB.DAT"
    SPIKE_FILE_NAME = "DUMMY_DSPK.DAT"
    BOOM_FILE_NAME = "DUMMY_DBM.DAT"
    DARK_VALUE_FILE_NAME = " "
    CALIBRATION_FILE_NAME = "ndat:venus.calx"
    SOLAR_FLUX_FILE_NAME = " "
    MERGED_MOSAIC_FILE_NAME = "venus.cub"
    STD_DEV_SELECTED_BACKPLANE = 5

    SUMMARY_IMAGE_RED_ID = 20
    SUMMARY_IMAGE_GREEN_ID = 139
    SUMMARY_IMAGE_BLUE_ID = 94
    ADAPT_STRETCH_SAT_FRAC = 0.0500
    ADAPT_STRETCH_SAMP_FRAC = 1.0000
    RED_STRETCH_RANGE = (   10, 1000)
    GREEN_STRETCH_RANGE = (   10, 1000)
    BLUE_STRETCH_RANGE = (   10, 1000)
  END_GROUP = PARAMETERS

END_GROUP = VISIS2

END
                                                                                                                                    
 
********** END OF HISTORY OBJECT **********
 
LET PIC = "/project/test_work/testdata/cassini/vims/"//"1294648727.013-154945"
ISISLAB /project/test_work/testdata/cassini/vims/1294648727.013-154945
Beginning VICAR task ISISLAB
*** ISISLAB version 3-Nov-05 ***
 
********** LABEL OBJECT **********
 
CSD3ZF0000100000001NJPL3IF0PDS200000001 = SFDU_LABEL

/* File Structure */

RECORD_TYPE = FIXED_LENGTH
RECORD_BYTES = 512
FILE_RECORDS = 322
LABEL_RECORDS = 32
FILE_STATE = CLEAN

^HISTORY = 33
OBJECT = HISTORY
END_OBJECT = HISTORY

^QUBE = 59
OBJECT = QUBE
   AXES = 3
   AXIS_NAME = (SAMPLE,BAND,LINE)
   CORE_ITEMS = (1,352,64)
   CORE_ITEM_BYTES = 2
   CORE_ITEM_TYPE = SUN_INTEGER
   CORE_BASE = 0.0
   CORE_MULTIPLIER = 1.0
   CORE_VALID_MINIMUM = -4095
   CORE_NULL = -8192
   CORE_LOW_REPR_SATURATION = -32767
   CORE_LOW_INSTR_SATURATION = -32766
   CORE_HIGH_REPR_SATURATION = -32764
   CORE_HIGH_INSTR_SATURATION = -32765
   CORE_MINIMUM_DN = -1162
   CORE_NAME = RAW_DATA_NUMBERS
   CORE_UNIT = DN
   SUFFIX_ITEMS = (1,0,0)
   SUFFIX_BYTES = 4
   SAMPLE_SUFFIX_NAME = BACKGROUND
   SAMPLE_SUFFIX_UNIT = DN
   SAMPLE_SUFFIX_ITEM_BYTES = 2
   SAMPLE_SUFFIX_ITEM_TYPE = SUN_INTEGER
   SAMPLE_SUFFIX_BASE = 0.0
   SAMPLE_SUFFIX_MULTIPLIER = 1.0
   SAMPLE_SUFFIX_VALID_MINIMUM = 0
   SAMPLE_SUFFIX_NULL = -8192
   SAMPLE_SUFFIX_LOW_REPR_SAT = -32767
   SAMPLE_SUFFIX_LOW_INSTR_SAT = -32766
   SAMPLE_SUFFIX_HIGH_REPR_SAT = -32765
   SAMPLE_SUFFIX_HIGH_INSTR_SAT = -32764
   SAMPLE_SUFFIX_NOTE = "If visible channel enabled, bands 1 to swath width
      contain swath width visible background words, and bands 65 through
      67 contain the visible sine value, visible cosine value, and visible
      motor current value. If ir channel enabled, bands 97 to 352 contain
      256 ir background data words."
   SPACECRAFT_NAME = "CASSINI"
   MISSION_PHASE_NAME = "CHECK_OUT"
   INSTRUMENT_NAME = "Visual and Infrared Mapping Spectrometer"
   INSTRUMENT_ID = "VIMS"
   SOFTWARE_VERSION_ID = "2.6.3"
   TARGET_NAME = "SKY"
   NATIVE_START_TIME = 10357189632
   NATIVE_END_TIME = 10357189632
   EVENT_START_TIME = "1999-010T08:34:04.625"
   EVENT_END_TIME = "1999-010T08:34:31.624" 
   OBSERVATION_ID = "ICO_QUIET_3"
   INSTRUMENT_MODE_ID = "POINT"
   IR_CHANNEL_STATE_FLAG = "ON"
   IR_GAIN_MODE_ID = "LOW"
   IR_BIAS_MODE_ID = "LOW"
   IR_SHUTTER_STATE_FLAG = "ENABLED"
   IR_EXPOSURE_DURATION = 80.0
   IR_INTEGRATION_DELAY_FLAG = "DISABLED"
   IR_INTERLINE_DELAY_DURATION = 2900.0
   IR_BACKGROUND_MODE_ID = "AVG2"
   IR_BACKGROUND_FREQUENCY = 1
   IR_DETECTOR_TEMPERATURE_LOW_RES = 145.19
   IR_DETECTOR_TEMPERATURE_HI_RES = 137.17
   IR_PRIMARY_OPTICS_TEMPERATURE = 169.60
   IR_SECONDARY_OPTICS_TEMPERATURE = 166.15
   IR_SHIELD_TEMPERATURE = 158.60
   IR_GRATING_TEMPERATURE = 164.46
   VIS_CHANNEL_STATE_FLAG = "ON"
   VIS_GAIN_MODE_ID = "HIGH"
   VIS_EXPOSURE_DURATION = 160.0
   VIS_ANTIBLOOMING_STATE_FLAG = "DISABLED"
   VIS_DETECTOR_TEMPERATURE = -999.
   VIS_OPTICS_TEMPERATURE = -999.
   SCAN_MODE_ID = "BOTH"
   DATA_REGION = (1,1,1,1,64,352)
   INTERFRAME_DELAY_DURATION = 20.0
   ENCODING_TYPE = "NOTCOMP"
   COMPRESSION_RATIO = -999.
   DATA_BUFFER_STATE_FLAG = "ENABLED"
   INSTRUMENT_DATA_RATE = 8.192
   MISSING_PIXELS = 0      

  GROUP = BAND_BIN
   BAND_BIN_CENTER = (0.3500000,0.3573684,0.3647368,0.3721053,0.3794737,
     0.3868421,0.3942105,0.4015789,0.4089474,0.4163158,0.4236842,0.4310526,
     0.4384211,0.4457895,0.4531579,0.4605263,0.4678947,0.4752632,0.4826316,
     0.4900000,0.4973684,0.5047368,0.5121053,0.5194737,0.5268421,0.5342105,
     0.5415789,0.5489474,0.5563158,0.5636842,0.5710526,0.5784211,0.5857895,
     0.5931579,0.6005263,0.6078947,0.6152632,0.6226316,0.6300000,0.6373684,
     0.6447368,0.6521053,0.6594737,0.6668421,0.6742105,0.6815789,0.6889474,
     0.6963158,0.7036842,0.7110526,0.7184211,0.7257895,0.7331579,0.7405263,
     0.7478947,0.7552632,0.7626316,0.7700000,0.7773684,0.7847368,0.7921053,
     0.7994737,0.8068421,0.8142105,0.8215789,0.8289474,0.8363158,0.8436842,
     0.8510526,0.8584211,0.8657895,0.8731579,0.8805263,0.8878947,0.8952632,
     0.9026316,0.9100000,0.9173684,0.9247368,0.9321053,0.9394737,0.9468421,
     0.9542105,0.9615789,0.9689474,0.9763158,0.9836842,0.9910526,0.9984211,
     1.0057895,1.0131579,1.0205263,1.0278947,1.0352632,1.0426316,1.0500000,
     0.8500000,0.8666667,0.8833333,0.9000000,0.9166667,0.9333333,0.9500000,
     0.9666667,0.9833333,1.0000000,1.0166667,1.0333333,1.0500000,1.0666667,
     1.0833333,1.1000000,1.1166667,1.1333333,1.1500000,1.1666667,1.1833333,
     1.2000000,1.2166667,1.2333333,1.2500000,1.2666667,1.2833333,1.3000000,
     1.3166667,1.3333333,1.3500000,1.3666667,1.3833333,1.4000000,1.4166667,
     1.4333333,1.4500000,1.4666667,1.4833333,1.5000000,1.5166667,1.5333333,
     1.5500000,1.5666667,1.5833333,1.6000000,1.6166667,1.6333333,1.6500000,
     1.6666667,1.6833333,1.7000000,1.7166667,1.7333333,1.7500000,1.7666667,
     1.7833333,1.8000000,1.8166667,1.8333333,1.8500000,1.8666667,1.8833333,
     1.9000000,1.9166667,1.9333333,1.9500000,1.9666667,1.9833333,2.0000000,
     2.0166667,2.0333333,2.0500000,2.0666667,2.0833333,2.1000000,2.1166667,
     2.1333333,2.1500000,2.1666667,2.1833333,2.2000000,2.2166667,2.2333333,
     2.2500000,2.2666667,2.2833333,2.3000000,2.3166667,2.3333333,2.3500000,
     2.3666667,2.3833333,2.4000000,2.4166667,2.4333333,2.4500000,2.4666667,
     2.4833333,2.5000000,2.5166667,2.5333333,2.5500000,2.5666667,2.5833333,
     2.6000000,2.6166667,2.6333333,2.6500000,2.6666667,2.6833333,2.7000000,
     2.7166667,2.7333333,2.7500000,2.7666667,2.7833333,2.8000000,2.8166667,
     2.8333333,2.8500000,2.8666667,2.8833333,2.9000000,2.9166667,2.9333333,
     2.9500000,2.9666667,2.9833333,3.0000000,3.0166667,3.0333333,3.0500000,
     3.0666667,3.0833333,3.1000000,3.1166667,3.1333333,3.1500000,3.1666667,
     3.1833333,3.2000000,3.2166667,3.2333333,3.2500000,3.2666667,3.2833333,
     3.3000000,3.3166667,3.3333333,3.3500000,3.3666667,3.3833333,3.4000000,
     3.4166667,3.4333333,3.4500000,3.4666667,3.4833333,3.5000000,3.5166667,
     3.5333333,3.5500000,3.5666667,3.5833333,3.6000000,3.6166667,3.6333333,
     3.6500000,3.6666667,3.6833333,3.7000000,3.7166667,3.7333333,3.7500000,
     3.7666667,3.7833333,3.8000000,3.8166667,3.8333333,3.8500000,3.8666667,
     3.8833333,3.9000000,3.9166667,3.9333333,3.9500000,3.9666667,3.9833333,
     4.0000000,4.0166667,4.0333333,4.0500000,4.0666667,4.0833333,4.1000000,
     4.1166667,4.1333333,4.1500000,4.1666667,4.1833333,4.2000000,4.2166667,
     4.2333333,4.2500000,4.2666667,4.2833333,4.3000000,4.3166667,4.3333333,
     4.3500000,4.3666667,4.3833333,4.4000000,4.4166667,4.4333333,4.4500000,
     4.4666667,4.4833333,4.5000000,4.5166667,4.5333333,4.5500000,4.5666667,
     4.5833333,4.6000000,4.6166667,4.6333333,4.6500000,4.6666667,4.6833333,
     4.7000000,4.7166667,4.7333333,4.7500000,4.7666667,4.7833333,4.8000000,
     4.8166667,4.8333333,4.8500000,4.8666667,4.8833333,4.9000000,4.9166667,
     4.9333333,4.9500000,4.9666667,4.9833333,5.0000000,5.0166667,5.0333333,
     5.0500000,5.0666667,5.0833333,5.1000000)
   BAND_BIN_UNIT = MICROMETER
   BAND_BIN_ORIGINAL_BAND = (0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,
     20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,
     45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,
     70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,
     95,96,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,
     115,116,117,118,119,120,121,122,123,124,125,126,127,128,129,130,131,132,
     133,134,135,136,137,138,139,140,141,142,143,144,145,146,147,148,149,150,
     151,152,153,154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,
     169,170,171,172,173,174,175,176,177,178,179,180,181,182,183,184,185,186,
     187,188,189,190,191,192,193,194,195,196,197,198,199,200,201,202,203,204,
     205,206,207,208,209,210,211,212,213,214,215,216,217,218,219,220,221,222,
     223,224,225,226,227,228,229,230,231,232,233,234,235,236,237,238,239,240,
     241,242,243,244,245,246,247,248,249,250,251,252,253,254,255,256,257,258,
     259,260,261,262,263,264,265,266,267,268,269,270,271,272,273,274,275,276,
     277,278,279,280,281,282,283,284,285,286,287,288,289,290,291,292,293,294,
     295,296,297,298,299,300,301,302,303,304,305,306,307,308,309,310,311,312,
     313,314,315,316,317,318,319,320,321,322,323,324,325,326,327,328,329,330,
     331,332,333,334,335,336,337,338,339,340,341,342,343,344,345,346,347,348,
     349,350,351)
  END_GROUP = BAND_BIN

  GROUP = VOLATILE_INFO
    CORE_MINIMUM = -1567.0000000
    CORE_MAXIMUM = 1889.0000000
  END_GROUP = VOLATILE_INFO
END_OBJECT = QUBE
END
 
********** END OF LABEL OBJECT **********
 
 
********** HISTORY OBJECT **********
 
ND
                                                                                                                                    
                                                                                                                                    
                                                                                                                                    
 
********** END OF HISTORY OBJECT **********
 
LET PIC = "/project/test_work/testdata/gll/"//"EUROPA_SINU.CUB"
ISISLAB /project/test_work/testdata/gll/EUROPA_SINU.CUB
Beginning VICAR task ISISLAB
*** ISISLAB version 3-Nov-05 ***
 
********** LABEL OBJECT **********
 
CSD3ZF0000100000001NJPL3IF0PDS200000001 = SFDU_LABEL

/* File Structure */

RECORD_TYPE = FIXED_LENGTH
RECORD_BYTES = 512
FILE_RECORDS = 5959
LABEL_RECORDS = 36
FILE_STATE = CLEAN

^HISTORY = 37
OBJECT = HISTORY
END_OBJECT = HISTORY

^QUBE = 86
OBJECT = QUBE
/* Qube object description */
/* Qube structure */
  AXES = 3
  AXIS_NAME = (SAMPLE,LINE,BAND)

/* Core description */
  CORE_ITEMS = (2453,1226,1)
  CORE_ITEM_BYTES = 1
  CORE_ITEM_TYPE = SUN_UNSIGNED_INTEGER
  CORE_BASE = 0.0
  CORE_MULTIPLIER = 1.0
/* "true_value" = base + (multiplier * stored_value) */
  CORE_VALID_MINIMUM = 1
  CORE_NULL = 0
  CORE_LOW_REPR_SATURATION = 0
  CORE_LOW_INSTR_SATURATION = 0
  CORE_HIGH_INSTR_SATURATION = 255
  CORE_HIGH_REPR_SATURATION = 255

/* Suffix description */
  SUFFIX_BYTES = 4
  SUFFIX_ITEMS = (0,0,0)
  CORE_NAME = RAW_DATA_NUMBER
  CORE_UNIT = NONE

  SPACECRAFT_NAME = VOYAGER_1
  IMAGE_NUMBER = "   1632318"
  TARGET_NAME = EUROPA
  INSTRUMENT_ID = "NA"
  CAL_TARGET_CODE = "J"
  CAMERA_STATE_1 = "1"
  CAMERA_STATE_2 = "0"
  CAMERA_STATE_3 = "1"
  EXPOSURE_DURATION = 0.1800000
  NO_CAMERA_STATE = 3
  PRODUCT_ID = "1433J1-003"
  START_TIME = "1979-03-03T07:11:47.925Z"
  COORDINATE_SYSTEM_NAME = 2000
  INSTRUMENT_DECLINATION = 21.1508961
  INSTRUMENT_RIGHT_ASCENSION = 110.7582626
  INSTRUMENT_TWIST = 17.6460400
  PLANET_DECLINATION = 64.3713379
  PLANET_RIGHT_ASCENSION = 269.1572571
  PLANET_ROTATION = 134.1985474
  SC_TARGET_POSITION_VECTOR = (812041.8750000,-2153385.2500000,-890739.0000000)
  SC_SUN_POSITION_VECTOR = (480145376.0000000,-573756032.0000000,
    -257657952.0000000)
  FILTER_NAME = "0"
  RESEAU_LINE = (3.0000000,-1.0000000,1.0000000,0.0000000,-2.0000000,4.0000000,
    4.0000000,4.0000000,4.0000000,7.0000000,3.0000000,16.0000000,15.0000000,
    22.0000000,21.0000000,21.0000000,21.0000000,21.0000000,23.0000000,
    24.0000000,26.0000000,29.0000000,26.0000000,51.0000000,50.0000000,
    49.0000000,49.0000000,49.0000000,50.0000000,50.0000000,51.0000000,
    53.0000000,55.0000000,59.0000000,61.0000000,88.0000000,87.0000000,
    87.0000000,88.0000000,88.0000000,89.0000000,89.0000000,90.0000000,
    91.0000000,94.0000000,97.0000000,126.0000000,126.0000000,133.0000000,
    133.0000000,165.0000000,165.0000000,166.0000000,166.0000000,167.0000000,
    167.0000000,168.0000000,168.0000000,169.0000000,170.0000000,172.0000000,
    203.0000000,204.0000000,210.0000000,209.0000000,244.0000000,244.0000000,
    245.0000000,246.0000000,246.0000000,246.0000000,247.0000000,247.0000000,
    247.0000000,248.0000000,250.0000000,281.0000000,284.0000000,288.0000000,
    287.0000000,323.0000000,324.0000000,324.0000000,325.0000000,325.0000000,
    325.0000000,326.0000000,326.0000000,326.0000000,327.0000000,328.0000000,
    360.0000000,363.0000000,367.0000000,366.0000000,402.0000000,403.0000000,
    404.0000000,403.0000000,404.0000000,405.0000000,405.0000000,405.0000000,
    405.0000000,406.0000000,407.0000000,439.0000000,442.0000000,446.0000000,
    443.0000000,481.0000000,482.0000000,483.0000000,482.0000000,483.0000000,
    484.0000000,484.0000000,484.0000000,484.0000000,485.0000000,486.0000000,
    517.0000000,521.0000000,525.0000000,522.0000000,559.0000000,561.0000000,
    562.0000000,562.0000000,562.0000000,563.0000000,563.0000000,563.0000000,
    563.0000000,564.0000000,565.0000000,596.0000000,599.0000000,604.0000000,
    600.0000000,636.0000000,638.0000000,639.0000000,640.0000000,640.0000000,
    641.0000000,641.0000000,641.0000000,642.0000000,642.0000000,643.0000000,
    673.0000000,675.0000000,681.0000000,681.0000000,711.0000000,714.0000000,
    716.0000000,717.0000000,718.0000000,718.0000000,719.0000000,719.0000000,
    719.0000000,719.0000000,719.0000000,745.0000000,748.0000000,752.0000000,
    754.0000000,755.0000000,756.0000000,757.0000000,757.0000000,757.0000000,
    757.0000000,757.0000000,754.0000000,780.0000000,777.0000000,780.0000000,
    782.0000000,784.0000000,784.0000000,785.0000000,785.0000000,785.0000000,
    785.0000000,793.0000000,789.0000000,800.0000000,799.0000000,800.0000000,
    800.0000000,801.0000000,802.0000000,802.0000000,803.0000000,801.0000000,
    803.0000000,800.0000000,129.0000000)
  RESEAU_SAMPLE = (12.0000000,50.0000000,126.0000000,204.0000000,281.0000000,
    357.0000000,437.0000000,515.0000000,599.0000000,676.0000000,750.0000000,
    796.0000000,22.0000000,93.0000000,168.0000000,245.0000000,323.0000000,
    402.0000000,480.0000000,559.0000000,637.0000000,715.0000000,788.0000000,
    5.0000000,55.0000000,129.0000000,206.0000000,283.0000000,362.0000000,
    441.0000000,519.0000000,598.0000000,676.0000000,753.0000000,799.0000000,
    27.0000000,90.0000000,166.0000000,244.0000000,322.0000000,401.0000000,
    480.0000000,559.0000000,637.0000000,715.0000000,781.0000000,5.0000000,
    52.0000000,753.0000000,799.0000000,24.0000000,89.0000000,166.0000000,
    244.0000000,322.0000000,401.0000000,480.0000000,558.0000000,637.0000000,
    715.0000000,781.0000000,3.0000000,51.0000000,753.0000000,799.0000000,
    23.0000000,88.0000000,166.0000000,244.0000000,322.0000000,401.0000000,
    480.0000000,559.0000000,637.0000000,715.0000000,782.0000000,0.0000000,
    50.0000000,753.0000000,801.0000000,22.0000000,88.0000000,166.0000000,
    244.0000000,323.0000000,402.0000000,480.0000000,559.0000000,637.0000000,
    715.0000000,782.0000000,0.0000000,50.0000000,754.0000000,802.0000000,
    22.0000000,88.0000000,166.0000000,245.0000000,324.0000000,402.0000000,
    481.0000000,560.0000000,638.0000000,715.0000000,782.0000000,0.0000000,
    50.0000000,754.0000000,801.0000000,22.0000000,89.0000000,166.0000000,
    245.0000000,325.0000000,403.0000000,482.0000000,560.0000000,638.0000000,
    716.0000000,782.0000000,0.0000000,51.0000000,754.0000000,801.0000000,
    23.0000000,89.0000000,167.0000000,246.0000000,325.0000000,403.0000000,
    482.0000000,561.0000000,639.0000000,716.0000000,782.0000000,2.0000000,
    51.0000000,754.0000000,799.0000000,24.0000000,90.0000000,168.0000000,
    246.0000000,325.0000000,404.0000000,483.0000000,561.0000000,639.0000000,
    716.0000000,781.0000000,3.0000000,52.0000000,753.0000000,799.0000000,
    25.0000000,91.0000000,168.0000000,247.0000000,326.0000000,404.0000000,
    483.0000000,561.0000000,639.0000000,715.0000000,779.0000000,3.0000000,
    54.0000000,130.0000000,208.0000000,286.0000000,365.0000000,444.0000000,
    522.0000000,600.0000000,676.0000000,750.0000000,800.0000000,19.0000000,
    92.0000000,169.0000000,247.0000000,326.0000000,405.0000000,483.0000000,
    561.0000000,638.0000000,713.0000000,783.0000000,10.0000000,56.0000000,
    130.0000000,209.0000000,284.0000000,363.0000000,441.0000000,519.0000000,
    597.0000000,673.0000000,746.0000000,793.0000000,598.0000000)
  RESEAU_TYPE = (5,0,5,0,0,2,2,2,2,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,
    5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,4,5,5,5,5,5,5,5,
    5,5,5,5,5,5,5,0,5,5,0,5,5,5,5,5,5,5,5,5,5,5,0,5,5,0,5,5,5,5,5,5,5,5,5,5,5,0,
    5,5,0,5,5,5,5,5,5,5,5,5,5,5,0,5,5,0,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,
    5,5,5,5,5,5,5,5,5,6,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,
    5,5,5,5,5,5,5,5,8,5,5,0,0,0,0,0,0,5,5)
  RESEAU_VALID = (0.0000000,0.0000000,0.0000000,0.0000000,0.0000000,1.0000000,
    1.0000000,1.0000000,1.0000000,1.0000000,0.0000000,0.0000000,1.0000000,
    1.0000000,1.0000000,1.0000000,1.0000000,1.0000000,1.0000000,1.0000000,
    1.0000000,1.0000000,1.0000000,1.0000000,1.0000000,1.0000000,1.0000000,
    1.0000000,1.0000000,1.0000000,1.0000000,1.0000000,1.0000000,1.0000000,
    0.0000000,1.0000000,1.0000000,1.0000000,1.0000000,1.0000000,1.0000000,
    1.0000000,1.0000000,1.0000000,1.0000000,1.0000000,1.0000000,1.0000000,
    1.0000000,0.0000000,1.0000000,1.0000000,1.0000000,1.0000000,1.0000000,
    1.0000000,1.0000000,1.0000000,1.0000000,1.0000000,1.0000000,1.0000000,
    1.0000000,1.0000000,0.0000000,1.0000000,1.0000000,1.0000000,1.0000000,
    1.0000000,1.0000000,1.0000000,1.0000000,1.0000000,1.0000000,1.0000000,
    0.0000000,1.0000000,1.0000000,0.0000000,1.0000000,1.0000000,1.0000000,
    1.0000000,1.0000000,1.0000000,1.0000000,1.0000000,1.0000000,1.0000000,
    1.0000000,0.0000000,1.0000000,1.0000000,0.0000000,1.0000000,1.0000000,
    1.0000000,0.0000000,1.0000000,1.0000000,1.0000000,1.0000000,1.0000000,
    1.0000000,1.0000000,0.0000000,1.0000000,1.0000000,0.0000000,1.0000000,
    1.0000000,1.0000000,1.0000000,1.0000000,1.0000000,1.0000000,1.0000000,
    1.0000000,1.0000000,1.0000000,0.0000000,1.0000000,1.0000000,0.0000000,
    1.0000000,1.0000000,1.0000000,1.0000000,1.0000000,1.0000000,1.0000000,
    1.0000000,1.0000000,1.0000000,1.0000000,0.0000000,1.0000000,1.0000000,
    0.0000000,1.0000000,1.0000000,1.0000000,1.0000000,1.0000000,1.0000000,
    1.0000000,1.0000000,1.0000000,1.0000000,1.0000000,1.0000000,1.0000000,
    1.0000000,1.0000000,1.0000000,1.0000000,1.0000000,1.0000000,1.0000000,
    1.0000000,1.0000000,1.0000000,1.0000000,1.0000000,1.0000000,1.0000000,
    1.0000000,1.0000000,1.0000000,1.0000000,1.0000000,1.0000000,1.0000000,
    1.0000000,1.0000000,1.0000000,0.0000000,1.0000000,1.0000000,1.0000000,
    1.0000000,1.0000000,1.0000000,1.0000000,1.0000000,1.0000000,1.0000000,
    1.0000000,1.0000000,0.0000000,1.0000000,0.0000000,0.0000000,0.0000000,
    0.0000000,0.0000000,0.0000000,0.0000000,0.0000000,0.0000000,1.0000000)
  RAW_TIE_POINT_1 = "   2.86000-155.82001     467.0     241.0       0.0"
  MATCH_POINT_1 = "     470.0     271.0   1634216     468.0     251.0"
  MATCH_POINT_2 = "     453.0     263.0   1634216     450.0     244.0"
  MATCH_POINT_3 = "     506.0     277.0   1634216     500.0     275.0"






  GROUP = BAND_BIN
    BAND_BIN_UNIT = NONE
    BAND_BIN_ORIGINAL_BAND = 1
    BAND_BIN_CENTER = 1.000
  END_GROUP = BAND_BIN

  GROUP = IMAGE_MAP_PROJECTION
    A_AXIS_RADIUS = 1561.5000000
    B_AXIS_RADIUS = 1561.5000000
    C_AXIS_RADIUS = 1561.5000000
    POSITIVE_LONGITUDE_DIRECTION = WEST
    MAP_PROJECTION_TYPE = 'SINUSOIDAL_EQUAL-AREA'
    MINIMUM_LATITUDE = -90.0000000
    MAXIMUM_LATITUDE = 90.0000000
    EASTERNMOST_LONGITUDE = 0.0000000
    WESTERNMOST_LONGITUDE = 360.0000000
    MAP_SCALE = 4.0000000
    MAP_RESOLUTION = 6.8133292
    LINE_PROJECTION_OFFSET = -612.6996161
    SAMPLE_PROJECTION_OFFSET = -1225.8992321
    REFERENCE_LATITUDE = 0.0000000
    REFERENCE_LONGITUDE = 0.0000000
    MAP_PROJECTION_ROTATION = 0.0000000
    CENTER_LONGITUDE = 180.0000000
    CENTER_LATITUDE = 90.0000000
  END_GROUP = IMAGE_MAP_PROJECTION
END_OBJECT = QUBE
END
 
********** END OF LABEL OBJECT **********
 
 
********** HISTORY OBJECT **********
 

GROUP = CPYLAB
  VERSION_DATE = 1996-02-08
  DATE_TIME = 1996-04-16T15:18:00
  NODE_NAME = "tycho"
  USER_NAME = "tbecker"
  SOFTWARE_DESC = "Copy keyword labels from one file to another"
  GROUP = PARAMETERS
    FROM = "north_msk8.cub"
    TO = "north_final.cub"
    COPY = "MGN"
    OPTION = "REPLACE"
  END_GROUP = PARAMETERS
END_GROUP = CPYLAB

GROUP = MAPLAB
  VERSION_DATE = 1996-03-01
  DATE_TIME = 1996-04-16T15:27:55
  NODE_NAME = "tycho"
  USER_NAME = "tbecker"
  SOFTWARE_DESC = " Puts map projection information in labels"
  GROUP = PARAMETERS
    FROM = "north_final.cub"
    PROJ = "POLA"
    PLANET = "EUROPA"
    LAT = (-1.0000000,90.0000000)
    LON = (0.0000000,360.0000000)
    KM = 4.0000000
    DEG = NULL
    PLAT = NULL
    PLON = NULL
    MAPROT = NULL
    CLON = 90.0000000
    CLAT = 0.0000000
    OFFSET = 0.0000000
    POINT1 = NULL
    POINT2 = NULL
    KILL = "NO"
  END_GROUP = PARAMETERS
END_GROUP = MAPLAB

GROUP = MAPLAB
  VERSION_DATE = 1996-03-01
  DATE_TIME = 1996-04-16T15:31:13
  NODE_NAME = "tycho"
  USER_NAME = "tbecker"
  SOFTWARE_DESC = " Puts map projection information in labels"
  GROUP = PARAMETERS
    FROM = "north_final.cub"
    PROJ = "POLA"
    PLANET = "EUROPA"
    LAT = (-1.0000000,90.0000000)
    LON = (0.0000000,360.0000000)
    KM = 4.0000000
    DEG = NULL
    PLAT = NULL
    PLON = NULL
    MAPROT = NULL
    CLON = 0.0000000
    CLAT = 90.0000000
    OFFSET = 0.0000000
    POINT1 = NULL
    POINT2 = NULL
    KILL = "NO"
  END_GROUP = PARAMETERS
END_GROUP = MAPLAB

GROUP = GEOM
  VERSION_DATE = 1995-06-16
  DATE_TIME = 1996-04-16T15:33:36
  NODE_NAME = "tycho"
  USER_NAME = "tbecker"
  SOFTWARE_DESC = 
    "Perform geometric transformation of image planes in ISIS cube"
  GROUP = PARAMETERS
    FROM = "north_final.cub"
    TFILE = "tfile.dat"
    TO = "north_sinu.cub"
    WORK1 = "geom1.WRK"
    WORK2 = "geom2.WRK"
    DNINTERP = "BILINEAR"
  END_GROUP = PARAMETERS
END_GROUP = GEOM

GROUP = QVIEW
  VERSION_DATE = 1995-11-27
  DATE_TIME = 1996-04-16T15:37:29
  NODE_NAME = "tycho"
  USER_NAME = "tbecker"
  SOFTWARE_DESC = "View a cube"
  GROUP = PARAMETERS
  END_GROUP = PARAMETERS
END_GROUP = QVIEW

GROUP = MOSAIC
  VERSION_DATE = 1996-03-12
  DATE_TIME = 1996-04-16T16:15:07
  NODE_NAME = "tycho"
  USER_NAME = "tbecker"
  SOFTWARE_DESC = "Merge an input file with an output mosaics file"
  USERNOTE = " "
  GROUP = PARAMETERS
    FROM = "north_sinu.cub"
    SFROM = NULL
    TO = "europa_polefix.cub"
    OBAND = NULL
    BANDCHK = "YES"
    HIST = "NO"
    CONV = "NO"
    OPTION = "MAP"
    AVERAGE = "NO"
    LAST = "NO"
    TOP = "YES"
    TVREG = ""
    INIT = "YES"
    MOSALL = "NO"
    USERNOTE = " "
    NLM = 0
    NSM = 0
    NBM = 0
    NIMGS = 50
    LAT = (-90.0000000,90.0000000)
    LON = (0.0000000,0.0000000)
    BACKCHK = NULL
    OUTPNT = ""
  END_GROUP = PARAMETERS
END_GROUP = MOSAIC

GROUP = MOSAIC
  VERSION_DATE = 1996-03-12
  DATE_TIME = 1996-04-16T16:15:26
  NODE_NAME = "tycho"
  USER_NAME = "tbecker"
  SOFTWARE_DESC = "Merge an input file with an output mosaics file"
  USERNOTE = " "
  GROUP = PARAMETERS
    FROM = "south_sinu.cub"
    SFROM = NULL
    TO = "europa_polefix.cub"
    OBAND = NULL
    BANDCHK = "YES"
    HIST = "NO"
    CONV = "NO"
    OPTION = "MAP"
    AVERAGE = "NO"
    LAST = "NO"
    TOP = "YES"
    TVREG = ""
    INIT = "NO"
    MOSALL = "NO"
    USERNOTE = " "
    NLM = 0
    NSM = 0
    NBM = 0
    NIMGS = 50
    LAT = (0.0000000,0.0000000)
    LON = (0.0000000,0.0000000)
    BACKCHK = NULL
    OUTPNT = ""
  END_GROUP = PARAMETERS
END_GROUP = MOSAIC

GROUP = TRIMSINU
  VERSION_DATE = 1996-01-23
  DATE_TIME = 1996-04-16T16:19:48
  NODE_NAME = "tycho"
  USER_NAME = "tbecker"
  SOFTWARE_DESC = "Trim cube in sinusoidal projection"
  GROUP = PARAMETERS
    FROM = "europa_polefix.cub"
    TO = "europa_polefixfin.cub"
  END_GROUP = PARAMETERS
END_GROUP = TRIMSINU

GROUP = QVIEW
  VERSION_DATE = 1995-11-27
  DATE_TIME = 1996-04-16T16:20:23
  NODE_NAME = "tycho"
  USER_NAME = "tbecker"
  SOFTWARE_DESC = "View a cube"
  GROUP = PARAMETERS
  END_GROUP = PARAMETERS
END_GROUP = QVIEW

GROUP = MOSAIC
  VERSION_DATE = 1996-03-12
  DATE_TIME = 1996-04-17T12:14:03
  NODE_NAME = "tycho"
  USER_NAME = "tbecker"
  SOFTWARE_DESC = "Merge an input file with an output mosaics file"
  USERNOTE = " "
  GROUP = PARAMETERS
    FROM = "europa_polefixfin.cub"
    SFROM = NULL
    TO = "europa_polefix2.cub"
    OBAND = NULL
    BANDCHK = "YES"
    HIST = "NO"
    CONV = "NO"
    OPTION = "MAP"
    AVERAGE = "NO"
    LAST = "NO"
    TOP = "YES"
    TVREG = ""
    INIT = "YES"
    MOSALL = "NO"
    USERNOTE = " "
    NLM = 0
    NSM = 0
    NBM = 0
    NIMGS = 50
    LAT = (0.0000000,0.0000000)
    LON = (0.0000000,0.0000000)
    BACKCHK = NULL
    OUTPNT = ""
  END_GROUP = PARAMETERS
END_GROUP = MOSAIC

GROUP = MOSAIC
  VERSION_DATE = 1996-03-12
  DATE_TIME = 1996-04-17T12:14:44
  NODE_NAME = "tycho"
  USER_NAME = "tbecker"
  SOFTWARE_DESC = "Merge an input file with an output mosaics file"
  USERNOTE = " "
  GROUP = PARAMETERS
    FROM = "north_c2sinu"
    SFROM = NULL
    TO = "europa_polefix2.cub"
    OBAND = NULL
    BANDCHK = "YES"
    HIST = "NO"
    CONV = "NO"
    OPTION = "MAP"
    AVERAGE = "NO"
    LAST = "NO"
    TOP = "YES"
    TVREG = ""
    INIT = "NO"
    MOSALL = "NO"
    USERNOTE = " "
    NLM = 0
    NSM = 0
    NBM = 0
    NIMGS = 50
    LAT = (0.0000000,0.0000000)
    LON = (0.0000000,0.0000000)
    BACKCHK = NULL
    OUTPNT = ""
  END_GROUP = PARAMETERS
END_GROUP = MOSAIC

GROUP = MOSAIC
  VERSION_DATE = 1996-03-12
  DATE_TIME = 1996-04-17T12:21:36
  NODE_NAME = "tycho"
  USER_NAME = "tbecker"
  SOFTWARE_DESC = "Merge an input file with an output mosaics file"
  USERNOTE = " "
  GROUP = PARAMETERS
    FROM = "north_c2sinu"
    SFROM = NULL
    TO = "europa_polefix2.cub"
    OBAND = NULL
    BANDCHK = "YES"
    HIST = "NO"
    CONV = "NO"
    OPTION = "MAP"
    AVERAGE = "NO"
    LAST = "NO"
    TOP = "YES"
    TVREG = ""
    INIT = "NO"
    MOSALL = "YES"
    USERNOTE = " "
    NLM = 0
    NSM = 0
    NBM = 0
    NIMGS = 50
    LAT = (0.0000000,0.0000000)
    LON = (0.0000000,0.0000000)
    BACKCHK = NULL
    OUTPNT = ""
  END_GROUP = PARAMETERS
END_GROUP = MOSAIC

GROUP = MOSAIC
  VERSION_DATE = 1996-03-12
  DATE_TIME = 1996-04-17T12:23:53
  NODE_NAME = "tycho"
  USER_NAME = "tbecker"
  SOFTWARE_DESC = "Merge an input file with an output mosaics file"
  USERNOTE = " "
  GROUP = PARAMETERS
    FROM = "europa_polefixfin.cub"
    SFROM = NULL
    TO = "europa_polefix2.cub"
    OBAND = NULL
    BANDCHK = "YES"
    HIST = "NO"
    CONV = "NO"
    OPTION = "MAP"
    AVERAGE = "NO"
    LAST = "NO"
    TOP = "YES"
    TVREG = ""
    INIT = "NO"
    MOSALL = "NO"
    USERNOTE = " "
    NLM = 0
    NSM = 0
    NBM = 0
    NIMGS = 50
    LAT = (0.0000000,0.0000000)
    LON = (0.0000000,0.0000000)
    BACKCHK = NULL
    OUTPNT = ""
  END_GROUP = PARAMETERS
END_GROUP = MOSAIC

GROUP = MOSAIC
  VERSION_DATE = 1996-03-12
  DATE_TIME = 1996-04-17T12:38:28
  NODE_NAME = "tycho"
  USER_NAME = "tbecker"
  SOFTWARE_DESC = "Merge an input file with an output mosaics file"
  USERNOTE = " "
  GROUP = PARAMETERS
    FROM = "north_c2sinu.cub"
    SFROM = NULL
    TO = "europa_polefix2.cub"
    OBAND = NULL
    BANDCHK = "YES"
    HIST = "NO"
    CONV = "NO"
    OPTION = "MAP"
    AVERAGE = "NO"
    LAST = "NO"
    TOP = "YES"
    TVREG = ""
    INIT = "NO"
    MOSALL = "YES"
    USERNOTE = " "
    NLM = 0
    NSM = 0
    NBM = 0
    NIMGS = 50
    LAT = (1.0000000,90.0000000)
    LON = (0.0000000,0.0000000)
    BACKCHK = NULL
    OUTPNT = ""
  END_GROUP = PARAMETERS
END_GROUP = MOSAIC

GROUP = MOSAIC
  VERSION_DATE = 1996-03-12
  DATE_TIME = 1996-04-17T12:40:36
  NODE_NAME = "tycho"
  USER_NAME = "tbecker"
  SOFTWARE_DESC = "Merge an input file with an output mosaics file"
  USERNOTE = " "
  GROUP = PARAMETERS
    FROM = "europa_polefixfin.cub"
    SFROM = NULL
    TO = "europa_polefix2.cub"
    OBAND = NULL
    BANDCHK = "YES"
    HIST = "NO"
    CONV = "NO"
    OPTION = "MAP"
    AVERAGE = "NO"
    LAST = "NO"
    TOP = "YES"
    TVREG = ""
    INIT = "NO"
    MOSALL = "NO"
    USERNOTE = " "
    NLM = 0
    NSM = 0
    NBM = 0
    NIMGS = 50
    LAT = (20.0000000,-90.0000000)
    LON = (0.0000000,0.0000000)
    BACKCHK = NULL
    OUTPNT = ""
  END_GROUP = PARAMETERS
END_GROUP = MOSAIC

GROUP = MOSAIC
  VERSION_DATE = 1996-03-12
  DATE_TIME = 1996-04-17T12:41:45
  NODE_NAME = "tycho"
  USER_NAME = "tbecker"
  SOFTWARE_DESC = "Merge an input file with an output mosaics file"
  USERNOTE = " "
  GROUP = PARAMETERS
    FROM = "north_c2sinu.cub"
    SFROM = NULL
    TO = "europa_polefix2.cub"
    OBAND = NULL
    BANDCHK = "YES"
    HIST = "NO"
    CONV = "NO"
    OPTION = "MAP"
    AVERAGE = "NO"
    LAST = "NO"
    TOP = "YES"
    TVREG = ""
    INIT = "NO"
    MOSALL = "YES"
    USERNOTE = " "
    NLM = 0
    NSM = 0
    NBM = 0
    NIMGS = 50
    LAT = (0.0000000,0.0000000)
    LON = (0.0000000,0.0000000)
    BACKCHK = NULL
    OUTPNT = ""
  END_GROUP = PARAMETERS
END_GROUP = MOSAIC

GROUP = MOSAIC
  VERSION_DATE = 1996-03-12
  DATE_TIME = 1996-04-17T12:42:49
  NODE_NAME = "tycho"
  USER_NAME = "tbecker"
  SOFTWARE_DESC = "Merge an input file with an output mosaics file"
  USERNOTE = " "
  GROUP = PARAMETERS
    FROM = "europa_polefixfin.cub"
    SFROM = NULL
    TO = "europa_polefix2.cub"
    OBAND = NULL
    BANDCHK = "YES"
    HIST = "NO"
    CONV = "NO"
    OPTION = "MAP"
    AVERAGE = "NO"
    LAST = "NO"
    TOP = "NO"
    TVREG = ""
    INIT = "NO"
    MOSALL = "NO"
    USERNOTE = " "
    NLM = 0
    NSM = 0
    NBM = 0
    NIMGS = 50
    LAT = (0.0000000,0.0000000)
    LON = (0.0000000,0.0000000)
    BACKCHK = NULL
    OUTPNT = ""
  END_GROUP = PARAMETERS
END_GROUP = MOSAIC

GROUP = MOSAIC
  VERSION_DATE = 1996-03-12
  DATE_TIME = 1996-04-17T13:38:34
  NODE_NAME = "tycho"
  USER_NAME = "tbecker"
  SOFTWARE_DESC = "Merge an input file with an output mosaics file"
  USERNOTE = " "
  GROUP = PARAMETERS
    FROM = "north_c2sinu.cub"
    SFROM = NULL
    TO = "europa_polefix2"
    OBAND = NULL
    BANDCHK = "YES"
    HIST = "NO"
    CONV = "NO"
    OPTION = "MAP"
    AVERAGE = "NO"
    LAST = "NO"
    TOP = "YES"
    TVREG = ""
    INIT = "NO"
    MOSALL = "YES"
    USERNOTE = " "
    NLM = 0
    NSM = 0
    NBM = 0
    NIMGS = 50
    LAT = (0.0000000,0.0000000)
    LON = (0.0000000,0.0000000)
    BACKCHK = NULL
    OUTPNT = ""
  END_GROUP = PARAMETERS
END_GROUP = MOSAIC

GROUP = MOSAIC
  VERSION_DATE = 1996-03-12
  DATE_TIME = 1996-04-17T14:15:10
  NODE_NAME = "tycho"
  USER_NAME = "tbecker"
  SOFTWARE_DESC = "Merge an input file with an output mosaics file"
  USERNOTE = " "
  GROUP = PARAMETERS
    FROM = "europa_polefixfin.cub"
    SFROM = NULL
    TO = "europa_polefix2.cub"
    OBAND = NULL
    BANDCHK = "YES"
    HIST = "NO"
    CONV = "NO"
    OPTION = "MAP"
    AVERAGE = "NO"
    LAST = "NO"
    TOP = "NO"
    TVREG = ""
    INIT = "NO"
    MOSALL = "NO"
    USERNOTE = " "
    NLM = 0
    NSM = 0
    NBM = 0
    NIMGS = 50
    LAT = (0.0000000,0.0000000)
    LON = (0.0000000,0.0000000)
    BACKCHK = NULL
    OUTPNT = ""
  END_GROUP = PARAMETERS
END_GROUP = MOSAIC

GROUP = TRIM
  VERSION_DATE = 1995-03-22
  DATE_TIME = 1996-04-17T14:18:39
  NODE_NAME = "tycho"
  USER_NAME = "tbecker"
  SOFTWARE_DESC = "NULL the edges of a cube"
  USERNOTE = " "
  GROUP = PARAMETERS
    FROM = "europa_polefix2.cub"
    SFROM = NULL
    TO = "europa_trim.cub"
    TOP = 82
    BOTTOM = 0
    LEFT = 0
    RIGHT = 0
    BPLANES = "NO"
  END_GROUP = PARAMETERS
END_GROUP = TRIM

GROUP = TRIMSINU
  VERSION_DATE = 1996-01-23
  DATE_TIME = 1996-04-17T14:21:31
  NODE_NAME = "tycho"
  USER_NAME = "tbecker"
  SOFTWARE_DESC = "Trim cube in sinusoidal projection"
  GROUP = PARAMETERS
    FROM = "europa_trim.cub"
    TO = "europa_sinufinal.cub"
  END_GROUP = PARAMETERS
END_GROUP = TRIMSINU

GROUP = QVIEW
  VERSION_DATE = 1995-11-27
  DATE_TIME = 1996-04-17T14:22:40
  NODE_NAME = "tycho"
  USER_NAME = "tbecker"
  SOFTWARE_DESC = "View a cube"
  GROUP = PARAMETERS
  END_GROUP = PARAMETERS
END_GROUP = QVIEW

GROUP = QVIEW
  VERSION_DATE = 1995-11-27
  DATE_TIME = 1996-04-17T17:02:31
  NODE_NAME = "tycho"
  USER_NAME = "tbecker"
  SOFTWARE_DESC = "View a cube"
  GROUP = PARAMETERS
  END_GROUP = PARAMETERS
END_GROUP = QVIEW
END
                                                                                                                                    
                                                                                                                                    
 
********** END OF HISTORY OBJECT **********
 
LET PIC = "/project/test_work/testdata/gll/"//"g7jnfea15003a.15"
label-remove /project/test_work/testdata/gll/g7jnfea15003a.15 edr1 'bin
Beginning VICAR task label
ISISLAB edr1
Beginning VICAR task ISISLAB
*** ISISLAB version 3-Nov-05 ***
 no History item present
 
********** LABEL OBJECT **********
 
CSD3ZF0000100000001NJPL3IF0PDS200000001 = SFDU_LABEL

/* File Structure (512-byte records required by ISIS software) */

RECORD_TYPE		= FIXED_LENGTH
RECORD_BYTES		= 512
FILE_RECORDS		= 174
LABEL_RECORDS		= 6
CHECKSUM		= 356815
/* This is an unsigned 32-bit checksum of all bytes after label records */
FILE_STATE		= CLEAN

/* Object pointers and descriptions */

^HEADER_TABLE		= 7
OBJECT			= HEADER_TABLE
 INTERCHANGE_FORMAT	= BINARY
 COLUMNS		= UNK
 ROWS			= 1
 ROW_BYTES		= 2048
 ^STRUCTURE		= "EDRHDR2.FMT"
END_OBJECT		= HEADER_TABLE

^DATA_TABLE		= 11
OBJECT			= DATA_TABLE
 INTERCHANGE_FORMAT	= BINARY
 COLUMNS		= UNK
 ROWS			= 82
 ROW_BYTES		= 1024
 ^STRUCTURE		= "EDRDATA2.FMT"

/* Data description: general */

 DATA_SET_ID			= "GO-J-NIMS-2-EDR-V2.0"
 SPACECRAFT_NAME		= GALILEO_ORBITER
 INSTRUMENT_NAME		= "NEAR INFRARED MAPPING SPECTROMETER"
 INSTRUMENT_ID			= NIMS
 ^INSTRUMENT_DESCRIPTION	= "NIMSINST.TXT"

 TARGET_NAME			= JUPITER
 MISSION_PHASE_NAME		= GANYMEDE_7_ENCOUNTER
 NATIVE_START_TIME		= "03901070.00"
 NATIVE_STOP_TIME		= "03901071.50"
/* Event times are SPICE E-kernel functions of native times */
/* Earth received times are inclusive of all merged downlink data */
 EARTH_RECEIVED_START_TIME	= 1997-04-24T23:04:55.165
 EARTH_RECEIVED_STOP_TIME	= 1997-04-24T23:17:11.567

 SPACECRAFT_COMPRESSION_TYPE	 = RICE
/* Most non-realtime NIMS data taken during Galileo phase-2 operations */
/* were compressed on the spacecraft using a lossless 8-option adaptive */
/* compression method devised by Robert Rice.  This EDR contains the */
/* original data after decompression on the ground. */
 SPACECRAFT_COMPRESSION_RATIO	=  1.62

 PRODUCT_CREATION_TIME		= 1998-04-30T08:40:09Z
 PRODUCT_ID			= "G7JNFEA15003A.15"
 NOTE = "EDR product provided by Multimission Image Processing
                 Laboratory of JPL, CALTECH"

END_OBJECT		= DATA_TABLE

END
 
********** END OF LABEL OBJECT **********
 
end-proc
exit
slogoff
if ($RUNTYPE = "INTERACTIVE")
  if ($syschar(1) = "VAX_VMS")
  end-if
else
  if ($syschar(1) = "VAX_VMS")
  end-if
end-if
ulogoff
END-PROC
END-PROC
$!-----------------------------------------------------------------------------
$ create tstisislab.log_linux
tstisislab.log_linux IS COMPLETELY IDENTICAL TO tstisislab.log_solos
$ Return
$!#############################################################################
