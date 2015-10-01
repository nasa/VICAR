$!****************************************************************************
$!
$! Build proc for MIPL module mp_labels
$! VPACK Version 1.9, Monday, December 07, 2009, 15:58:17
$!
$! Execute by entering:		$ @mp_labels
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
$!   TEST        Only the test files are created.
$!   IMAKE       Only the IMAKE file (used with the VIMAKE program) is created.
$!   OTHER       Only the "other" files are created.
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
$ write sys$output "*** module mp_labels ***"
$!
$ Create_Source = ""
$ Create_Repack =""
$ Create_Test = ""
$ Create_Imake = ""
$ Create_Other = ""
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
$ if primary .eqs. "TEST" then Create_Test = "Y"
$ if primary .eqs. "IMAKE" then Create_Imake = "Y"
$ if primary .eqs. "OTHER" then Create_Other = "Y"
$ if (f$locate("CLEAN", primary) .eqs. 0) then Do_Make = "Y"
$!
$ if (Create_Source .or. Create_Repack .or. Create_Test .or. Create_Imake .or -
        Create_Other .or. Do_Make) -
        then goto Parameter_Okay
$ write sys$output "Invalid argument given to mp_labels.com file -- ", primary
$ write sys$output "For a list of valid arguments, please see the header of"
$ write sys$output "of this .com file."
$ exit
$!
$Parameter_Okay:
$ if Create_Repack then gosub Repack_File
$ if Create_Source then gosub Source_File
$ if Create_Test then gosub Test_File
$ if Create_Imake then gosub Imake_File
$ if Create_Other then gosub Other_File
$ if Do_Make then gosub Run_Make_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_Source = "Y"
$   Create_Test = "Y"
$   Create_Imake = "Y"
$   Create_Other = "Y"
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
$ Return
$!
$ Set_All_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_Sys_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$Run_Make_File:
$   if F$SEARCH("mp_labels.imake") .nes. ""
$   then
$      vimake mp_labels
$      purge mp_labels.bld
$   else
$      if F$SEARCH("mp_labels.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake mp_labels
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @mp_labels.bld "STD"
$   else
$      @mp_labels.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create mp_labels.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack mp_labels.com -mixed -
	-s mplabelread.c mplabelwrite.c mpbuf2mpo.c -
	-i mp_labels.imake -
	-t tmp_routines.c tmp_routines.imake tmp_routines.pdf tmp_routines_f.f -
	   tmp_routines_verbose.c tmp_routines_verbose.imake -
	   tmp_routines_verbose.pdf tmp_routines_f.f tmp_routines_f.imake -
	   tmp_routines_f.pdf tmp_label.imake tmp_label.pdf tmp_label.c -
	   tmp_forward.c tmp_forward.imake tmp_forward.pdf tmp_mpo2buf.c -
	   tmp_mpo2buf.imake tmp_mpo2buf.pdf tmp_buf2mpo.c tmp_buf2mpo.imake -
	   tmp_buf2mpo.pdf tmp_create.imake tmp_create.pdf tmp_create.c -
	   tstmp_routines.pdf -
	-o mpinit.hlp mpfree.hlp mpll2xy.hlp mpxy2ll.hlp mpsetvalues.hlp -
	   mp_set_value.hlp mp_set_value_str.hlp mpgetvalues.hlp -
	   mp_get_value.hlp mp_get_value_str.hlp mplabelread.hlp -
	   mplabelwrite.hlp mpgetkeywords.hlp mpsetdebugflag.hlp -
	   release_notes.txt
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create mplabelread.c
$ DECK/DOLLARS="$ VOKAGLEVE"
				/*					*/
				/* INCLUDE FILES NEEDED FOR ROUTINES 	*/
				/*					*/
#include "xvmaininc.h"		/* Standard VICAR Include File		*/
#include "errdefs.h"
#include <math.h>		/* Standard math library include	*/
#include <stdio.h>		/* Standard C I/O Include File		*/
#include <stdlib.h>		/* C Memory Management Include File	*/
#include <time.h>
#include <zvproto.h>
#include <string.h>
#include "mp_routines.h"

/********************************************************************/
	/*
	 * Function mpLabelRead:
	 *
	 * routine to read the MP Property labels from a VICAR
	 * image and store the values into the MP object;  this
	 * routine simply reads in all the items in this group
	 * without performing any validity check
	 *
	 * 27aug93  -lwk-  intial version
	 * 18sep93  -lwk-  added support for old labels (SEARCV3)
         * 09mar94  -lwk-  changed declaration of mp from (MP_STRUCTURE *)
         *		to MP, since it's public
	 * 18may94  -jfm-  changed "REAL" to "DOUB" in call to zlget
	 * june94   -jfm-  corrected tests for success or failure against
	 *		VICAR RTL functions and other MP functions (FR 85078)
	 * 14dec94  -jfm-  ERR_ACT flag of blanks added to VICAR RTL routines
         *                 to override ZVEACTION by calling programs.
	 * 25feb98  -lwk-  initialize oldbuf[] to prevent problems in mpbuf2mpo
	 */
/**************************************************************************/
int mpLabelRead( 
  MP mp,				/* IN:  user's MP structure */
  int unit)				/* IN:  VICAR file unit number */
{
  int i, len, nelements, status;
  float oldbuf[40];
  double dval;
  char sval[mpMAX_KEYWD_LENGTH], keywd[mpMAX_KEYWD_LENGTH], format[12];

	/* if there are MP Property labels, set the key pointer to them;
	 * else check for old-style labels and convert: */

  status = zlinfo( unit, "PROPERTY", "PROPERTY", format, &i, &nelements,
   "PROPERTY", mpPROPERTY_NAME, "ERR_ACT", " ", NULL);
  if (status < VICARrtlSUCCESS) {
    for (i=0; i<40; i++) oldbuf[i] = (float)0.;	/* clear out any garbage */
    if (sizeof(int) != sizeof(float)) {
	/* The two oldbuf's in searcv3_c are supposed to be parallel and      */
	/* overlapping.  Not a good design, but it is what it is.  This works */
	/* as long as sizeof(int) == sizeof(float) (which it should always,   */
	/* for machines we care about).  If this condition does NOT hold,     */
	/* this call is broken.  Continue just in case, but warn.  rgd 3/2010 */
	zvmessage("WARNING!! sizeof(int)!=sizeof(float) on this machine.","");
	zvmessage("The call to searcv3_c() may be incorrect!","");
    }
    status = searcv3_c( unit, oldbuf, (int *)oldbuf);
    if (status != 1) return mp_NO_MAP_LABELS;
    status = mpBuf2Mpo( oldbuf, mp);
    if (status < mpSUCCESS) return status;
    return mpSUCCESS;
  }

	/* there are MP Property labels -- cycle through all the items in
	 * this group: */
  while (TRUE) {
    status = zlninfo( unit, keywd, format, &len, &nelements, "ERR_ACT", " ", NULL);
    if (len>mpMAX_KEYWD_LENGTH || EQUAL(keywd,mpMAP_PROJECTION_DESC)) continue;
    if (status==END_OF_LABEL || EQUAL(keywd,"TASK") || EQUAL(keywd,"PROPERTY"))
      break;
    if (EQUAL( format, "STRING")) {
      status = zlget( unit, "PROPERTY", keywd, sval, "FORMAT", "STRING",
       "PROPERTY", mpPROPERTY_NAME, "ERR_ACT", " ", NULL);
      if (status >= VICARrtlSUCCESS) {
	status = mpSetValues( mp, keywd, sval, NULL);
	if (status < mpSUCCESS) return status;
      }
    }
    if (EQUAL( format, "REAL")) {
      status = zlget( unit, "PROPERTY", keywd, (char*) &dval, "FORMAT", "DOUB",
       "PROPERTY", mpPROPERTY_NAME, "ERR_ACT", " ", NULL);
      if (status >= VICARrtlSUCCESS) {
	status = mpSetValues( mp, keywd, dval, NULL);
	if (status < mpSUCCESS) return status;
      }
    }
  }

  /* here follow some special checks for old-style labels prior to introduction
   * of the COORDINATE_SYSTEM_NAME keyword ... */

  status = zlget( unit, "PROPERTY", mpCOORDINATE_SYSTEM_NAME, sval, "FORMAT",
   "STRING", "PROPERTY", mpPROPERTY_NAME, "ERR_ACT", " ", NULL);
  if (status >= VICARrtlSUCCESS) return mpSUCCESS;

  /* COORDINATE_SYSTEM_NAME not found -- start checking the history */

  /* only MAP3, MAPLABPROG, and PERSLAB generate MP labels from scratch ... */

  status = zlget( unit, "HISTORY", "A_AXIS_RADIUS", (char*) &dval, "HIST", "MAP3",
   "ERR_ACT", " ", NULL);
  if (status >= VICARrtlSUCCESS) {
    status = mpSetValues( mp, "COORDINATE_SYSTEM_NAME", "PLANETOCENTRIC", NULL);
    if (status < mpSUCCESS) return status;
  }
  status = zlget( unit, "HISTORY", "A_AXIS_RADIUS", (char*) &dval, "HIST", "MAPLABPROG",
   "ERR_ACT", " ", NULL);
  if (status >= VICARrtlSUCCESS) {
    status = mpSetValues( mp, "COORDINATE_SYSTEM_NAME", "PLANETOCENTRIC", NULL);
    if (status < mpSUCCESS) return status;
  }
  status = zlget( unit, "HISTORY", "A_AXIS_RADIUS", (char*) &dval, "HIST", "PERSLAB",
   "ERR_ACT", " ", NULL);
  if (status >= VICARrtlSUCCESS) {
    status = mpSetValues( mp, "COORDINATE_SYSTEM_NAME", "PLANETOCENTRIC", NULL);
    if (status < mpSUCCESS) return status;
  }

  /* NIMS cubes have their own special keyword: */
  status = zlget( unit, "HISTORY", "LATITUDE_TYPE", sval, "HIST", "NIMSCMM",
   "ERR_ACT", " ", NULL);
  if (status >= VICARrtlSUCCESS) {
    status = mpSetValues( mp, "COORDINATE_SYSTEM_NAME", sval, NULL);
    if (status < mpSUCCESS) return status;
  }
  status = zlget( unit, "HISTORY", "LATITUDE_TYPE", sval, "HIST", "NIMSCMM2",
   "ERR_ACT", " ", NULL);
  if (status >= VICARrtlSUCCESS) {
    status = mpSetValues( mp, "COORDINATE_SYSTEM_NAME", sval, NULL);
    if (status < mpSUCCESS) return status;
  }

  /* if no known program wrote the label, then assume planetodetic */
  status = mpSetValues( mp, "COORDINATE_SYSTEM_NAME", "PLANETOGRAPHIC", NULL);
  if (status < mpSUCCESS) return status;

  return mpSUCCESS;
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create mplabelwrite.c
$ DECK/DOLLARS="$ VOKAGLEVE"
				/*					*/
				/* INCLUDE FILES NEEDED FOR ROUTINES 	*/
				/*					*/
#include "xvmaininc.h"		/* Standard VICAR Include File		*/
#include "errdefs.h"
#include <math.h>		/* Standard math library include	*/
#include <stdio.h>		/* Standard C I/O Include File		*/
#include <stdlib.h>		/* C Memory Management Include File	*/
#include <time.h>
#include "mp_routines.h"
#include <string.h>
#include <zvproto.h>

/********************************************************************/
	/*
	 * Function mpLabelWrite:
	 *
	 * routine to write two complete sets of map labels to a VICAR
	 * image:  one set of Property labels, and one of History labels.
	 *
	 * These two sets share the same core of items, but are not 
	 * identical;  e.g., only Property labels include the
	 * MAP_PROJECTION_DESC item, while only History labels include 
	 * the SUPPLEMENTARY_KEYWORDS items.
	 *
	 * 09aug93  -lwk-  intial version
	 * 30aug93  -lwk-  delete existing property labels on output file
         * 09mar94  -lwk-  changed declaration of mp from (MP_STRUCTURE *)
         *                 to MP, since it's public
	 * 18may94  -jfm-  changed "REAL" to "DOUB" in calls to zladd
	 * june94   -jfm-  corrected tests for success and failure of
         *		   VICAR RTL functions and MP routines (FR 85078)
	 * july94   -jfm-  success flag returned set to mpSUCCESS if
	 * 		   routine continues to completion
	 * 14dec94  -jfm-  ERR_ACT flag set to blanks added to VICAR RTL
         *                 routine calls to override calling program's
         *		   ZVEACTION.
	 * 22aug98  -lwk-  changes for revised mpGetKeywords calling sequence
	 */

/**************************************************************************/
int mpLabelWrite( 
  MP mp,				/* IN:  user's MP structure       */
  int unit,				/* IN:  VICAR file unit number    */
  char labtype[9])			/* IN:  "HISTORY" or "PROPERTY"   */
{
  int i, len, nelements, num, status;
  int desc_lines;
  int kwd_type[mpNUMBER_OF_KEYWORDS], kwd_class[mpNUMBER_OF_KEYWORDS];

  char sval[mpMAX_KEYWD_LENGTH+1], keywd[mpMAX_KEYWD_LENGTH+1], format[12];
  char keywds[mpNUMBER_OF_KEYWORDS][mpMAX_KEYWD_LENGTH+1];
  char desc[mpMAX_DESCRIPTION_LINES][100];	/* mpMAP_PROJECTION_DESC */
  double dval;  

	/* if we're adding Property labels, then first delete all existing 
	 * ones in the output file: */
  if (EQUAL(labtype,"PROPERTY")) {

	/* set the key pointer to the start of the MP group: */
    status = zlinfo( unit, "PROPERTY", "PROPERTY", format, &i, &nelements,
     "PROPERTY", mpPROPERTY_NAME, "ERR_ACT", " ", NULL);

    if (status>=VICARrtlSUCCESS) {

	/* ... and cycle through all the labels in this group: */
      while (TRUE) {

        status = zlninfo( unit, keywd, format, &len, &nelements, "ERR_ACT", " ", NULL);

	if (status==END_OF_LABEL || EQUAL(keywd,"TASK") ||
	 EQUAL(keywd,"PROPERTY")) break;

	status = zldel( unit, "PROPERTY", keywd, "PROPERTY", mpPROPERTY_NAME,
		"ERR_ACT", " ", NULL);

	if (status<VICARrtlSUCCESS) return status;
      }
    }
  }

  /* check to make sure that COORDINATE_SYSTEM_NAME is set to something: */
  status = mpGetValues( mp, "COORDINATE_SYSTEM_NAME", sval, NULL);
  if (status < mpSUCCESS)
    status = mpSetValues( mp, "COORDINATE_SYSTEM_NAME", "PLANETOGRAPHIC", NULL);


  status = mpGetKeywords( mp, keywds, &num, kwd_type, kwd_class);

  for (i=0; i<num; i++) {

	/* Supplementary items go only to History labels: */
    if (EQUAL(labtype,"PROPERTY") && kwd_class[i] == mpSUPPL) continue;

    if (kwd_type[i] == mpCHAR) {

      status = mpGetValues( mp, keywds[i], sval, NULL);

      if (status<mpSUCCESS) return status;

      if (EQUAL(labtype,"PROPERTY"))
	status = zladd( unit, labtype, keywds[i], sval, "FORMAT", "STRING",
	 "NELEMENT", 1, "PROPERTY", mpPROPERTY_NAME, "ERR_ACT", " ", NULL);
      else 
	status = zladd( unit, labtype, keywds[i], sval, "FORMAT",
	 "STRING", "NELEMENT", 1, "ERR_ACT", " ", NULL);

      if (status<VICARrtlSUCCESS) return status;
    }
    else if (kwd_type[i] == mpDBLE) {

      status = mpGetValues( mp, keywds[i], &dval, NULL);

      if (status<mpSUCCESS) return status;

      if (EQUAL(labtype,"PROPERTY"))
	status = zladd( unit, labtype, keywds[i], &dval, "FORMAT", "DOUB",
	 "NELEMENT", 1, "PROPERTY", mpPROPERTY_NAME, "ERR_ACT", " ", NULL);
      else 
	status = zladd( unit, labtype, keywds[i], &dval, "FORMAT", "DOUB",
       	 "NELEMENT", 1, "ERR_ACT", " ", NULL);

      if (status<VICARrtlSUCCESS) return status;
    }
  }
	/* Descriptive text (Property only) is generated separately: */
  if (EQUAL(labtype,"PROPERTY")) {
    status = mpGetDesc( mp, desc, &desc_lines );

    if (status<mpSUCCESS) return status;

    status = zladd( unit, labtype, mpMAP_PROJECTION_DESC, desc, 
	"FORMAT", "STRING", "NELEMENT", desc_lines, "ULEN", 100,
	"PROPERTY", mpPROPERTY_NAME, "ERR_ACT", " ", NULL);

    if (status<VICARrtlSUCCESS) return status;
  }

  return mpSUCCESS;
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create mpbuf2mpo.c
$ DECK/DOLLARS="$ VOKAGLEVE"
				/*					*/
				/* INCLUDE FILES NEEDED FOR ROUTINES 	*/
				/*					*/
#include <math.h>		/* Standard math library include	*/
#include <stdio.h>		/* Standard C I/O Include File		*/
#include <stdlib.h>		/* C Memory Management Include File	*/
#include "mp_routines.h"

/********************************************************************/
/*
 * Function mpBuf2Mpo:
 *
 * routine to convert an "old" (pre-1993) VICAR map buffer
 * to the new MP map object;  this is the "C" interface
 *
 *  14sep93  -lwk-  intial version
 *  21oct93  -lwk-  replaced union in argument list with casts to
 *		new arrays
 *  19jan94  -lwk-  added +1 to LINE/SAMPLE_PROJECTION_OFFSET
 *		conversion
 *  16feb94  -lwk-  added items B_AXIS_RADIUS, MAP_RESOLUTION
 *  24feb94  -lwk-  added SPHERICAL_/CARTESIAN_AZIMUTH for all cases
 *  09mar94  -lwk-  changed declaration of mp from (MP_STRUCTURE *)
 *		to MP, since it's public
 *  13mar95  -jfm-  changed mapping to Cylindrical Equal-Area
 *		projection such that XC or SAMPLE_PROJECTION_OFFSET
 *		is set to 0.0; this required modification of cases
 *		in switch statement for most all projections.
 *		Code modified to use one main switch statement and
 *		return status flag set to variable status (FR 85803)
 *		Added mpSetValues for projections that have 
 *		map resolution in the MAPXXX label.
 *  12jul95  -lwk-  change 'break' to 'return' for invalid projection
 *  28nov95  -lwk-  added POSITIVE_LONGITUDE_DIRECTION=WEST
 *  15oct96  -lwk-  added COORDINATE_SYSTEM_NAME=PLANETOCENTRIC
 *  22jan97  -lwk-  fixed B_AXIS_RADIUS setting in triaxial case
 *   1dec97  -lwk-  Changed SPACECRAFT_DISTANCE to TARGET_CENTER_DISTANCE
 *  17dec99  -lwk-  removed MAP_RESOLUTION item, as this is now done
 *		automatically in mpsetvalues when MAP_SCALE is set (it was
 *		causing an error for some projections, as it was called
 *		before RADIUS was set)
 */

int mpBuf2Mpo( void *buf, MP mp)
{
  int status;
  int *int_buf = (int *)buf;
  float *float_buf = (float *)buf;
  double x;

/*  The structure of the old VICAR standard MAP data buffer is:
 *
 *	These two FORTRAN arrays are overlaid:
 *	REAL*4 RDATA(40)
 *	INTEGER IDATA(40)
 *
 *	IDATA(39)= PROJECTION TYPE
 *			1=POLAR ORTHOGRAPHIC
 *			2=OBLIQUE    "
 *			3=POLAR STEREOGRAPHIC
 *			4=OBLIQUE    "
 *			5=LAMBERT
 *			6=MERCATOR
 *			7=RAW UNCORRECTED IMAGE (IMAGE SPACE)
 *			8=GEOMETRICALLY CORRECTED IMAGE (OBJECT SPACE)
 *			9=NORMAL CYLINDRICAL
 *		       10=SIMPLE     "
 *                     11=OBLIQUE SIMPLE CYLINDRICAL
 *                     12=SINUSOIDAL
 *                     13=OBLIQUE SINUSOIDAL
 *                     14=MOLLWEIDE
 *                     15=TRANSVERSE MERCATOR
 *                     16=PERSPECTIVE
 *
 *    For projection codes 1-6 and 9-15:
 *
 *	RDATA(1) = SPECIAL SAMPLE POINT             
 *	RDATA(2) = SPECIAL LINE POINT               
 *	RDATA(3) = SPECIAL LATITUDE POINT           
 *	RDATA(4) = LATITUDE OF SPEC PARALLEL (DEG)  
 *	RDATA(5) = LATITUDE OF SPEC PARALLEL  (DEG) 
 *	RDATA(6) = SPECIAL LONGITUDE (WEST) (DEG)   
 *	RDATA(7) = SCALE (KM/PIXEL)                 
 *	RDATA(8) = VISIBLE POLE  1=N -1=S           
 *	RDATA(9) = NORTH ANGLE                      
 *	RDATA(10)= MAP RESOLUTION (PIXELS/DEG) for proj. codes 6,9,10,15
 *	RDATA(25)= POLAR RADIUS (KM)                
 *	RDATA(26)= EQUATORIAL RADIUS (KM)           
 *
 *  For projection code 16:
 *
 *       RDATA(1-18)  = OM matrix              
 *       RDATA(19-24) = RS vector
 *	 RDATA(25) =  polar radius (km)            
 *	 RDATA(26) =  equatorial radius (km) 
 *       RDATA(27) = focal length 
 *       RDATA(28) = optical axis line        
 *       RDATA(29) = optical axis sample      
 *       RDATA(30) = scale in pixels/mm.      
 *       RDATA(31) = s/c latitude             
 *       RDATA(32) = s/c longitude            
 *       RDATA(33) = line                     
 *       RDATA(34) = sample                   
 *       RDATA(35) = North angle              
 *       RDATA(38) = range to target body     
 *       RDATA(39) = 16
 *
 *  For the case when the projection type is 7 or 8 (Image or Object
 *  space), the buffer is not defined.
 *
 *  Each of the above items has a corresponding element in the MP Object
 *  except:
 *
 *    Visible Pole (word 8 for non-Perspective) - this is determined by
 *	the "Special Latitude" (word 3);
 *
 *    OM-Matrix and RS-Vector (words 1-24 for Perspective) - these are
 *	determined by the other Perspective items and can be computed
 *	from them using VICAR routine MOMATI.
 *
 *  Since these elements are redundant, they are simply ignored in this
 *  routine.
 *
 *  Note that in the MAP buffer, all longitudes are West and all latitudes
 *  are planetocentric!  (In the MP routines, the reverse is the default in
 *  both cases.)
 */

  switch (int_buf[38]) {

    case 7:		/* Image and Object Space images not supported */
    case 8:
    default:

      status = mpINVALID_PROJECTION;
      return status;

    case 16:    /* Point Perspective projection - not a map */

      status = mpSetValues( mp, mpMAP_PROJECTION_TYPE, 
      mpPOINT_PERSPECTIVE,NULL);
      if (status != mpSUCCESS) return status;
      status = mpSetValues( mp, mpFOCAL_LENGTH, (double)float_buf[26], NULL);
      if (status != mpSUCCESS) return status;
      status = mpSetValues( mp, mpOPT_AXIS_INTERCEPT_LINE, 
      (double)float_buf[27],NULL);
      if (status != mpSUCCESS) return status;
      status = mpSetValues( mp, mpOPT_AXIS_INTERCEPT_SAMPLE, 
      (double)float_buf[28],NULL);
      if (status != mpSUCCESS) return status;
      status = mpSetValues( mp, mpFOCAL_PLANE_SCALE, (double)float_buf[29], NULL);
      if (status != mpSUCCESS) return status;
      status = mpSetValues( mp, mpSUB_SPACECRAFT_LATITUDE, 
      (double)float_buf[30], NULL);
      if (status != mpSUCCESS) return status;
      status = mpSetValues( mp, mpSUB_SPACECRAFT_LONGITUDE, 
      (double)float_buf[31], NULL);
      if (status != mpSUCCESS) return status;
      status = mpSetValues( mp, mpPLANET_CENTER_LINE, (double)float_buf[32],NULL);
      if (status != mpSUCCESS) return status;
      status = mpSetValues( mp, mpPLANET_CENTER_SAMPLE, (double)float_buf[33],
       NULL);
      if (status != mpSUCCESS) return status;
      status = mpSetValues( mp, mpNORTH_ANGLE, (double)float_buf[34], NULL);
      if (status != mpSUCCESS) return status;
      status = mpSetValues( mp, mpTARGET_CENTER_DISTANCE, 
      (double)float_buf[37], NULL);
      if (status != mpSUCCESS) return status;
      break;

      /* Standard map projections */

    case 1:     /*     Polar Orthographic        */

      status = mpSetValues( mp, mpMAP_PROJECTION_TYPE, mpPOLAR_ORTHOGRAPHIC,NULL);
      if (status != mpSUCCESS) return status;
      status = mpSetValues( mp, mpCENTER_LATITUDE, (double)float_buf[2], NULL);
      if (status != mpSUCCESS) return status;
      status = mpSetValues( mp, mpCARTESIAN_AZIMUTH, (double)0, NULL);
      if (status != mpSUCCESS) return status;
      x = float_buf[0] - 1.0;
      status = mpSetValues( mp, mpSAMPLE_PROJECTION_OFFSET, x, NULL);
      if (status != mpSUCCESS) return status;
      break;

    case 2:     /*     Oblique Orthographic        */

      status = mpSetValues( mp, mpMAP_PROJECTION_TYPE, mpOBLIQUE_ORTHOGRAPHIC,
       NULL);
      if (status != mpSUCCESS) return status;
      status = mpSetValues( mp, mpCARTESIAN_AZIMUTH, (double)float_buf[8], NULL);
      if (status != mpSUCCESS) return status;
      status = mpSetValues( mp, mpCENTER_LATITUDE, (double)float_buf[2], NULL);
      if (status != mpSUCCESS) return status;
      x = float_buf[0] - 1.0;
      status = mpSetValues( mp, mpSAMPLE_PROJECTION_OFFSET, x, NULL);
      if (status != mpSUCCESS) return status;
      break;

    case 3:     /*    Polar Stereographic        */

      status = mpSetValues( mp, mpMAP_PROJECTION_TYPE, mpPOLAR_STEREOGRAPHIC,
       NULL);
      if (status != mpSUCCESS) return status;
      status = mpSetValues( mp, mpCENTER_LATITUDE, (double)float_buf[2], NULL);
      if (status != mpSUCCESS) return status;
      status = mpSetValues( mp, mpCARTESIAN_AZIMUTH, (double)0, NULL);
      if (status != mpSUCCESS) return status;
      x = float_buf[0] - 1.0;
      status = mpSetValues( mp, mpSAMPLE_PROJECTION_OFFSET, x, NULL);
      if (status != mpSUCCESS) return status;
      break;

    case 4:     /*     Oblique Stereographic        */

      status = mpSetValues( mp, mpMAP_PROJECTION_TYPE, mpOBLIQUE_STEREOGRAPHIC,
       NULL);
      if (status != mpSUCCESS) return status;
      status = mpSetValues( mp, mpCARTESIAN_AZIMUTH, (double)float_buf[8], NULL);
      if (status != mpSUCCESS) return status;
      status = mpSetValues( mp, mpCENTER_LATITUDE, (double)float_buf[2], NULL);
      if (status != mpSUCCESS) return status;
      x = float_buf[0] - 1.0;
      status = mpSetValues( mp, mpSAMPLE_PROJECTION_OFFSET, x, NULL);
      if (status != mpSUCCESS) return status;
      break;

    case 5:     /*     Lambert Conformal Conic        */

      status = mpSetValues( mp, mpMAP_PROJECTION_TYPE, mpLAMBERT_CONFORMAL, NULL);
      if (status != mpSUCCESS) return status;
      status = mpSetValues( mp, mpFIRST_STANDARD_PARALLEL, (double)float_buf[3],
       NULL);
      if (status != mpSUCCESS) return status;
      status = mpSetValues( mp, mpSECOND_STANDARD_PARALLEL,
       (double)float_buf[4],NULL);
      if (status != mpSUCCESS) return status;
      status = mpSetValues( mp, mpCARTESIAN_AZIMUTH, (double)0, NULL);
      if (status != mpSUCCESS) return status;
      x = float_buf[0] - 1.0;
      status = mpSetValues( mp, mpSAMPLE_PROJECTION_OFFSET, x, NULL);
      if (status != mpSUCCESS) return status;
      break;

    case 6:     /*    Mercator            */

      status = mpSetValues( mp, mpMAP_PROJECTION_TYPE, mpMERCATOR, NULL);
      if (status != mpSUCCESS) return status;
      status = mpSetValues( mp, mpCENTER_LATITUDE, (double)float_buf[2], NULL);
      if (status != mpSUCCESS) return status;
      status = mpSetValues( mp, mpCARTESIAN_AZIMUTH, (double)0, NULL);
      if (status != mpSUCCESS) return status;
      x = float_buf[0] - 1.0;
      status = mpSetValues( mp, mpSAMPLE_PROJECTION_OFFSET, x, NULL);
      if (status != mpSUCCESS) return status;
      break;

    case 9:     /*     Cylindrical Equal Area        */

      status = mpSetValues( mp, mpMAP_PROJECTION_TYPE, mpNORMAL_CYLINDRICAL,NULL);
      if (status != mpSUCCESS) return status;

      /* Set FIRST_STANDARD_PARALLEL to zero.
       * Previous map projections of Cylindrical Equal Area assumed 
       * constant scale at the equator (standard parallel defined at the
       * equator.
       */
      status = mpSetValues( mp, mpFIRST_STANDARD_PARALLEL, (double)0, NULL);
      if (status != mpSUCCESS) return status;
      status = mpSetValues( mp, mpCENTER_LATITUDE, (double)float_buf[2], NULL);
      if (status != mpSUCCESS) return status;
      status = mpSetValues( mp, mpCARTESIAN_AZIMUTH, (double)0, NULL);
      if (status != mpSUCCESS) return status;
      status = mpSetValues( mp, mpSAMPLE_PROJECTION_OFFSET, 0.0, NULL);
      if (status != mpSUCCESS) return status;
      break;

    case 10:     /*    Equidistant Cylindrical        */

      status = mpSetValues( mp, mpMAP_PROJECTION_TYPE, mpSIMPLE_CYLINDRICAL,NULL);
      if (status != mpSUCCESS) return status;
      status = mpSetValues( mp, mpFIRST_STANDARD_PARALLEL, (double)float_buf[3],
       NULL);
      if (status != mpSUCCESS) return status;
      status = mpSetValues( mp, mpCENTER_LATITUDE, (double)float_buf[2], NULL);
      if (status != mpSUCCESS) return status;
      status = mpSetValues( mp, mpCARTESIAN_AZIMUTH, (double)0, NULL);
      if (status != mpSUCCESS) return status;
      x = float_buf[0] - 1.0;
      status = mpSetValues( mp, mpSAMPLE_PROJECTION_OFFSET, x, NULL);
      if (status != mpSUCCESS) return status;
      break;

    case 11:     /*    Oblique Equidistant Cylindrical        */

      status = mpSetValues( mp, mpMAP_PROJECTION_TYPE,
       mpOBLIQUE_SIMPLE_CYLINDRICAL, NULL);
      if (status != mpSUCCESS) return status;
      status = mpSetValues( mp, mpCARTESIAN_AZIMUTH, (double)float_buf[3], NULL);
      if (status != mpSUCCESS) return status;
      status = mpSetValues( mp, mpCENTER_LATITUDE, (double)float_buf[2], NULL);
      if (status != mpSUCCESS) return status;
      x = float_buf[0] - 1.0;
      status = mpSetValues( mp, mpSAMPLE_PROJECTION_OFFSET, x, NULL);
      if (status != mpSUCCESS) return status;
      break;

    case 12:     /*    Sinusoidal            */

      status = mpSetValues( mp, mpMAP_PROJECTION_TYPE, mpSINUSOIDAL, NULL);
      if (status != mpSUCCESS) return status;
      status = mpSetValues( mp, mpCENTER_LATITUDE, (double)float_buf[2], NULL);
      if (status != mpSUCCESS) return status;
      status = mpSetValues( mp, mpCARTESIAN_AZIMUTH, (double)0, NULL);
      if (status != mpSUCCESS) return status;
      x = float_buf[0] - 1.0;
      status = mpSetValues( mp, mpSAMPLE_PROJECTION_OFFSET, x, NULL);
      if (status != mpSUCCESS) return status;
      break;

    case 13:     /*    Oblique sinusoidal        */

      status = mpSetValues( mp, mpMAP_PROJECTION_TYPE, mpOBLIQUE_SINUSOIDAL,NULL);
      if (status != mpSUCCESS) return status;
      status = mpSetValues( mp, mpCARTESIAN_AZIMUTH, (double)float_buf[3], NULL);
      if (status != mpSUCCESS) return status;
      status = mpSetValues( mp, mpCENTER_LATITUDE, (double)float_buf[2], NULL);
      if (status != mpSUCCESS) return status;
      x = float_buf[0] - 1.0;
      status = mpSetValues( mp, mpSAMPLE_PROJECTION_OFFSET, x, NULL);
      if (status != mpSUCCESS) return status;
      break;

    case 14:     /*     Mollweide            */

      status = mpSetValues( mp, mpMAP_PROJECTION_TYPE, mpMOLLWEIDE, NULL);
      if (status != mpSUCCESS) return status;
      status = mpSetValues( mp, mpCARTESIAN_AZIMUTH,(double)0 , NULL);
      if (status != mpSUCCESS) return status;
      x = float_buf[0] - 1.0;
      status = mpSetValues( mp, mpSAMPLE_PROJECTION_OFFSET, x, NULL);
      if (status != mpSUCCESS) return status;
      break;

    case 15:     /*    Transverse Mercator        */

      status = mpSetValues( mp, mpMAP_PROJECTION_TYPE, mpTRANSVERSE_MERCATOR,
       NULL);
      if (status != mpSUCCESS) return status;
      status = mpSetValues( mp, mpCENTER_LATITUDE, (double)float_buf[2], NULL);
      if (status != mpSUCCESS) return status;
      status = mpSetValues( mp, mpCARTESIAN_AZIMUTH, (double)0, NULL);
      if (status != mpSUCCESS) return status;
      x = float_buf[0] - 1.0;
      status = mpSetValues( mp, mpSAMPLE_PROJECTION_OFFSET, x, NULL);
      if (status != mpSUCCESS) return status;
      break;

  }
	
  /* Items common to all projections */

  status = mpSetValues( mp, mpA_AXIS_RADIUS, (double)float_buf[25], NULL);
  if (status != mpSUCCESS) return status;

  /* word 36 holds the short equatorial axis for the triaxial case in
   * routine persp_proj, but for other cases this is zero ... */
  if (float_buf[36]>0.0001) {		/* (may be some garbage there) */
    status = mpSetValues( mp, mpB_AXIS_RADIUS, (double)float_buf[36], NULL);
  }
  else {
    status = mpSetValues( mp, mpB_AXIS_RADIUS, (double)float_buf[25], NULL);
  }
  if (status != mpSUCCESS) return status;

  status = mpSetValues( mp, mpC_AXIS_RADIUS, (double)float_buf[24], NULL);
  if (status != mpSUCCESS) return status;
 
/* 
  status = mpSetValues( mp, mpBODY_LONG_AXIS,(double)float_buf[35], NULL);
  if (status != mpSUCCESS) return status;
*/

  /* old VICAR always assumes West Longitudes and Planetocentric latitudes: */
  status = mpSetValues( mp, mpPOSITIVE_LONGITUDE_DIRECTION, "WEST", NULL);
  if (status != mpSUCCESS) return status;
  status = mpSetValues( mp, mpCOORDINATE_SYSTEM_NAME, "PLANETOCENTRIC", NULL);
  if (status != mpSUCCESS) return status;

  /* 
   * The remaining items are common to all standard map projections:
   */
  if ( int_buf[38] != 16 ) {

    /* PDS offsets are with respect to pixel (1,1), while VICAR's are to (0,0)*/
    x = float_buf[1] - 1.0;
    status = mpSetValues( mp, mpLINE_PROJECTION_OFFSET, x, NULL);
    if (status != mpSUCCESS) return status;
    status = mpSetValues( mp, mpCENTER_LONGITUDE, (double)float_buf[5], NULL);
    if (status != mpSUCCESS) return status;
    status = mpSetValues( mp, mpMAP_SCALE, (double)float_buf[6], NULL);
    if (status != mpSUCCESS) return status;

    /* note this item is functionally identical to CARTESIAN_AZIMUTH
     * in all VICAR map projections, so we use the latter and set
     * this to zero: */
    status = mpSetValues( mp, mpSPHERICAL_AZIMUTH, (double)0, NULL);
    if (status != mpSUCCESS) return status;
  }

  return status;
}
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create mp_labels.imake
#define SUBROUTINE  	mp_labels

#define MODULE_LIST 	mplabelread.c mplabelwrite.c \
			mpbuf2mpo.c

#define P1_SUBLIB

#define USES_ANSI_C

#define LIB_LOCAL	/* for development, remove on delivery */
#define DEBUG		/* for development, remove on delivery */

$ Return
$!#############################################################################
$Test_File:
$ create tmp_routines.c
#include <stdio.h>
#include "vicmain_c"
#include <math.h>
#include "mp_routines.h"

#define MAX_LATS_LONS 	10

/**********************************************************************
 
Test Program TMP_ROUTINES

Program calls mpGetPar which calls mpInit to allocate memory 
for a map projection data object and then sets values in the
data object based on values passed by the application programs
parameter list. Then mpll2xy and mpxy2ll are called to perform
point transformations.

Author:			Justin McNeill
Cognizant Engineer:	Justin McNeill
Date Written:		May 1994
Revision history:	
			August 1994	(JFM)
		
			Revised test program to simplify output
			of points to a tabular form, ending us of
			VERBOSE/DEBUG mode of output.	
*/
void main44()
{
int 	i,j,k;
int	count;
int	def;
int	status;
int	indices[2],lengthes[2];
int	number_keywords;
int	types[mpNUMBER_OF_KEYWORDS],classes[mpNUMBER_OF_KEYWORDS];
int	ll_type;
int	lat_count,lon_count;
int	line_count,samp_count;
char 	lat_lon_type[20];

double	double_value;
float	lines[MAX_LATS_LONS];
float	samples[MAX_LATS_LONS];
float	latitudes[MAX_LATS_LONS];
float	longitudes[MAX_LATS_LONS];

double	latitude;
double	longitude;
double	line;
double	sample;

double	new_lat;
double	new_lon;
double	new_line;
double	new_samp;

char	keys[mpNUMBER_OF_KEYWORDS][mpMAX_KEYWD_LENGTH+1];
char	pdf_parms[mpNUMBER_OF_KEYWORDS][mpMAX_PARM_LENGTH+1];
char	pds_keys[mpNUMBER_OF_KEYWORDS][mpMAX_KEYWD_LENGTH+1];
char    PCKfile[200];
char	PCKpath[200];
char	string[300];
char	string_value[200];

MP mp_obj;

zvmessage("***************************************************"," ");
zvmessage("\n\tTest of MP routines in C\n"," ");
zvmessage("***************************************************\n"," ");

/*

Define user parameters to be retrieved from PDF file.

*/

strcpy(pdf_parms[0],"TARGET");
strcpy(pds_keys[0],mpTARGET_NAME);

strcpy(pdf_parms[1],"PROJ");
strcpy(pds_keys[1],mpMAP_PROJECTION_TYPE);

strcpy(pdf_parms[2],"A_AXIS");
strcpy(pds_keys[2],mpA_AXIS_RADIUS);

strcpy(pdf_parms[3],"B_AXIS");
strcpy(pds_keys[3],mpB_AXIS_RADIUS);

strcpy(pdf_parms[4],"C_AXIS");
strcpy(pds_keys[4],mpC_AXIS_RADIUS);

strcpy(pdf_parms[5],"SCALE");
strcpy(pds_keys[5],mpMAP_SCALE);

strcpy(pdf_parms[6],"RESOLUTION");
strcpy(pds_keys[6],mpMAP_RESOLUTION);

strcpy(pdf_parms[7],"POS_LON_DIR");
strcpy(pds_keys[7],mpPOSITIVE_LONGITUDE_DIRECTION);

strcpy(pdf_parms[8],"CTR_LAT");
strcpy(pds_keys[8],mpCENTER_LATITUDE);

strcpy(pdf_parms[9],"CTR_LON");
strcpy(pds_keys[9],mpCENTER_LONGITUDE);

strcpy(pdf_parms[10],"SPHERICAL_AZ");
strcpy(pds_keys[10],mpSPHERICAL_AZIMUTH);

strcpy(pdf_parms[11],"CARTESIAN_AZ");
strcpy(pds_keys[11],mpCARTESIAN_AZIMUTH);

strcpy(pdf_parms[12],"LINE_OFFSET");
strcpy(pds_keys[12],mpLINE_PROJECTION_OFFSET);

strcpy(pdf_parms[13],"SAMPLE_OFFSET");
strcpy(pds_keys[13],mpSAMPLE_PROJECTION_OFFSET);

strcpy(pdf_parms[14],"PARALLEL_ONE");
strcpy(pds_keys[14],mpFIRST_STANDARD_PARALLEL);

strcpy(pdf_parms[15],"PARALLEL_TWO");
strcpy(pds_keys[15],mpSECOND_STANDARD_PARALLEL);

strcpy(pdf_parms[16],"XYZ");
strcpy(pds_keys[16],mpFOCAL_LENGTH);

strcpy(pdf_parms[17],"TGT_DIST");
strcpy(pds_keys[17],mpTARGET_CENTER_DISTANCE);

strcpy(pdf_parms[18],"AXIS_LINE");
strcpy(pds_keys[18],mpOPT_AXIS_INTERCEPT_LINE);

strcpy(pdf_parms[19],"AXIS_SAMPLE");
strcpy(pds_keys[19],mpOPT_AXIS_INTERCEPT_SAMPLE);

strcpy(pdf_parms[20],"FOCAL_SCALE");
strcpy(pds_keys[20],mpFOCAL_PLANE_SCALE);

strcpy(pdf_parms[21],"SUB_SPACE_LAT");
strcpy(pds_keys[21],mpSUB_SPACECRAFT_LATITUDE);

strcpy(pdf_parms[22],"SUB_SPACE_LON");
strcpy(pds_keys[22],mpSUB_SPACECRAFT_LONGITUDE);

strcpy(pdf_parms[23],"CENT_LINE");
strcpy(pds_keys[23],mpPLANET_CENTER_LINE);

strcpy(pdf_parms[24],"CENT_SAMPLE");
strcpy(pds_keys[24],mpPLANET_CENTER_SAMPLE);

strcpy(pdf_parms[25],"N_ANGLE");
strcpy(pds_keys[25],mpNORTH_ANGLE);

strcpy(pdf_parms[26],"LONG_AXIS");
strcpy(pds_keys[26],mpBODY_LONG_AXIS);

strcpy(pdf_parms[27],"LL_TYPE");
strcpy(pds_keys[27],mpCOORDINATE_SYSTEM_NAME);

strcpy(pdf_parms[28],"TGTBOD");
strcpy(pds_keys[28],mpTARGET_BODY);

pdf_parms[29][0] = '\0';
pds_keys[29][0] = '\0';

/*

Set user parameters for subsequent input to mpLL2XY or mpXY2LL

*/
status = zvparm("PCK_PATH",PCKpath,&count,&def,1,0 );
if ( status < 0 )
	{
	zvmessage("SPICE PCK file pathname not found."," ");
	zvmessage("Pathname set to blank"," ");
	strcpy(PCKpath," ");
	}
zvsptr(PCKpath,count,indices,lengthes);
indices[0] -= 1;
strncpy(PCKfile,&PCKpath[indices[0]],lengthes[0]);
PCKfile[lengthes[0]]='\0';
/*

Convert input path for PCK file to absolute path.

*/
status = zvfilename( PCKfile, PCKpath, 0 );
ABENDif( status < VICARrtlSUCCESS );

status = mpGetPar( &mp_obj,pdf_parms,pds_keys,PCKpath );
if ( status < 0 )
	{
	zvmessage("Error in mpgetpar call"," ");
	zvmessage("Test failed."," ");
	zabend();
	}

status = mpGetKeywords( mp_obj,keys,&number_keywords,types,classes );
if ( status < 0 )
	{
	zvmessage("Error in mpGetKeywords call"," ");
	zvmessage("Test failed."," ");
	zabend();
	}

status = mpSetDebugFlag( FALSE );
ABENDif( status<mpSUCCESS );

for ( i=0; i<number_keywords; i++ )
     	switch ( types[i] )	{

	case mpCHAR:

		status = mpGetValues( mp_obj,keys[i],string_value,"" );
		ABENDif( status < mpSUCCESS );
		
		sprintf(string,"KEYWORD %s equals %s",keys[i],string_value);
		zvmessage(string," ");
		
		break;

	case mpDBLE:

		status = mpGetValues( mp_obj,keys[i],&double_value,"" );
		ABENDif( status < mpSUCCESS );
		
		sprintf(string,"KEYWORD %s equals %4.3e",keys[i],double_value);
		zvmessage(string," ");

		break;

	default:

		zvmessage("PDS KEY of unacceptable data type"," ");
		break;	}


/*

Print output banner for map transformation

*/

zvmessage(" "," ");
zvmessage("***************************************************"," ");
zvmessage("\n\tTransformation results:"," ");
zvmessage(" "," ");

/*

Determine input latitude and longitude type specified as input.

*/

status = zvp("LL_TYPE",lat_lon_type,&count);
ABENDif( status < mpSUCCESS );

if ( strcmp(lat_lon_type,"PLANETOCENTRIC") == 0 )
	{
	ll_type = 1;
	zvmessage("\n\tPlanetocentric lat/lon pairs\n"," ");
	}
if ( (strcmp(lat_lon_type,"PLANETOGRAPHIC") == 0) ||
     (strcmp(lat_lon_type,"PLANETODETIC") == 0) )
	{
	ll_type = 2;
	zvmessage("\n\tPlanetographic lat/lon pairs\n"," ");
	}
if ( strcmp(lat_lon_type,"SNYDER_DEFINED") == 0 )
	{
	ll_type = 3;
	zvmessage("\n\tSnyder-defined lat/lon pairs\n"," ");
	}

/*

TRANSFORMATIONS: forward and inverse

*/

zvmessage(" ",0);
zvmessage(" Table of Point Transformations",0);
zvmessage(" ",0);

/*

Get latitude and longitude array from parameter values.

*/

status = zvp("LATITUDES",latitudes,&lat_count);
ABENDif( status < mpSUCCESS );

status = zvp("LONGITUDES",longitudes,&lon_count);
ABENDif( status < mpSUCCESS );

if ( lat_count > 0 )
	{
	zvmessage(" (Lat,Lon) -> (Line,Sample) -> (Lat',Lon')",0);
	zvmessage(" ",0);
	}
else
	{
	zvmessage(" (Line,Sample) -> (Lat,Lon) -> (Line',Sample')",0);
	zvmessage(" ",0);
	}

for( i=0; i<lat_count; i++ )
	{
	latitude = (double) latitudes[i];
	longitude = (double) longitudes[i];

	status = mpll2xy( mp_obj,&line,&sample,latitude,longitude,ll_type );
	if ( status < mpSUCCESS )
		{
		sprintf(string,"*** mpLL2XY error on lat,lon = (%e,%e)",
			latitude,longitude);
		zvmessage(string,0);
		break;
		}
	status = mpxy2ll( mp_obj,line,sample,&new_lat,&new_lon,ll_type );
	if ( status < mpSUCCESS )
		{
		sprintf(string,"*** mpXY2LL error on line,sample = (%e,%e)",
			line,sample);
		zvmessage(string,0);
		break;
		}

	sprintf(string," (%8.3f,%8.3f) -> (%8.3f,%8.3f) -> (%8.3f,%8.3f)",
		latitude,longitude,line,sample,new_lat,new_lon);
	zvmessage(string,0);
	}

/*

Get line and sample array from parameter values.

*/

status = zvp("LINES",lines,&line_count);
ABENDif( status < mpSUCCESS );

status = zvp("SAMPLES",samples,&samp_count);
ABENDif( status < mpSUCCESS );

for( i=0; i<line_count; i++ )
	{
	line = (double) lines[i];
	sample = (double) samples[i];

	status = mpxy2ll( mp_obj,line,sample,&latitude,&longitude,ll_type );
	if ( status < mpSUCCESS )
		{
		sprintf(string,"*** mpXY2LL error on line,sample = (%e,%e)",
			line,sample);
		zvmessage(string,0);
		break;
		}

	status = mpll2xy( mp_obj,&new_line,&new_samp,
			latitude,longitude,ll_type );
	if ( status < mpSUCCESS )
		{
		sprintf(string,"*** mpLL2XY error on lat,lon = (%e,%e)",
			latitude,longitude);
		zvmessage(string,0);
		break;
		}

	sprintf(string," (%8.3f,%8.3f) -> (%9.3f,%9.3f) -> (%8.3f,%8.3f)",
		line,sample,latitude,longitude,new_line,new_samp);
	zvmessage(string,0);
	}

zvmessage(" "," ");
zvmessage("***************************************************"," ");
zvmessage("\n\tEnd test of MP routines in C\n"," ");
zvmessage("***************************************************"," ");
zvmessage(" "," ");

mpFree( mp_obj );
}
$!-----------------------------------------------------------------------------
$ create tmp_routines.imake
#define PROGRAM tmp_routines
#define MODULE_LIST tmp_routines.c

#define MAIN_LANG_C
#define R2LIB 

#define USES_ANSI_C
#define USES_FORTRAN

#define LIB_RTL
#define LIB_P1SUB
#define LIB_SPICE
#define LIB_P2SUB
#define LIB_TAE
#define LIB_FORTRAN
#define LIB_MATH77

#if 0
#define LIB_LOCAL	/* for development, remove on delivery */
#define DEBUG		/* for development, remove on delivery */
#endif
$!-----------------------------------------------------------------------------
$ create tmp_routines.pdf
process help=*
PARM TARGET		STRING	COUNT=0:1       DEFAULT=--
PARM PROJ		STRING	COUNT=1
PARM A_AXIS		REAL	COUNT=0:1	DEFAULT=--		
PARM B_AXIS		REAL	COUNT=0:1	DEFAULT=--
PARM C_AXIS		REAL	COUNT=0:1	DEFAULT=--
PARM SCALE		REAL	COUNT=0:1	DEFAULT=--
PARM RESOLUTION		REAL	COUNT=0:1	DEFAULT=--
PARM POS_LON_DIR	STRING	COUNT=1
PARM CTR_LAT		REAL	COUNT=1
PARM CTR_LON		REAL	COUNT=1
PARM SPHERICAL_AZ	REAL	COUNT=0:1	DEFAULT=0.0
PARM CARTESIAN_AZ	REAL	COUNT=0:1	DEFAULT=0.0
PARM LINE_OFFSET	REAL	COUNT=0:1	DEFAULT=0.0
PARM SAMPLE_OFFSET	REAL	COUNT=0:1	DEFAULT=0.0
PARM PARALLEL_ONE	REAL	COUNT=0:1	DEFAULT=--
PARM PARALLEL_TWO	REAL	COUNT=0:1	DEFAULT=--
PARM LONG_AXIS		REAL	COUNT=0:1	DEFAULT=--
PARM TGTBOD		STRING 	COUNT=0:1	DEFAULT=--
PARM PCK_PATH		STRING 	COUNT=1
PARM LL_TYPE		STRING  COUNT=1         
PARM LATITUDES		REAL	COUNT=0:10
PARM LONGITUDES		REAL	COUNT=0:10
PARM LINES		REAL	COUNT=0:10
PARM SAMPLES		REAL	COUNT=0:10
PARM TGT_DIST		REAL	COUNT=0:1	DEFAULT=--		
PARM SC_DIST		REAL	COUNT=0:1	DEFAULT=--		
PARM XYZ		REAL	COUNT=0:1	DEFAULT=--
PARM AXIS_LINE		REAL	COUNT=0:1	DEFAULT=--
PARM AXIS_SAMPLE	REAL	COUNT=0:1	DEFAULT=--
PARM FOCAL_SCALE	REAL	COUNT=0:1	DEFAULT=--
PARM SUB_SPACE_LAT	REAL	COUNT=0:1	DEFAULT=--
PARM SUB_SPACE_LON	REAL	COUNT=0:1	DEFAULT=--
PARM CENT_LINE		REAL	COUNT=0:1	DEFAULT=--
PARM CENT_SAMPLE	REAL	COUNT=0:1	DEFAULT=--
PARM N_ANGLE		REAL	COUNT=0:1	DEFAULT=--
end-proc
.TITLE
VICAR program TMP_ROUTINES
.HELP
PURPOSE:
This program is a simple test program for the HWCARTO
subroutine. It calls zhwgetpar, zhwcarto and mpFree and
prints the contents of the map projection object after zhwgetpar 
is called and the transformation results. MP routines are
embedded in the calls of zhwgetpar and zhwcarto.
.LEVEL1

.VARI TARGET
Target body of object for which map projection points will
be transformed.

.VARI PROJ
Map projection type requested.

.VARI A_AXIS
Semimajor axis of target body.

.VARI B_AXIS
Semiminor axis of target body.

.VARI C_AXIS
Polar axis of target body.

.VARI SCALE
Map scale.

.VARI RESOLUTION
Map resolution.

.VARI POS_LON_DIR
Positive longitude direction.

.VARI CTR_LAT
Center latitude

.VARI CTR_LON
Center longitude

.VARI SPHERICAL_AZ
Spherical azimuth

.VARI CARTESIAN_AZ
Cartesian azimuth

.VARI LINE_OFFSET
Line projection offset

.VARI SAMPLE_OFFSET
Sample projection offset

.VARI PARALLEL_ONE
First standard parallel

.VARI PARALLEL_TWO
Second standard paralel

.VARI LONG_AXIS
Body long axis offset longitude

.VARI LAT_LON_TYPE
Type of latitude and longitude as input or to be returned.
Valid types are 'PLANETOCENTRIC', 'PLANETODETIC', and 'SNYDER-DEFINED'.

.VARI LATITUDES
Array of planetocentric, planetodetic or Snyder-defined latitudes on
a target body.

.VARI LONGITUDES
Array of planetOcentric, planetodetic or Snyder-defined longitudes on
a target body. NOTE THAT LONGITUDES ARE PLANETODETIC, PLANETOCENTRIC
OR SNYDER-DEFINED ONLY WITH THE TRIAXIAL ELLIPSOID MODEL.

.END
$!-----------------------------------------------------------------------------
$ create tmp_routines_f.f
	include 'VICMAIN_FOR'

C**********************************************************************
C
C	Test Program TMP_ROUTINES
C
C	Program calls mpGetPar which calls mpInit to allocate memory 
C	for a map projection data object and then sets values in the	
C	data object based on values passed by the application programs
C	parameter list. Then mpll2xy and mpxy2ll are called to perform
C	point transformations.
C
C	Author:			Justin McNeill
C	Cognizant Engineer:	Justin McNeill
C	Date Written:		May 1994	
C	Revision history:	Original
C	
C**********************************************************************

	subroutine main44
	implicit integer (a-y)
	implicit real*8  (z)
	
	include 'mp_for_defs'

	integer i
	integer count
	integer status
	integer number_keywords
    	integer types(mp_number_of_keywords)
	integer classes(mp_number_of_keywords)
	integer ll_type
	integer lat_count, lon_count

	real*8 mp

	character*20 lat_lon_type
	character*100 msg

	real*8 double_value
	real*8 lines(10)
	real*8 samples(10)
	real*4 latitudes(10)
	real*4 longitudes(10)

	real*8 latitude
	real*8 longitude
	real*8 new_lat
	real*8 new_lon

	character*(mp_max_keywd_length) keys(mp_number_of_keywords)
	character*(mp_max_keywd_length) pdf_parms(mp_number_of_keywords)
	character*(mp_max_keywd_length) pds_keys(mp_number_of_keywords)

	character*200 pck_file
	
	character*40 string_value

	call xvmessage(' ',' ')
	call xvmessage('*********************************************'
     .		,' ')
	call xvmessage(' ',' ')
	call xvmessage('     FORTRAN Test of MP Routines',' ')
	call xvmessage(' ',' ')
	call xvmessage('*********************************************'
     .		,' ')
	call xvmessage(' ',' ')

C
C	Set PDF parms to PDS keywords mappings
C	

	pdf_parms(1) = 'TARGET'
	pds_keys(1) = 'TARGET_NAME'

	pdf_parms(2) = 'PROJ'
	pds_keys(2) = 'MAP_PROJECTION_TYPE'

	pdf_parms(3) = 'A_AXIS'
	pds_keys(3) = 'A_AXIS_RADIUS'

	pdf_parms(4) = 'B_AXIS'
	pds_keys(4) = 'B_AXIS_RADIUS'

	pdf_parms(5) = 'C_AXIS'
	pds_keys(5) = 'C_AXIS_RADIUS'

	pdf_parms(6) = 'SCALE'
	pds_keys(6) = 'MAP_SCALE'

	pdf_parms(7) = 'RESOLUTION'
	pds_keys(7) = 'MAP_RESOLUTION'

	pdf_parms(8) = 'POS_LON_DIR'
	pds_keys(8) = 'POSITIVE_LONGITUDE_DIRECTION'

	pdf_parms(9) = 'CTR_LAT'
	pds_keys(9) = 'CENTER_LATITUDE'

	pdf_parms(10) = 'CTR_LON'
	pds_keys(10) = 'CENTER_LONGITUDE'

	pdf_parms(11) = 'SPHERICAL_AZ'
	pds_keys(11) = 'SPHERICAL_AZIMUTH'

	pdf_parms(12) = 'CARTESIAN_AZ'
	pds_keys(12) = 'CARTESIAN_AZIMUTH'

	pdf_parms(13) = 'LINE_OFFSET'
	pds_keys(13) = 'LINE_PROJECTION_OFFSET'

	pdf_parms(14) = 'SAMPLE_OFFSET'
	pds_keys(14) = 'SAMPLE_PROJECTION_OFFSET'

	pdf_parms(15) = 'PARALLEL_ONE'
	pds_keys(15) = 'FIRST_STANDARD_PARALLEL'

	pdf_parms(16) = 'PARALLEL_TWO'
	pds_keys(16) = 'SECOND_STANDARD_PARALLEL'

	pdf_parms(17) = 'XYZ'
	pds_keys(17) = 'FOCAL_LENGTH'

	pdf_parms(18) = 'TGT_DIST'
	pds_keys(18) = 'TARGET_CENTER_DISTANCE'

	pdf_parms(19) = 'AXIS_LINE'
	pds_keys(19) = 'OPT_AXIS_INTERCEPT_LINE'

	pdf_parms(20) = 'AXIS_SAMPLE'
	pds_keys(20) = 'OPT_AXIS_INTERCEPT_SAMPLE'

	pdf_parms(21) = 'FOCAL_SCALE'
	pds_keys(21) = 'FOCAL_PLANE_SCALE'

	pdf_parms(22) = 'SUB_SPACE_LAT'
	pds_keys(22) = 'SUB_SPACECRAFT_LATITUDE'

	pdf_parms(23) = 'SUB_SPACE_LON'
	pds_keys(23) = 'SUB_SPACECRAFT_LONGITUDE'

	pdf_parms(24) = 'CENT_LINE'
	pds_keys(24) = 'PLANET_CENTER_LINE'

	pdf_parms(25) = 'CENT_SAMPLE'
	pds_keys(25) = 'PLANET_CENTER_SAMPLE'

	pdf_parms(26) = 'N_ANGLE'
	pds_keys(26) = 'NORTH_ANGLE'

	pdf_parms(27) = 'LONG_AXIS'
	pds_keys(27) = 'BODY_LONG_AXIS'

	pdf_parms(28) = 'LL_TYPE'
	pds_keys(28) = 'COORDINATE_SYSTEM_NAME'

	! test acceptance of old keword:
	pdf_parms(29) = 'TGTBOD'
	pds_keys(29) = 'TARGET_BODY'

	pdf_parms(30) = '\0'
	pds_keys(30) = '\0'

C
C	Retrieve NAIF planetary constants kernel path name
C

	call xvp('PCK_PATH',pck_file,count)

C
C	Set up MP data object	
C

	call xvfilename( pck_file, pck_file, status )
	if ( status.lt.0 ) call abend

	call mp_get_par( mp,pdf_parms,pds_keys,pck_file )

	flag = 0

	call mp_set_debug_flag( flag,status )
	if ( status.lt.mp_success ) call abend

C
C	Verify keywords set
C

	call mp_get_keywords( mp,keys,number_keywords,types,classes,
     .		status )
	if ( status.lt.mp_success ) then
  		call xvmessage( 'Error in mp_get_keywords call',' ')
		call xvmessage( 'Test failed', ' ')
		call abend
	endif

C
C	Retrieve and print MP data object values
C

	do i=1,number_keywords
		
	  if( types(i).eq.mp_char ) then
	  	
	     call mp_get_value_str( mp,keys(i),string_value,status )
	     if ( status.lt.mp_success ) then
		call abend 
	     endif

	     write( msg, 1000 ) keys(i), string_value
	     call xvmessage( msg, ' ' )

	  else 
           
           if( types(i).eq.mp_dble ) then

	     call mp_get_value( mp,keys(i),double_value,status )
	     if ( status.lt.mp_success ) then
		call abend 
	     endif

	     write( msg, 1010 ) keys(i), double_value
	     call xvmessage( msg, ' ' )

	   else

	     call xvmessage( 'PDS KEY of unacceptable data type', ' ' )

 	   endif

	  endif	     
	
	enddo
		
C
C	Determine input latitude and longitude type specified as input
C

	call xvp('LL_TYPE',lat_lon_type,count)

	if ( lat_lon_type.eq.'PLANETOCENTRIC' ) ll_type = 1
	if ( lat_lon_type.eq.'PLANETOGRAPHIC' .or.
     &       lat_lon_type.eq.'PLANETODETIC' ) ll_type = 2

	if ( lat_lon_type.eq.'PLANETOGRAPHIC' ) ll_type = 2
	if ( lat_lon_type.eq.'SNYDER_DEFINED' ) ll_type = 3

C
C	Retrieve latitudes and longitudes from the PDS file
C

	call xvp('LATITUDES',latitudes,lat_count)
	call xvp('LONGITUDES',longitudes,lon_count)
	if ( lat_count.lt.lon_count ) then
		count = lat_count
	else
		count = lon_count
	endif

C
C       Perform the forward transformation
C
	call xvmessage(' ',' ')
	call xvmessage('     Table of Point Transformations',' ')
	call xvmessage(' ',' ')
	call xvmessage(' (Lat,Lon) -> (Line,Sample) -> (Lat",Lon")',
     .  		' ')
	call xvmessage(' ',' ')

	do i=1,count
	
	  latitude = latitudes(i)
	  longitude = longitudes(i)

	  call mp_ll2xy(mp,lines(i),samples(i),latitude,
     .   	longitude,ll_type,status)
	  if( status.lt.mp_success ) call abend

	  call mp_xy2ll(mp,lines(i),samples(i),new_lat,
     .   	new_lon,ll_type,status)
	  if( status.lt.mp_success ) call abend

	  write ( msg, 1020 ) latitude,longitude,
     .		lines(i),samples(i),new_lat,new_lon
	  call xvmessage( msg,' ' )

	enddo
	
	call xvmessage(' ',' ')
	call xvmessage('*********************************************'
     .		,' ')
	call xvmessage(' ',' ')
	call xvmessage('     End FORTRAN test of MP routines', ' ')
	call xvmessage(' ',' ')
	call xvmessage('*********************************************'
     .		,' ')
	call xvmessage(' ',' ')

 1000   format( '     ',a, ' = ', a)
 1010	format( '     ',a, ' = ', d10.3)
 1020   format (1X,'(',F8.3,',',F8.3,') -> (',F8.3,',',F8.3,') -> (',
     .          F8.3,',',F8.3,') ')
	end
$!-----------------------------------------------------------------------------
$ create tmp_routines_verbose.c
#include <stdio.h>
#include "vicmain_c"
#include <math.h>
#include "mp_routines.h"

#define MAX_LATS_LONS 	10

/**********************************************************************
 
Test Program TMP_ROUTINES_VERBOSE

Program calls mpGetPar which calls mpInit to allocate memory 
for a map projection data object and then sets values in the
data object based on values passed by the application programs
parameter list. Then mpll2xy and mpxy2ll are called to perform
point transformations.  DEBUG/VERBOSE option is used to show
internal calculations.

Author:			Justin McNeill
Cognizant Engineer:	Justin McNeill
Date Written:		May 1994
Revision history:	Original

*/
void main44()
{
int 	i,j,k;
int	count;
int	status;
int	indices[2],lengthes[2];
int	number_keywords;
int	types[mpNUMBER_OF_KEYWORDS],classes[mpNUMBER_OF_KEYWORDS];
int	ll_type;
int	lat_count,lon_count;
char 	lat_lon_type[20];

double	double_value;
double	lines[MAX_LATS_LONS];
double	samples[MAX_LATS_LONS];
float	latitudes[MAX_LATS_LONS];
float	longitudes[MAX_LATS_LONS];
double	latitude;
double	longitude;

char	keys[mpNUMBER_OF_KEYWORDS][mpMAX_KEYWD_LENGTH+1];
char	pdf_parms[mpNUMBER_OF_KEYWORDS][mpMAX_PARM_LENGTH+1];
char	pds_keys[mpNUMBER_OF_KEYWORDS][mpMAX_KEYWD_LENGTH+1];
char    PCKfile[200];
char	PCKpath[200];
char	string[300];
char	string_value[200];

MP mp_obj;

zvmessage("***************************************************"," ");
zvmessage("\n\tTest of MP routines in C\n"," ");
zvmessage("***************************************************\n"," ");

/*

Define user parameters to be retrieved from PDF file.

*/

strcpy(pdf_parms[0],"TARGET");
strcpy(pds_keys[0],mpTARGET_NAME);

strcpy(pdf_parms[1],"PROJ");
strcpy(pds_keys[1],mpMAP_PROJECTION_TYPE);

strcpy(pdf_parms[2],"A_AXIS");
strcpy(pds_keys[2],mpA_AXIS_RADIUS);

strcpy(pdf_parms[3],"B_AXIS");
strcpy(pds_keys[3],mpB_AXIS_RADIUS);

strcpy(pdf_parms[4],"C_AXIS");
strcpy(pds_keys[4],mpC_AXIS_RADIUS);

strcpy(pdf_parms[5],"SCALE");
strcpy(pds_keys[5],mpMAP_SCALE);

strcpy(pdf_parms[6],"RESOLUTION");
strcpy(pds_keys[6],mpMAP_RESOLUTION);

strcpy(pdf_parms[7],"POS_LON_DIR");
strcpy(pds_keys[7],mpPOSITIVE_LONGITUDE_DIRECTION);

strcpy(pdf_parms[8],"CTR_LAT");
strcpy(pds_keys[8],mpCENTER_LATITUDE);

strcpy(pdf_parms[9],"CTR_LON");
strcpy(pds_keys[9],mpCENTER_LONGITUDE);

strcpy(pdf_parms[10],"SPHERICAL_AZ");
strcpy(pds_keys[10],mpSPHERICAL_AZIMUTH);

strcpy(pdf_parms[11],"CARTESIAN_AZ");
strcpy(pds_keys[11],mpCARTESIAN_AZIMUTH);

strcpy(pdf_parms[12],"LINE_OFFSET");
strcpy(pds_keys[12],mpLINE_PROJECTION_OFFSET);

strcpy(pdf_parms[13],"SAMPLE_OFFSET");
strcpy(pds_keys[13],mpSAMPLE_PROJECTION_OFFSET);

strcpy(pdf_parms[14],"PARALLEL_ONE");
strcpy(pds_keys[14],mpFIRST_STANDARD_PARALLEL);

strcpy(pdf_parms[15],"PARALLEL_TWO");
strcpy(pds_keys[15],mpSECOND_STANDARD_PARALLEL);

strcpy(pdf_parms[16],"XYZ");
strcpy(pds_keys[16],mpFOCAL_LENGTH); 

strcpy(pdf_parms[17],"RANGE");
strcpy(pds_keys[17],mpTARGET_CENTER_DISTANCE);

strcpy(pdf_parms[18],"AXIS_LINE");
strcpy(pds_keys[18],mpOPT_AXIS_INTERCEPT_LINE);

strcpy(pdf_parms[19],"AXIS_SAMPLE");
strcpy(pds_keys[19],mpOPT_AXIS_INTERCEPT_SAMPLE);

strcpy(pdf_parms[20],"FOCAL_SCALE");
strcpy(pds_keys[20],mpFOCAL_PLANE_SCALE);

strcpy(pdf_parms[21],"SUB_SPACE_LAT");
strcpy(pds_keys[21],mpSUB_SPACECRAFT_LATITUDE);

strcpy(pdf_parms[22],"SUB_SPACE_LON");
strcpy(pds_keys[22],mpSUB_SPACECRAFT_LONGITUDE);

strcpy(pdf_parms[23],"CENT_LINE");
strcpy(pds_keys[23],mpPLANET_CENTER_LINE);

strcpy(pdf_parms[24],"CENT_SAMPLE");
strcpy(pds_keys[24],mpPLANET_CENTER_SAMPLE);

strcpy(pdf_parms[25],"N_ANGLE");
strcpy(pds_keys[25],mpNORTH_ANGLE);

strcpy(pdf_parms[26],"LONG_AXIS");
strcpy(pds_keys[26],mpBODY_LONG_AXIS);

strcpy(pdf_parms[27],"LL_TYPE");
strcpy(pds_keys[27],mpCOORDINATE_SYSTEM_NAME);

pdf_parms[28][0] = '\0';
pds_keys[28][0] = '\0';

/*

Set user parameters for subsequent input to mpLL2XY or mpXY2LL

*/
status = zvp("PCK_PATH",PCKpath,&count);
if ( status < 0 )
	{
	zvmessage("SPICE PCK file pathname not found."," ");
	zvmessage("Pathname set to blank"," ");
	strcpy(PCKpath," ");
	}

zvsptr(PCKpath,count,indices,lengthes);
indices[0] -= 1;
strncpy(PCKfile,&PCKpath[indices[0]],lengthes[0]);
PCKfile[lengthes[0]]='\0';
status = zvfilename( PCKfile, PCKpath, 0);
ABENDif( status < VICARrtlSUCCESS );

status = mpGetPar( &mp_obj,pdf_parms,pds_keys,PCKpath );
if ( status < 0 )
	{
	zvmessage("Error in mpgetpar call"," ");
	zvmessage("Test failed."," ");
	zabend();
	}

status = mpGetKeywords( mp_obj,keys,&number_keywords,types,classes );
if ( status < 0 )
	{
	zvmessage("Error in mpGetKeywords call"," ");
	zvmessage("Test failed."," ");
	zabend();
	}

status = mpSetDebugFlag( TRUE );
ABENDif( status<mpSUCCESS );


for ( i=0; i<number_keywords; i++ )
     	switch ( types[i] )	{

	case mpCHAR:

		status = mpGetValues( mp_obj,keys[i],string_value,"" );
		ABENDif( status < mpSUCCESS );
		
		sprintf(string,"KEYWORD %s equals %s",keys[i],string_value);
		zvmessage(string," ");
		
		break;

	case mpDBLE:

		status = mpGetValues( mp_obj,keys[i],&double_value,"" );
		ABENDif( status < mpSUCCESS );
		
		sprintf(string,"KEYWORD %s equals %4.3e",keys[i],double_value);
		zvmessage(string," ");

		break;

	default:

		zvmessage("PDS KEY of unacceptable data type"," ");
		break;	}

/*

Print output banner for map transformation

*/

zvmessage(" "," ");
zvmessage("***************************************************"," ");
zvmessage("\n\tTransformation results:"," ");
zvmessage(" "," ");

/*

Determine input latitude and longitude type specified as input.

*/

status = zvp("LL_TYPE",lat_lon_type,&count);
ABENDif( status < mpSUCCESS );

if ( strcmp(lat_lon_type,"PLANETOCENTRIC") == 0 )
	{
	ll_type = 1;
	zvmessage("\n\tPlanetocentric lat/lon pairs\n"," ");
	}
if ( (strcmp(lat_lon_type,"PLANETOGRAPHIC") == 0) ||
     (strcmp(lat_lon_type,"PLANETODETIC") == 0) )
	{
	ll_type = 2;
	zvmessage("\n\tPlanetographic lat/lon pairs\n"," ");
	}
if ( strcmp(lat_lon_type,"SNYDER_DEFINED") == 0 )
	{
	ll_type = 3;
	zvmessage("\n\tSnyder-defined lat/lon pairs\n"," ");
	}
/*

Get latitude and longitude array from parameter values.

*/

status = zvp("LATITUDES",latitudes,&lat_count);
ABENDif( status < mpSUCCESS );

status = zvp("LONGITUDES",longitudes,&lon_count);
ABENDif( status < mpSUCCESS );

if ( lat_count < lon_count )
	count = lat_count;
else
	count = lon_count;

/*

FORWARD TRANSFORMATION

*/

for( i=0; i<count; i++ )
	{
	latitude = (double) latitudes[i];
	longitude = (double) longitudes[i];

	zvmessage("\n******************************************\n"," ");
	zvmessage("\t(LAT,LON) -> (X,Y)"," ");
	zvmessage("\n\twhere"," ");
	sprintf(string,"\n\t(LAT,LON) = (%5.3f,%6.3f)\n",
		latitude,longitude);
	zvmessage(string," ");

	status = mpll2xy( mp_obj,&lines[i],&samples[i],
			latitude,longitude,ll_type );

	zvmessage("\n\tTransform completed."," ");
	zvmessage("\n******************************************\n"," ");
	}
/*

INVERSE TRANSFORMATION

*/

for( i=0; i<count; i++ )
	{
	zvmessage("\n******************************************\n"," ");
	zvmessage("\t(X,Y) -> (LAT,LON)"," ");
	zvmessage("\n\twhere"," ");
	sprintf(string,"\n\t(X,Y) = (%7.3f,%7.3f)\n",
		samples[i],lines[i]);
	zvmessage(string," ");

	status = mpxy2ll( mp_obj,lines[i],samples[i],
			&latitude,&longitude,ll_type );

	zvmessage("\n\tTransform completed."," ");
	zvmessage("\n******************************************\n"," ");
	}

zvmessage(" "," ");
zvmessage("***************************************************"," ");
zvmessage("\n\tEnd test of MP routines in C\n"," ");
zvmessage("***************************************************"," ");
zvmessage(" "," ");

mpFree( mp_obj );
}
$!-----------------------------------------------------------------------------
$ create tmp_routines_verbose.imake
#define PROGRAM     tmp_routines_verbose
#define MODULE_LIST tmp_routines_verbose.c

#define MAIN_LANG_C
#define R2LIB 

#define USES_ANSI_C
#define USES_FORTRAN

#define LIB_P1SUB
#define LIB_RTL
#define LIB_SPICE
#define LIB_P2SUB
#define LIB_TAE
#define LIB_FORTRAN
#define LIB_MATH77

#if 0
#define LIB_LOCAL	/* for development, remove on delivery */
#define DEBUG		/* for development, remove on delivery */
#endif

$!-----------------------------------------------------------------------------
$ create tmp_routines_verbose.pdf
process help=*
PARM TARGET		STRING	COUNT=1
PARM PROJ		STRING	COUNT=1
PARM A_AXIS		REAL	COUNT=0:1	DEFAULT=--		
PARM B_AXIS		REAL	COUNT=0:1	DEFAULT=--
PARM C_AXIS		REAL	COUNT=0:1	DEFAULT=--
PARM SCALE		REAL	COUNT=1
PARM RESOLUTION		REAL	COUNT=0:1	DEFAULT=--
PARM POS_LON_DIR	STRING	COUNT=1
PARM CTR_LAT		REAL	COUNT=1
PARM CTR_LON		REAL	COUNT=1
PARM SPHERICAL_AZ	REAL	COUNT=0:1	DEFAULT=0.0
PARM CARTESIAN_AZ	REAL	COUNT=0:1	DEFAULT=0.0
PARM LINE_OFFSET	REAL	COUNT=0:1	DEFAULT=0.0
PARM SAMPLE_OFFSET	REAL	COUNT=0:1	DEFAULT=0.0
PARM PARALLEL_ONE	REAL	COUNT=0:1	DEFAULT=--
PARM PARALLEL_TWO	REAL	COUNT=0:1	DEFAULT=--
PARM LONG_AXIS		REAL	COUNT=0:1	DEFAULT=--
PARM PCK_PATH		STRING 	COUNT=1
PARM LL_TYPE		STRING  COUNT=1 	+
  VALID=("PLANETODETIC", "PLANETOCENTRIC", "PLANETOGRAPHIC", "SNYDER_DEFINED")
PARM LATITUDES		REAL	COUNT=1:10
PARM LONGITUDES		REAL	COUNT=1:10
end-proc
.TITLE
VICAR program TMP_ROUTINES
.HELP
PURPOSE:
This program is a simple test program for the HWCARTO
subroutine. It calls zhwgetpar, zhwcarto and mpFree and
prints the contents of the map projection object after zhwgetpar 
is called and the transformation results. MP routines are
embedded in the calls of zhwgetpar and zhwcarto.
.LEVEL1

.VARI TARGET
Target body of object for which map projection points will
be transformed.

.VARI PROJ
Map projection type requested.

.VARI A_AXIS
Semimajor axis of target body.

.VARI B_AXIS
Semiminor axis of target body.

.VARI C_AXIS
Polar axis of target body.

.VARI SCALE
Map scale.

.VARI RESOLUTION
Map resolution.

.VARI POS_LON_DIR
Positive longitude direction.

.VARI CTR_LAT
Center latitude

.VARI CTR_LON
Center longitude

.VARI SPHERICAL_AZ
Spherical azimuth

.VARI CARTESIAN_AZ
Cartesian azimuth

.VARI LINE_OFFSET
Line projection offset

.VARI SAMPLE_OFFSET
Sample projection offset

.VARI PARALLEL_ONE
First standard parallel

.VARI PARALLEL_TWO
Second standard paralel

.VARI LONG_AXIS
Body long axis meridian offset longitude

.VARI LAT_LON_TYPE
Type of latitude and longitude as input or to be returned.
Valid types are 'PLANETOCENTRIC', 'PLANETODETIC', and 'SNYDER-DEFINED'.

.VARI LATITUDES
Array of planetocentric, planetodetic or Snyder-defined latitudes on
a target body.

.VARI LONGITUDES
Array of planetOcentric, planetodetic or Snyder-defined longitudes on
a target body. NOTE THAT LONGITUDES ARE PLANETODETIC, PLANETOCENTRIC
OR SNYDER-DEFINED ONLY WITH THE TRIAXIAL ELLIPSOID MODEL.

.END
$!-----------------------------------------------------------------------------
$ create tmp_routines_f.f
	include 'VICMAIN_FOR'

C**********************************************************************
C
C	Test Program TMP_ROUTINES
C
C	Program calls mpGetPar which calls mpInit to allocate memory 
C	for a map projection data object and then sets values in the	
C	data object based on values passed by the application programs
C	parameter list. Then mpll2xy and mpxy2ll are called to perform
C	point transformations.
C
C	Author:			Justin McNeill
C	Cognizant Engineer:	Justin McNeill
C	Date Written:		May 1994	
C	Revision history:	Original
C	
C**********************************************************************

	subroutine main44
	implicit integer (a-y)
	implicit real*8  (z)
	
	include 'mp_for_defs'

	integer i
	integer count
	integer status
	integer number_keywords
    	integer types(mp_number_of_keywords)
	integer classes(mp_number_of_keywords)
	integer ll_type
	integer lat_count, lon_count

	real*8 mp

	character*20 lat_lon_type
	character*100 msg

	real*8 double_value
	real*8 lines(10)
	real*8 samples(10)
	real*4 latitudes(10)
	real*4 longitudes(10)

	real*8 latitude
	real*8 longitude
	real*8 new_lat
	real*8 new_lon

	character*(mp_max_keywd_length) keys(mp_number_of_keywords)
	character*(mp_max_keywd_length) pdf_parms(mp_number_of_keywords)
	character*(mp_max_keywd_length) pds_keys(mp_number_of_keywords)

	character*200 pck_file
	
	character*40 string_value

	call xvmessage(' ',' ')
	call xvmessage('*********************************************'
     .		,' ')
	call xvmessage(' ',' ')
	call xvmessage('     FORTRAN Test of MP Routines',' ')
	call xvmessage(' ',' ')
	call xvmessage('*********************************************'
     .		,' ')
	call xvmessage(' ',' ')

C
C	Set PDF parms to PDS keywords mappings
C	

	pdf_parms(1) = 'TARGET'
	pds_keys(1) = 'TARGET_NAME'

	pdf_parms(2) = 'PROJ'
	pds_keys(2) = 'MAP_PROJECTION_TYPE'

	pdf_parms(3) = 'A_AXIS'
	pds_keys(3) = 'A_AXIS_RADIUS'

	pdf_parms(4) = 'B_AXIS'
	pds_keys(4) = 'B_AXIS_RADIUS'

	pdf_parms(5) = 'C_AXIS'
	pds_keys(5) = 'C_AXIS_RADIUS'

	pdf_parms(6) = 'SCALE'
	pds_keys(6) = 'MAP_SCALE'

	pdf_parms(7) = 'RESOLUTION'
	pds_keys(7) = 'MAP_RESOLUTION'

	pdf_parms(8) = 'POS_LON_DIR'
	pds_keys(8) = 'POSITIVE_LONGITUDE_DIRECTION'

	pdf_parms(9) = 'CTR_LAT'
	pds_keys(9) = 'CENTER_LATITUDE'

	pdf_parms(10) = 'CTR_LON'
	pds_keys(10) = 'CENTER_LONGITUDE'

	pdf_parms(11) = 'SPHERICAL_AZ'
	pds_keys(11) = 'SPHERICAL_AZIMUTH'

	pdf_parms(12) = 'CARTESIAN_AZ'
	pds_keys(12) = 'CARTESIAN_AZIMUTH'

	pdf_parms(13) = 'LINE_OFFSET'
	pds_keys(13) = 'LINE_PROJECTION_OFFSET'

	pdf_parms(14) = 'SAMPLE_OFFSET'
	pds_keys(14) = 'SAMPLE_PROJECTION_OFFSET'

	pdf_parms(15) = 'PARALLEL_ONE'
	pds_keys(15) = 'FIRST_STANDARD_PARALLEL'

	pdf_parms(16) = 'PARALLEL_TWO'
	pds_keys(16) = 'SECOND_STANDARD_PARALLEL'

	pdf_parms(17) = 'XYZ'
	pds_keys(17) = 'FOCAL_LENGTH'

	pdf_parms(18) = 'TGT_DIST'
	pds_keys(18) = 'TARGET_CENTER_DISTANCE'

	pdf_parms(19) = 'AXIS_LINE'
	pds_keys(19) = 'OPT_AXIS_INTERCEPT_LINE'

	pdf_parms(20) = 'AXIS_SAMPLE'
	pds_keys(20) = 'OPT_AXIS_INTERCEPT_SAMPLE'

	pdf_parms(21) = 'FOCAL_SCALE'
	pds_keys(21) = 'FOCAL_PLANE_SCALE'

	pdf_parms(22) = 'SUB_SPACE_LAT'
	pds_keys(22) = 'SUB_SPACECRAFT_LATITUDE'

	pdf_parms(23) = 'SUB_SPACE_LON'
	pds_keys(23) = 'SUB_SPACECRAFT_LONGITUDE'

	pdf_parms(24) = 'CENT_LINE'
	pds_keys(24) = 'PLANET_CENTER_LINE'

	pdf_parms(25) = 'CENT_SAMPLE'
	pds_keys(25) = 'PLANET_CENTER_SAMPLE'

	pdf_parms(26) = 'N_ANGLE'
	pds_keys(26) = 'NORTH_ANGLE'

	pdf_parms(27) = 'LONG_AXIS'
	pds_keys(27) = 'BODY_LONG_AXIS'

	pdf_parms(28) = 'LL_TYPE'
	pds_keys(28) = 'COORDINATE_SYSTEM_NAME'

	! test acceptance of old keword:
	pdf_parms(29) = 'TGTBOD'
	pds_keys(29) = 'TARGET_BODY'

	pdf_parms(30) = '\0'
	pds_keys(30) = '\0'

C
C	Retrieve NAIF planetary constants kernel path name
C

	call xvp('PCK_PATH',pck_file,count)

C
C	Set up MP data object	
C

	call xvfilename( pck_file, pck_file, status )
	if ( status.lt.0 ) call abend

	call mp_get_par( mp,pdf_parms,pds_keys,pck_file )

	flag = 0

	call mp_set_debug_flag( flag,status )
	if ( status.lt.mp_success ) call abend

C
C	Verify keywords set
C

	call mp_get_keywords( mp,keys,number_keywords,types,classes,
     .		status )
	if ( status.lt.mp_success ) then
  		call xvmessage( 'Error in mp_get_keywords call',' ')
		call xvmessage( 'Test failed', ' ')
		call abend
	endif

C
C	Retrieve and print MP data object values
C

	do i=1,number_keywords
		
	  if( types(i).eq.mp_char ) then
	  	
	     call mp_get_value_str( mp,keys(i),string_value,status )
	     if ( status.lt.mp_success ) then
		call abend 
	     endif

	     write( msg, 1000 ) keys(i), string_value
	     call xvmessage( msg, ' ' )

	  else 
           
           if( types(i).eq.mp_dble ) then

	     call mp_get_value( mp,keys(i),double_value,status )
	     if ( status.lt.mp_success ) then
		call abend 
	     endif

	     write( msg, 1010 ) keys(i), double_value
	     call xvmessage( msg, ' ' )

	   else

	     call xvmessage( 'PDS KEY of unacceptable data type', ' ' )

 	   endif

	  endif	     
	
	enddo
		
C
C	Determine input latitude and longitude type specified as input
C

	call xvp('LL_TYPE',lat_lon_type,count)

	if ( lat_lon_type.eq.'PLANETOCENTRIC' ) ll_type = 1
	if ( lat_lon_type.eq.'PLANETOGRAPHIC' .or.
     &       lat_lon_type.eq.'PLANETODETIC' ) ll_type = 2

	if ( lat_lon_type.eq.'PLANETOGRAPHIC' ) ll_type = 2
	if ( lat_lon_type.eq.'SNYDER_DEFINED' ) ll_type = 3

C
C	Retrieve latitudes and longitudes from the PDS file
C

	call xvp('LATITUDES',latitudes,lat_count)
	call xvp('LONGITUDES',longitudes,lon_count)
	if ( lat_count.lt.lon_count ) then
		count = lat_count
	else
		count = lon_count
	endif

C
C       Perform the forward transformation
C
	call xvmessage(' ',' ')
	call xvmessage('     Table of Point Transformations',' ')
	call xvmessage(' ',' ')
	call xvmessage(' (Lat,Lon) -> (Line,Sample) -> (Lat",Lon")',
     .  		' ')
	call xvmessage(' ',' ')

	do i=1,count
	
	  latitude = latitudes(i)
	  longitude = longitudes(i)

	  call mp_ll2xy(mp,lines(i),samples(i),latitude,
     .   	longitude,ll_type,status)
	  if( status.lt.mp_success ) call abend

	  call mp_xy2ll(mp,lines(i),samples(i),new_lat,
     .   	new_lon,ll_type,status)
	  if( status.lt.mp_success ) call abend

	  write ( msg, 1020 ) latitude,longitude,
     .		lines(i),samples(i),new_lat,new_lon
	  call xvmessage( msg,' ' )

	enddo
	
	call xvmessage(' ',' ')
	call xvmessage('*********************************************'
     .		,' ')
	call xvmessage(' ',' ')
	call xvmessage('     End FORTRAN test of MP routines', ' ')
	call xvmessage(' ',' ')
	call xvmessage('*********************************************'
     .		,' ')
	call xvmessage(' ',' ')

 1000   format( '     ',a, ' = ', a)
 1010	format( '     ',a, ' = ', d10.3)
 1020   format (1X,'(',F8.3,',',F8.3,') -> (',F8.3,',',F8.3,') -> (',
     .          F8.3,',',F8.3,') ')
	end
$!-----------------------------------------------------------------------------
$ create tmp_routines_f.imake
#define  PROGRAM   tmp_routines_f

#define MODULE_LIST tmp_routines_f.f

#define MAIN_LANG_FORTRAN
#define R2LIB 

#define FTNINC_LIST mp_for_defs

#define USES_FORTRAN

#define LIB_P1SUB
#define LIB_RTL
#define LIB_SPICE
#define LIB_TAE
#define LIB_P2SUB
#define LIB_FORTRAN
#define LIB_MATH77

#if 0
#define LIB_LOCAL	/* for development, remove on delivery */
#define DEBUG		/* for development, remove on delivery */
#endif

$!-----------------------------------------------------------------------------
$ create tmp_routines_f.pdf
process help=*
PARM TARGET		STRING	COUNT=0:1       DEFAULT=--
PARM PROJ		STRING	COUNT=1
PARM A_AXIS		REAL	COUNT=0:1	DEFAULT=--		
PARM B_AXIS		REAL	COUNT=0:1	DEFAULT=--
PARM C_AXIS		REAL	COUNT=0:1	DEFAULT=--
PARM SCALE		REAL	COUNT=0:1	DEFAULT=--
PARM RESOLUTION		REAL	COUNT=0:1	DEFAULT=--
PARM POS_LON_DIR	STRING	COUNT=1
PARM CTR_LAT		REAL	COUNT=1
PARM CTR_LON		REAL	COUNT=1
PARM SPHERICAL_AZ	REAL	COUNT=0:1	DEFAULT=0.0
PARM CARTESIAN_AZ	REAL	COUNT=0:1	DEFAULT=0.0
PARM LINE_OFFSET	REAL	COUNT=0:1	DEFAULT=0.0
PARM SAMPLE_OFFSET	REAL	COUNT=0:1	DEFAULT=0.0
PARM PARALLEL_ONE	REAL	COUNT=0:1	DEFAULT=--
PARM PARALLEL_TWO	REAL	COUNT=0:1	DEFAULT=--
PARM LONG_AXIS		REAL	COUNT=0:1	DEFAULT=--
PARM TGTBOD		STRING 	COUNT=0:1	DEFAULT=--
PARM PCK_PATH		STRING 	COUNT=1
PARM LL_TYPE		STRING  COUNT=1         
PARM LATITUDES		REAL	COUNT=0:10
PARM LONGITUDES		REAL	COUNT=0:10
PARM LINES		REAL	COUNT=0:10
PARM SAMPLES		REAL	COUNT=0:10
PARM TGT_DIST		REAL	COUNT=0:1	DEFAULT=--		
PARM SC_DIST		REAL	COUNT=0:1	DEFAULT=--		
PARM XYZ		REAL	COUNT=0:1	DEFAULT=--
PARM AXIS_LINE		REAL	COUNT=0:1	DEFAULT=--
PARM AXIS_SAMPLE	REAL	COUNT=0:1	DEFAULT=--
PARM FOCAL_SCALE	REAL	COUNT=0:1	DEFAULT=--
PARM SUB_SPACE_LAT	REAL	COUNT=0:1	DEFAULT=--
PARM SUB_SPACE_LON	REAL	COUNT=0:1	DEFAULT=--
PARM CENT_LINE		REAL	COUNT=0:1	DEFAULT=--
PARM CENT_SAMPLE	REAL	COUNT=0:1	DEFAULT=--
PARM N_ANGLE		REAL	COUNT=0:1	DEFAULT=--
end-proc
.TITLE
VICAR program TMP_ROUTINES
.HELP
PURPOSE:
This program is a simple test program for the HWCARTO
subroutine. It calls zhwgetpar, zhwcarto and mpFree and
prints the contents of the map projection object after zhwgetpar 
is called and the transformation results. MP routines are
embedded in the calls of zhwgetpar and zhwcarto.
.LEVEL1

.VARI TARGET
Target body of object for which map projection points will
be transformed.

.VARI PROJ
Map projection type requested.

.VARI A_AXIS
Semimajor axis of target body.

.VARI B_AXIS
Semiminor axis of target body.

.VARI C_AXIS
Polar axis of target body.

.VARI SCALE
Map scale.

.VARI RESOLUTION
Map resolution.

.VARI POS_LON_DIR
Positive longitude direction.

.VARI CTR_LAT
Center latitude

.VARI CTR_LON
Center longitude

.VARI SPHERICAL_AZ
Spherical azimuth

.VARI CARTESIAN_AZ
Cartesian azimuth

.VARI LINE_OFFSET
Line projection offset

.VARI SAMPLE_OFFSET
Sample projection offset

.VARI PARALLEL_ONE
First standard parallel

.VARI PARALLEL_TWO
Second standard paralel

.VARI LONG_AXIS
Body long axis offset longitude

.VARI LAT_LON_TYPE
Type of latitude and longitude as input or to be returned.
Valid types are 'PLANETOCENTRIC', 'PLANETODETIC', and 'SNYDER-DEFINED'.

.VARI LATITUDES
Array of planetocentric, planetodetic or Snyder-defined latitudes on
a target body.

.VARI LONGITUDES
Array of planetOcentric, planetodetic or Snyder-defined longitudes on
a target body. NOTE THAT LONGITUDES ARE PLANETODETIC, PLANETOCENTRIC
OR SNYDER-DEFINED ONLY WITH THE TRIAXIAL ELLIPSOID MODEL.

.END
$!-----------------------------------------------------------------------------
$ create tmp_label.imake
/***********************

IMAKE file for TMP_LABEL

************************/

#define PROGRAM	tmp_label
#define MODULE_LIST tmp_label.c

#define MAIN_LANG_C
#define R2LIB

#define USES_ANSI_C
#define USES_FORTRAN

#define LIB_P1SUB
#define LIB_RTL
#define LIB_SPICE
#define LIB_P2SUB
#define LIB_TAE
#define LIB_FORTRAN
#define LIB_MATH77

#if 0
#define LIB_LOCAL	/* for development, remove on delivery */
#define DEBUG		/* for development, remove on delivery */
#endif

$!-----------------------------------------------------------------------------
$ create tmp_label.pdf
PROCESS HELP=*
PARM INP     TYPE=STRING
PARM OUT     TYPE=STRING
END-PROC
$!-----------------------------------------------------------------------------
$ create tmp_label.c

#include <stdio.h>		/* Standard C I/O Include File          */
#include <math.h>		/* Map Projection Include File  */
#include "vicmain_c"
#include "mp_routines.h"

/****************************************

TEST PROGRAM TMP_LABEL.C for MP Routines

(taken from program by E. Hauber of DLR)

****************************************/

void main44()
{
	int	status,count;
	int	inunit,outunit;
	int	inlines,insampl;
	int	sll,i,num;
	int	buf_step;
	int	read;
	int	type[mpNUMBER_OF_KEYWORDS], class[mpNUMBER_OF_KEYWORDS];

	unsigned char	buffer[10000];
	char		keylist[mpNUMBER_OF_KEYWORDS][mpMAX_KEYWD_LENGTH+1];
	char		cval[133],msg[133];
	double		dval,zval;

	MP mp_obj;			/* Declaration of MP pointer */

/*************************************************************
	Assign a unit number to the input file:
*************************************************************/
	status = zveaction( "AU", "System error" );

	status = zvunit(&inunit,"INP",1,0);

/****************************
	Open the input file :
****************************/

	status = zvopen(inunit,"OP","READ","U_FORMAT","BYTE",0);
	zvmessage(" ",0);	

/********************************************************************
	Retrieve the number of lines and samples in the input file :
********************************************************************/

	status = zvget(inunit,"nl",&inlines,"ns",&insampl,0);

/******************************************************************
	Map Label processing begins: "mpInit"
******************************************************************/

	status = mpInit(&mp_obj);	 /* Create MP data object */
	ABENDif( status<mpSUCCESS );

	zvmessage("Mp object has been opened",0);

/*********************************************
	MP label is being read
*********************************************/

	status = mpLabelRead(mp_obj,inunit);
	ABENDif( status<mpSUCCESS );

	zvmessage("Old map label has been read.",0);

/****************************************************************************
	Now we check the mp_obj:
	- We use the routine "mpGetKeywords" to derive all the keywords from
	the 'mp_obj', ie. the number of keywords is returned into "num"
	and the names of the keywords are returned into "keylist".
	- We use the routine "mpGetValues" to get the values of the keywords
	found in the 'mp_obj' with "mpGetKeywords".
****************************************************************************/

	status = mpGetKeywords( mp_obj, keylist, &num, type, class);
	ABENDif( status<mpSUCCESS );


	for (i=0; i<num; i++) {

		if (type[i] == mpCHAR) {
		status = mpGetValues( mp_obj, keylist[i], cval, "");
		strcpy( msg, keylist[i]);
		strcat( msg, " = ");
		strcat( msg, cval);
		zvmessage( msg, "");

		}
		else if (type[i] == mpDBLE) {

		status = mpGetValues( mp_obj, keylist[i], &dval, "");
		strcpy( msg, keylist[i]);
		strcat( msg, " = ");
		sprintf( cval, " %10.3e", dval);
		strcat( msg, cval);
		zvmessage( msg, "");
		}
	}

	status=zvunit(&outunit,"OUT",1,0);
	status=zvopen(outunit,"OP","WRITE","OPEN_ACT","SA","U_FORMAT",
	"BYTE","O_FORMAT","BYTE","U_NL",inlines,"U_NS",insampl,0);

	status = mpLabelWrite( mp_obj, outunit, "HISTORY");
	ABENDif( status<mpSUCCESS );

	status = mpLabelWrite( mp_obj, outunit, "PROPERTY");
	ABENDif( status<mpSUCCESS );


/********************************************************
	-- Loop through all image lines
	-- All lines are read in from the input image
	-- All lines are written out to the output image
********************************************************/

	for( i=1; i <= inlines; i++)
		{
			sll=1+i-1;
			buf_step = (i-1)* insampl;

			status = zvread(inunit,buffer,"samp",1,
				      "line",sll,"nsamps",insampl,0);

			if (status == 0)
			{
				free(buffer);
				break;
			}

			status = zvwrit(outunit,buffer,"samp",1,
				      "nsamps",insampl,0);

			if (status == 0)
			{
				free(buffer);
				break;
			}

		}

	status = zvclose(outunit,0);
	zvmessage("Output image has been closed",0);


/*********************************************
	MP object is free again
*********************************************/

	status = mpFree(mp_obj);
	zvmessage("Mp object has been closed",0);

/*********************************************
	Images are closed after processing
*********************************************/

	status = zvclose(inunit,0);
	zvmessage("Input image has been closed",0);

}
$!-----------------------------------------------------------------------------
$ create tmp_forward.c
#include <stdio.h>
#include "vicmain_c"
#include <math.h>
#include "mp_routines.h"

#define MAX_LATS_LONS 	10

/**********************************************************************
 
Test Program TMP_FORWARD

Program calls mpGetPar which calls mpInit to allocate memory 
for a map projection data object and then sets values in the
data object based on values passed by the application programs
parameter list. Then mpll2xy is called.

Author:			Justin McNeill
Cognizant Engineer:	Justin McNeill
Date Written:		May 1994
Revision history:	Original

*/
void main44()
{
int 	i,j,k;
int	count;
int	status;
int	mode;
int	indices[2],lengthes[2];
int	number_keywords;
int	types[mpNUMBER_OF_KEYWORDS],classes[mpNUMBER_OF_KEYWORDS];
int	ll_type;
int	lat_count,lon_count;
char 	lat_lon_type[20];

double	double_value;
double	lines[MAX_LATS_LONS];
double	samples[MAX_LATS_LONS];
float	latitudes[MAX_LATS_LONS];
float	longitudes[MAX_LATS_LONS];
double	latitude;
double	longitude;

char	keys[mpNUMBER_OF_KEYWORDS][mpMAX_KEYWD_LENGTH+1];
char	pdf_parms[mpNUMBER_OF_KEYWORDS][mpMAX_PARM_LENGTH+1];
char	pds_keys[mpNUMBER_OF_KEYWORDS][mpMAX_KEYWD_LENGTH+1];
char    PCKfile[200];
char	PCKpath[200];
char	string[300];
char	string_value[200];

MP mp_obj;

zvmessage("***************************************************"," ");
zvmessage("\n\tTest of MP Routines\n"," ");
zvmessage("***************************************************\n"," ");

/*

Define user parameters to be retrieved from PDF file.

*/

strcpy(pdf_parms[0],"TARGET");
strcpy(pds_keys[0],mpTARGET_NAME);

strcpy(pdf_parms[1],"PROJ");
strcpy(pds_keys[1],mpMAP_PROJECTION_TYPE);

strcpy(pdf_parms[2],"A_AXIS");
strcpy(pds_keys[2],mpA_AXIS_RADIUS);

strcpy(pdf_parms[3],"B_AXIS");
strcpy(pds_keys[3],mpB_AXIS_RADIUS);

strcpy(pdf_parms[4],"C_AXIS");
strcpy(pds_keys[4],mpC_AXIS_RADIUS);

strcpy(pdf_parms[5],"SCALE");
strcpy(pds_keys[5],mpMAP_SCALE);

strcpy(pdf_parms[6],"RESOLUTION");
strcpy(pds_keys[6],mpMAP_RESOLUTION);

strcpy(pdf_parms[7],"POS_LON_DIR");
strcpy(pds_keys[7],mpPOSITIVE_LONGITUDE_DIRECTION);

strcpy(pdf_parms[8],"CTR_LAT");
strcpy(pds_keys[8],mpCENTER_LATITUDE);

strcpy(pdf_parms[9],"CTR_LON");
strcpy(pds_keys[9],mpCENTER_LONGITUDE);

strcpy(pdf_parms[10],"SPHERICAL_AZ");
strcpy(pds_keys[10],mpSPHERICAL_AZIMUTH);

strcpy(pdf_parms[11],"CARTESIAN_AZ");
strcpy(pds_keys[11],mpCARTESIAN_AZIMUTH);

strcpy(pdf_parms[12],"LINE_OFFSET");
strcpy(pds_keys[12],mpLINE_PROJECTION_OFFSET);

strcpy(pdf_parms[13],"SAMPLE_OFFSET");
strcpy(pds_keys[13],mpSAMPLE_PROJECTION_OFFSET);

strcpy(pdf_parms[14],"PARALLEL_ONE");
strcpy(pds_keys[14],mpFIRST_STANDARD_PARALLEL);

strcpy(pdf_parms[15],"PARALLEL_TWO");
strcpy(pds_keys[15],mpSECOND_STANDARD_PARALLEL);

strcpy(pdf_parms[16],"XYZ"); 
strcpy(pds_keys[16],mpFOCAL_LENGTH); 

strcpy(pdf_parms[17],"RANGE");
strcpy(pds_keys[17],mpTARGET_CENTER_DISTANCE);

strcpy(pdf_parms[18],"AXIS_LINE");
strcpy(pds_keys[18],mpOPT_AXIS_INTERCEPT_LINE);

strcpy(pdf_parms[19],"AXIS_SAMPLE");
strcpy(pds_keys[19],mpOPT_AXIS_INTERCEPT_SAMPLE);

strcpy(pdf_parms[20],"FOCAL_SCALE");
strcpy(pds_keys[20],mpFOCAL_PLANE_SCALE);

strcpy(pdf_parms[21],"SUB_SPACE_LAT");
strcpy(pds_keys[21],mpSUB_SPACECRAFT_LATITUDE);

strcpy(pdf_parms[22],"SUB_SPACE_LON");
strcpy(pds_keys[22],mpSUB_SPACECRAFT_LONGITUDE);

strcpy(pdf_parms[23],"CENT_LINE");
strcpy(pds_keys[23],mpPLANET_CENTER_LINE);

strcpy(pdf_parms[24],"CENT_SAMPLE");
strcpy(pds_keys[24],mpPLANET_CENTER_SAMPLE);

strcpy(pdf_parms[25],"N_ANGLE");
strcpy(pds_keys[25],mpNORTH_ANGLE);

strcpy(pdf_parms[26],"LONG_AXIS");
strcpy(pds_keys[26],mpBODY_LONG_AXIS);

strcpy(pdf_parms[27],"LL_TYPE");
strcpy(pds_keys[27],mpCOORDINATE_SYSTEM_NAME);

pdf_parms[28][0] = '\0';
pds_keys[28][0] = '\0';

/*

Set user parameters for subsequent input to mpLL2XY or mpXY2LL

*/
status = zvp("PCK_PATH",PCKpath,&count);
if ( status < 0 )
	{
	zvmessage("SPICE PCK file pathname not found."," ");
	zvmessage("Pathname set to blank"," ");
	strcpy(PCKpath," ");
	}

zvsptr(PCKpath,count,indices,lengthes);
indices[0] -= 1;
strncpy(PCKfile,&PCKpath[indices[0]],lengthes[0]);
PCKfile[lengthes[0]]='\0';

status = zvfilename( PCKfile, PCKpath, 0 );
ABENDif( status < VICARrtlSUCCESS );

status = mpGetPar( &mp_obj,pdf_parms,pds_keys,PCKpath );
if ( status < 0 )
	{
	zvmessage("Error in mpgetpar call"," ");
	zvmessage("Test failed."," ");
	zabend();
	}

status = mpSetDebugFlag( TRUE );
ABENDif( status<mpSUCCESS );

status = mpGetKeywords( mp_obj,keys,&number_keywords,types,classes );
if ( status < 0 )
	{
	zvmessage("Error in mpGetKeywords call"," ");
	zvmessage("Test failed."," ");
	zabend();
	}

for ( i=0; i<number_keywords; i++ )
	switch ( types[i] )	{

	case mpCHAR:

		status = mpGetValues( mp_obj,keys[i],string_value,"" );
		ABENDif( status < mpSUCCESS );
		
		sprintf(string,"KEYWORD %s equals %s",keys[i],string_value);
		zvmessage(string," ");
		
		break;

	case mpDBLE:

		status = mpGetValues( mp_obj,keys[i],&double_value,"" );
		ABENDif( status < mpSUCCESS );
		
		sprintf(string,"KEYWORD %s equals %4.3e",keys[i],double_value);
		zvmessage(string," ");

		break;

	default:

		zvmessage("PDS KEY of unacceptable data type"," ");
		break;	}

/*

Print output banner for map transformation

*/

zvmessage(" "," ");
zvmessage("***************************************************"," ");
zvmessage("\n\tTransformation results:"," ");
zvmessage(" "," ");

/*

Determine input latitude and longitude type specified as input.

*/

status = zvp("LL_TYPE",lat_lon_type,&count);
ABENDif( status < mpSUCCESS );

if ( strcmp(lat_lon_type,"PLANETOCENTRIC") == 0 )
	{
	ll_type = 1;
	zvmessage("\n\tPlanetocentric lat/lon pairs\n"," ");
	}
if ( strcmp(lat_lon_type,"PLANETOGRAPHIC") == 0 )
	{
	ll_type = 2;
	zvmessage("\n\tPlanetographic lat/lon pairs\n"," ");
	}
if ( strcmp(lat_lon_type,"SNYDER_DEFINED") == 0 )
	{
	ll_type = 3;
	zvmessage("\n\tSnyder-defined lat/lon pairs\n"," ");
	}
/*

Get latitude and longitude array from parameter values.

*/

status = zvp("LATITUDES",latitudes,&lat_count);
ABENDif( status < mpSUCCESS );

status = zvp("LONGITUDES",longitudes,&lon_count);
ABENDif( status < mpSUCCESS );

if ( lat_count < lon_count )
	count = lat_count;
else
	count = lon_count;

/*

FORWARD TRANSFORMATION

*/

for( i=0; i<count; i++ )
	{
	latitude = (double) latitudes[i];
	longitude = (double) longitudes[i];

	zvmessage("\n******************************************\n"," ");
	zvmessage("\t(LAT,LON) -> (X,Y)"," ");
	zvmessage("\n\twhere"," ");
	sprintf(string,"\n\t(LAT,LON) = (%5.3f,%6.3f)\n",
		latitude,longitude);
	zvmessage(string," ");

	status = mpll2xy( mp_obj,&lines[i],&samples[i],
			latitude,longitude,ll_type );

	zvmessage("\n\tTransform completed."," ");
	zvmessage("\n******************************************\n"," ");
	}

zvmessage(" "," ");
zvmessage("***************************************************"," ");
zvmessage("\n\tEnd test of MP Routines\n"," ");
zvmessage("***************************************************"," ");
zvmessage(" "," ");

mpFree( mp_obj );
}
$!-----------------------------------------------------------------------------
$ create tmp_forward.imake
#define  PROGRAM   tmp_forward

#define MODULE_LIST tmp_forward.c

#define MAIN_LANG_C
#define R2LIB 

#define USES_ANSI_C
#define USES_FORTRAN

#define LIB_P1SUB
#define LIB_RTL
#define LIB_SPICE
#define LIB_TAE
#define LIB_P2SUB
#define LIB_FORTRAN
#define LIB_MATH77

#if 0
#define LIB_LOCAL	/* for development, remove on delivery */
#define DEBUG		/* for development, remove on delivery */
#endif

$!-----------------------------------------------------------------------------
$ create tmp_forward.pdf
process help=*
PARM TARGET		STRING	COUNT=1
PARM PROJ		STRING	COUNT=1
PARM A_AXIS		REAL	COUNT=0:1	DEFAULT=--		
PARM B_AXIS		REAL	COUNT=0:1	DEFAULT=--
PARM C_AXIS		REAL	COUNT=0:1	DEFAULT=--
PARM SCALE		REAL	COUNT=1
PARM RESOLUTION		REAL	COUNT=0:1	DEFAULT=--
PARM POS_LON_DIR	STRING	COUNT=1
PARM CTR_LAT		REAL	COUNT=1
PARM CTR_LON		REAL	COUNT=1
PARM SPHERICAL_AZ	REAL	COUNT=0:1	DEFAULT=0.0
PARM CARTESIAN_AZ	REAL	COUNT=0:1	DEFAULT=0.0
PARM LINE_OFFSET	REAL	COUNT=0:1	DEFAULT=0.0
PARM SAMPLE_OFFSET	REAL	COUNT=0:1	DEFAULT=0.0
PARM PARALLEL_ONE	REAL	COUNT=0:1	DEFAULT=--
PARM PARALLEL_TWO	REAL	COUNT=0:1	DEFAULT=--
PARM LONG_AXIS		REAL	COUNT=0:1	DEFAULT=--
PARM PCK_PATH		STRING 	COUNT=1
PARM LL_TYPE		STRING  COUNT=1 +
	VALID=("PLANETODETIC","PLANETOCENTRIC","SNYDER_DEFINED")
PARM LATITUDES		REAL	COUNT=1:10
PARM LONGITUDES		REAL	COUNT=1:10
parm debug integer valid=(0,1) default=1
end-proc
.TITLE
VICAR program TMP_ROUTINES
.HELP
PURPOSE:
This program is a simple test program for the HWCARTO
subroutine. It calls zhwgetpar, zhwcarto and mpFree and
prints the contents of the map projection object after zhwgetpar 
is called and the transformation results. MP routines are
embedded in the calls of zhwgetpar and zhwcarto.
.LEVEL1

.VARI TARGET
Target body of object for which map projection points will
be transformed.

.VARI PROJ
Map projection type requested.

.VARI A_AXIS
Semimajor axis of target body.

.VARI B_AXIS
Semiminor axis of target body.

.VARI C_AXIS
Polar axis of target body.

.VARI SCALE
Map scale.

.VARI RESOLUTION
Map resolution.

.VARI POS_LON_DIR
Positive longitude direction.

.VARI CTR_LAT
Center latitude

.VARI CTR_LON
Center longitude

.VARI SPHERICAL_AZ
Spherical azimuth

.VARI CARTESIAN_AZ
Cartesian azimuth

.VARI LINE_OFFSET
Line projection offset

.VARI SAMPLE_OFFSET
Sample projection offset

.VARI PARALLEL_ONE
First standard parallel

.VARI PARALLEL_TWO
Second standard paralel

.VARI LONG_AXIS
Body long axis meridian offset longitude

.VARI LAT_LON_TYPE
Type of latitude and longitude as input or to be returned.
Valid types are 'PLANETOCENTRIC', 'PLANETODETIC', and 'SNYDER-DEFINED'.

.VARI LATITUDES
Array of planetocentric, planetodetic or Snyder-defined latitudes on
a target body.

.VARI LONGITUDES
Array of planetocentric, planetodetic or Snyder-defined longitudes on
a target body. NOTE THAT LONGITUDES ARE PLANETODETIC, PLANETOCENTRIC
OR SNYDER-DEFINED ONLY WITH THE TRIAXIAL ELLIPSOID MODEL.

.END
$!-----------------------------------------------------------------------------
$ create tmp_mpo2buf.c
#include <math.h>
#include "mp_routines.h"
#include "vicmain_c"

/* 

Test Program TMP_MPO2BUF

Author		Lucas Kamp
Date		March 1995

Purpose

Test interface to OM matrix computation for POINT_PERSPECTIVE images.
MP routines called are mpInit, mpLabelRead, mpMpo2Buf, and mpFree,
in that order.
		
*/

void main44()
{
  int 	i,j,k;
  int	status, unit1;
  double buf[20];
  char msg[200];
  MP mp_obj;

  zveaction("","");

  status = mpInit( &mp_obj );

  zvpcnt( "INP", &i );

  if ( i > 0 ) {
    status = zvunit( &unit1, "INP", 1, 0);
    status = zvopen( unit1, "OP", "READ", 0);
    ABENDif( status!=1);
    status = mpLabelRead( mp_obj, unit1);
    ABENDif( status!=mpSUCCESS);
    zvclose( unit1, 0);
  }

  /*

  Translate MP data object to 40-word buffer and print the computed
  OM matrix

  */

  status = mpMpo2Buf( mp_obj, buf );
  ABENDif( status<mpSUCCESS);

  zvmessage( "\n***\n*** Computed OM-matrix:\n***","" );

  for (i=0; i<9; i++) {
    sprintf( msg,"\tOM[%d] = %8.4f", i, buf[i]);
    zvmessage( msg, "");
  }

  mpFree( mp_obj );
}
$!-----------------------------------------------------------------------------
$ create tmp_mpo2buf.imake
#define PROGRAM tmp_mpo2buf

#define MODULE_LIST tmp_mpo2buf.c

#define MAIN_LANG_C
#define USES_ANSI_C
#define USES_FORTRAN

#define R2LIB
#define LIB_P1SUB
#define LIB_MATH77
#define LIB_RTL
#define LIB_SPICE
#define LIB_P2SUB
#define LIB_TAE
#define LIB_FORTRAN

#if 0
#define LIB_LOCAL	/* for development, remove on delivery */
#define DEBUG		/* for development, remove on delivery */
#endif

$!-----------------------------------------------------------------------------
$ create tmp_mpo2buf.pdf
PROCESS HELP=*

parm INP 	string 	count=0:1 default=--
parm LATLON 	real 	count=2

END-PROC

.TITLE
VICAR Program TMP_MPO2BUF
.HELP

 Test of mpMpo2Buf routine in mp_routines.com.
.END
$!-----------------------------------------------------------------------------
$ create tmp_buf2mpo.c
/* TMP_BUF2MPO:
 *
 * use code from tmp_routines.c to construct an MP object, then
 * call mpmpo2buf, then mpbuf2mpo
 *
 *  17dec99  -lwk-
 */

#include <stdio.h>
#include "vicmain_c"
#include <math.h>
#include "mp_routines.h"

#define MAX_LATS_LONS 	10

void main44()
{
int 	i,j,k;
int	count;
int	def;
int	status;
int	indices[2],lengthes[2];
int	number_keywords;
int	types[mpNUMBER_OF_KEYWORDS],classes[mpNUMBER_OF_KEYWORDS];
int	ll_type;
int	lat_count,lon_count;
int	line_count,samp_count;
char 	lat_lon_type[20];

double	double_value;
float	lines[MAX_LATS_LONS];
float	samples[MAX_LATS_LONS];
float	latitudes[MAX_LATS_LONS];
float	longitudes[MAX_LATS_LONS];

double	latitude;
double	longitude;
double	line;
double	sample;

double	new_lat;
double	new_lon;
double	new_line;
double	new_samp;

char	keys[mpNUMBER_OF_KEYWORDS][mpMAX_KEYWD_LENGTH+1];
char	pdf_parms[mpNUMBER_OF_KEYWORDS][mpMAX_PARM_LENGTH+1];
char	pds_keys[mpNUMBER_OF_KEYWORDS][mpMAX_KEYWD_LENGTH+1];
char    PCKfile[200];
char	PCKpath[200];
char	string[300];
char	string_value[200];

union {
  struct { float reals[38]; int ints[2];} map1;
  struct { double dbls[12]; float reals[14]; int ints[2];} map2;
} mapbuf;

MP mp_obj;

/* Define user parameters to be retrieved from PDF file. */

strcpy(pdf_parms[0],"TARGET");
strcpy(pds_keys[0],mpTARGET_NAME);

strcpy(pdf_parms[1],"PROJ");
strcpy(pds_keys[1],mpMAP_PROJECTION_TYPE);

strcpy(pdf_parms[2],"A_AXIS");
strcpy(pds_keys[2],mpA_AXIS_RADIUS);

strcpy(pdf_parms[3],"B_AXIS");
strcpy(pds_keys[3],mpB_AXIS_RADIUS);

strcpy(pdf_parms[4],"C_AXIS");
strcpy(pds_keys[4],mpC_AXIS_RADIUS);

strcpy(pdf_parms[5],"SCALE");
strcpy(pds_keys[5],mpMAP_SCALE);

strcpy(pdf_parms[6],"RESOLUTION");
strcpy(pds_keys[6],mpMAP_RESOLUTION);

strcpy(pdf_parms[7],"POS_LON_DIR");
strcpy(pds_keys[7],mpPOSITIVE_LONGITUDE_DIRECTION);

strcpy(pdf_parms[8],"CTR_LAT");
strcpy(pds_keys[8],mpCENTER_LATITUDE);

strcpy(pdf_parms[9],"CTR_LON");
strcpy(pds_keys[9],mpCENTER_LONGITUDE);

strcpy(pdf_parms[10],"SPHERICAL_AZ");
strcpy(pds_keys[10],mpSPHERICAL_AZIMUTH);

strcpy(pdf_parms[11],"CARTESIAN_AZ");
strcpy(pds_keys[11],mpCARTESIAN_AZIMUTH);

strcpy(pdf_parms[12],"LINE_OFFSET");
strcpy(pds_keys[12],mpLINE_PROJECTION_OFFSET);

strcpy(pdf_parms[13],"SAMPLE_OFFSET");
strcpy(pds_keys[13],mpSAMPLE_PROJECTION_OFFSET);

strcpy(pdf_parms[14],"PARALLEL_ONE");
strcpy(pds_keys[14],mpFIRST_STANDARD_PARALLEL);

strcpy(pdf_parms[15],"PARALLEL_TWO");
strcpy(pds_keys[15],mpSECOND_STANDARD_PARALLEL);

strcpy(pdf_parms[16],"XYZ");
strcpy(pds_keys[16],mpFOCAL_LENGTH);

strcpy(pdf_parms[17],"RANGE");
strcpy(pds_keys[17],mpTARGET_CENTER_DISTANCE);

strcpy(pdf_parms[18],"AXIS_LINE");
strcpy(pds_keys[18],mpOPT_AXIS_INTERCEPT_LINE);

strcpy(pdf_parms[19],"AXIS_SAMPLE");
strcpy(pds_keys[19],mpOPT_AXIS_INTERCEPT_SAMPLE);

strcpy(pdf_parms[20],"FOCAL_SCALE");
strcpy(pds_keys[20],mpFOCAL_PLANE_SCALE);

strcpy(pdf_parms[21],"SUB_SPACE_LAT");
strcpy(pds_keys[21],mpSUB_SPACECRAFT_LATITUDE);

strcpy(pdf_parms[22],"SUB_SPACE_LON");
strcpy(pds_keys[22],mpSUB_SPACECRAFT_LONGITUDE);

strcpy(pdf_parms[23],"CENT_LINE");
strcpy(pds_keys[23],mpPLANET_CENTER_LINE);

strcpy(pdf_parms[24],"CENT_SAMPLE");
strcpy(pds_keys[24],mpPLANET_CENTER_SAMPLE);

strcpy(pdf_parms[25],"N_ANGLE");
strcpy(pds_keys[25],mpNORTH_ANGLE);

strcpy(pdf_parms[26],"LONG_AXIS");
strcpy(pds_keys[26],mpBODY_LONG_AXIS);

strcpy(pdf_parms[27],"LL_TYPE");
strcpy(pds_keys[27],mpCOORDINATE_SYSTEM_NAME);

strcpy(pdf_parms[28],"TGTBOD");
strcpy(pds_keys[28],mpTARGET_BODY);

pdf_parms[29][0] = '\0';
pds_keys[29][0] = '\0';

/* Set user parameters */

status = zvparm("PCK_PATH",PCKpath,&count,&def,1,0 );
if ( status < 0 )
	{
	zvmessage("SPICE PCK file pathname not found."," ");
	zvmessage("Pathname set to blank"," ");
	strcpy(PCKpath," ");
	}
zvsptr(PCKpath,count,indices,lengthes);
indices[0] -= 1;
strncpy(PCKfile,&PCKpath[indices[0]],lengthes[0]);
PCKfile[lengthes[0]]='\0';

/* Convert input path for PCK file to absolute path. */
status = zvfilename( PCKfile, PCKpath, 0 );
ABENDif( status < VICARrtlSUCCESS );

status = mpGetPar( &mp_obj,pdf_parms,pds_keys,PCKpath );
if ( status < 0 ) zmabend("Error in mpgetpar call");

status = mpGetKeywords( mp_obj,keys,&number_keywords,types,classes );
if ( status < 0 ) zmabend("Error in mpGetKeywords call");

status = mpSetDebugFlag( FALSE );
ABENDif( status<mpSUCCESS );

for ( i=0; i<number_keywords; i++ )
  switch ( types[i] )  {

  case mpCHAR:

    status = mpGetValues( mp_obj,keys[i],string_value,"" );
    ABENDif( status < mpSUCCESS );
    
    sprintf(string,"KEYWORD %s equals %s",keys[i],string_value);
    zvmessage(string," ");
    
    break;

  case mpDBLE:

    status = mpGetValues( mp_obj,keys[i],&double_value,"" );
    ABENDif( status < mpSUCCESS );
    
    sprintf(string,"KEYWORD %s equals %4.3e",keys[i],double_value);
    zvmessage(string," ");

    break;

  default:

    zvmessage("PDS KEY of unacceptable data type"," ");
    break;

  }

status = mpMpo2Buf( mp_obj, &mapbuf );
if( status != mpSUCCESS ) zvmessage(" MPO2BUF failed!", "");

zvmessage( " Old MAP Buffer:", "");
for (i=0; i<39; i++) {
  if (i<23) {
    if (mapbuf.map1.ints[0]==16) {
      j = i/2;
      if (2*j != i) continue;
      else sprintf( string, " BUF(%d:%d) = %f", i+1, i+2, mapbuf.map2.dbls[j]);
    }
    else sprintf( string, " BUF(%d) = %f", i+1, mapbuf.map1.reals[i]);
  }
  else if (i<38)
    sprintf( string, " BUF(%d) = %f", i+1, mapbuf.map1.reals[i]);
  else
    sprintf( string, " BUF(%d) = %d", i+1, mapbuf.map1.ints[i-38]);
  zvmessage( string, "");
}

status = mpBuf2Mpo( &mapbuf.map1, mp_obj);

if( status == mpSUCCESS ) zvmessage(" BUF2MPO succeeded!", "");
else zvmessage(" BUF2MPO *failed* !!", "");

mpFree( mp_obj );

}
$!-----------------------------------------------------------------------------
$ create tmp_buf2mpo.imake
#define PROGRAM tmp_buf2mpo

#define MODULE_LIST tmp_buf2mpo.c

#define MAIN_LANG_C
#define USES_ANSI_C
#define USES_FORTRAN

#define R2LIB
#define LIB_P1SUB
#define LIB_MATH77
#define LIB_RTL
#define LIB_SPICE
#define LIB_P2SUB
#define LIB_TAE
#define LIB_FORTRAN

#if 0
#define LIB_LOCAL	/* for development, remove on delivery */
#define DEBUG		/* for development, remove on delivery */
#endif

$!-----------------------------------------------------------------------------
$ create tmp_buf2mpo.pdf
process help=*
PARM TARGET		STRING	COUNT=1
PARM PROJ		STRING	COUNT=1
PARM A_AXIS		REAL	COUNT=0:1	DEFAULT=--		
PARM B_AXIS		REAL	COUNT=0:1	DEFAULT=--
PARM C_AXIS		REAL	COUNT=0:1	DEFAULT=--
PARM SCALE		REAL	COUNT=0:1	DEFAULT=--
PARM RESOLUTION		REAL	COUNT=0:1	DEFAULT=--
PARM POS_LON_DIR	STRING	COUNT=1
PARM CTR_LAT		REAL	COUNT=1
PARM CTR_LON		REAL	COUNT=1
PARM SPHERICAL_AZ	REAL	COUNT=0:1	DEFAULT=0.0
PARM CARTESIAN_AZ	REAL	COUNT=0:1	DEFAULT=0.0
PARM LINE_OFFSET	REAL	COUNT=0:1	DEFAULT=0.0
PARM SAMPLE_OFFSET	REAL	COUNT=0:1	DEFAULT=0.0
PARM PARALLEL_ONE	REAL	COUNT=0:1	DEFAULT=--
PARM PARALLEL_TWO	REAL	COUNT=0:1	DEFAULT=--
PARM LONG_AXIS		REAL	COUNT=0:1	DEFAULT=--
PARM TGTBOD		STRING	COUNT=0:1	DEFAULT=--
PARM PCK_PATH		STRING 	COUNT=1
PARM LL_TYPE		STRING  COUNT=1 +
	VALID=("PLANETODETIC","PLANETOCENTRIC","SNYDER_DEFINED")
PARM RANGE		REAL	COUNT=0:1	DEFAULT=--		
PARM XYZ		REAL	COUNT=0:1	DEFAULT=--
PARM AXIS_LINE		REAL	COUNT=0:1	DEFAULT=--
PARM AXIS_SAMPLE	REAL	COUNT=0:1	DEFAULT=--
PARM FOCAL_SCALE	REAL	COUNT=0:1	DEFAULT=--
PARM SUB_SPACE_LAT	REAL	COUNT=0:1	DEFAULT=--
PARM SUB_SPACE_LON	REAL	COUNT=0:1	DEFAULT=--
PARM CENT_LINE		REAL	COUNT=0:1	DEFAULT=--
PARM CENT_SAMPLE	REAL	COUNT=0:1	DEFAULT=--
PARM N_ANGLE		REAL	COUNT=0:1	DEFAULT=--
end-proc
.TITLE
VICAR program TMP_ROUTINES
.HELP
PURPOSE:
Test program for mpBuf2Mpo()

.LEVEL1

.VARI TARGET
Target body of object for which map projection points will
be transformed.

.VARI PROJ
Map projection type requested.

.VARI A_AXIS
Semimajor axis of target body.

.VARI B_AXIS
Semiminor axis of target body.

.VARI C_AXIS
Polar axis of target body.

.VARI SCALE
Map scale.

.VARI RESOLUTION
Map resolution.

.VARI POS_LON_DIR
Positive longitude direction.

.VARI CTR_LAT
Center latitude

.VARI CTR_LON
Center longitude

.VARI SPHERICAL_AZ
Spherical azimuth

.VARI CARTESIAN_AZ
Cartesian azimuth

.VARI LINE_OFFSET
Line projection offset

.VARI SAMPLE_OFFSET
Sample projection offset

.VARI PARALLEL_ONE
First standard parallel

.VARI PARALLEL_TWO
Second standard paralel

.VARI LONG_AXIS
Body long axis offset longitude

.VARI LAT_LON_TYPE
Type of latitude and longitude as input or to be returned.
Valid types are 'PLANETOCENTRIC', 'PLANETODETIC', and 'SNYDER-DEFINED'.

.END
$!-----------------------------------------------------------------------------
$ create tmp_create.imake
#define PROGRAM tmp_create

#define MODULE_LIST tmp_create.c

#define MAIN_LANG_C
#define R2LIB

#define USES_ANSI_C

#define R2LIB
#define LIB_P1SUB
#define LIB_SPICE
#define LIB_MATH77
#define LIB_RTL
#define LIB_P2SUB
#define LIB_TAE

#define USES_FORTRAN
#define LIB_FORTRAN

#if 0
#define LIB_LOCAL	/* for development, remove on delivery */
#define DEBUG		/* for development, remove on delivery */
#endif

$!-----------------------------------------------------------------------------
$ create tmp_create.pdf
PROCESS HELP=*

parm inp string count=0:1 default=--
parm out string count=0:1 default=--
parm nl integer count=0:1 default=--
parm ns integer count=0:1 default=--

parm MAP_SCALE real count=0:1 default=--

parm MAP_RESOLUTION real count=0:1 default=--

parm MAP_PROJECTION string valid=(POLAR_ORTHOGRAPHIC, +
 OBLIQUE_ORTHOGRAPHIC, POLAR_STEREOGRAPHIC, OBLIQUE_STEREOGRAPHIC, +
 LAMBERT_CONFORMAL, MERCATOR, NORMAL_CYLINDRICAL, SIMPLE_CYLINDRICAL, +
 OBLIQUE_SIMPLE_CYLINDRICAL, SINUSOIDAL, OBLIQUE_SINUSOIDAL, MOLLWEIDE, +
 TRANSVERSE_MERCATOR, POINT_PERSPECTIVE) count=0:1 default=--

parm POSITIVE_LONGIT string valid=(EAST,WEST) count=0:1 default=--

parm CENTER_LATITUDE real count=0:1 default=--

parm CENTER_LONGITUD real count=0:1 default=--

parm SPHERICAL_AZIMU real count=0:1 default=--

parm CARTESIAN_AZIMU real count=0:1 default=--

parm LINE_PROJECTION real count=0:1 default=--

parm SAMPLE_PROJECTI real count=0:1 default=--

parm FIRST_STANDARD_ real count=0:1 default=--

parm SECOND_STANDARD real count=0:1 default=--

parm FOCAL_LENGTH real count=0:1 default=--

parm FOCAL_PLANE_SCA real count=0:1 default=--

parm NORTH_ANGLE real count=0:1 default=--

parm OPT_AXIS_LINE real count=0:1 default=--

parm OPT_AXIS_SAMP real count=0:1 default=--

parm PLANET_CENTER_L real count=0:1 default=--

parm PLANET_CENTER_S real count=0:1 default=--

parm SUB_SCRAFT_LATI real count=0:1 default=--

parm SUB_SCRAFT_LONG real count=0:1 default=--

parm SPACECRAFT_DIST real count=0:1 default=--

parm TARGET_CENTER_D real count=0:1 default=--

parm A_AXIS_RADIUS real count=0:1 default=--

parm B_AXIS_RADIUS real count=0:1 default=--

parm C_AXIS_RADIUS real count=0:1 default=--

parm TARGET_NAME string count=0:1 default=--

parm TARGET_BODY string count=0:1 default=--

parm MINIMUM_LATITUD real count=0:1 default=--

parm MAXIMUM_LATITUD real count=0:1 default=--

parm MINIMUM_LONGITU real count=0:1 default=--

parm MAXIMUM_LONGITU real count=0:1 default=--

END-PROC

.TITLE
VICAR Program MPTEST
.HELP

 Test MP package by creating a label from arbitrary set of inputs.

.END
$!-----------------------------------------------------------------------------
$ create tmp_create.c
/* Program TMP_CREATE
 *
 *  3dec97 -lwk- hacked from pgm mptest, in order to test new MP keywords
 */

#include <math.h>
#include "mp_routines.h"
#include "vicmain_c"

#define NKWDS 32		/* number of keywords in TMP_CREATE.PDF */

void main44()
{
  int cnt, def, i, in, ival, ival1, len, nl, nl1, ns, ns1, nkeys, num, out,
   status, unit1, unit2;
  int type[mpNUMBER_OF_KEYWORDS], class[mpNUMBER_OF_KEYWORDS];
  double dval, dval1;
  char cval[133], cval1[133], msg[133],
   keylist[mpNUMBER_OF_KEYWORDS][mpMAX_KEYWD_LENGTH+1];
  MP mp_obj;

  /* becuse TAE limits parameter names to 15 chars, we must keep track
   * of both PDF and true names ... */
  char pdfkwds[NKWDS][17] = { "MAP_SCALE", "MAP_RESOLUTION", "MAP_PROJECTION",
   "POSITIVE_LONGIT", "CENTER_LATITUDE", "CENTER_LONGITUD",
   "SPHERICAL_AZIMU", "CARTESIAN_AZIMU", "LINE_PROJECTION",
   "SAMPLE_PROJECTI", "FIRST_STANDARD_", 
   "SECOND_STANDARD", "FOCAL_LENGTH", "FOCAL_PLANE_SCA", 
   "NORTH_ANGLE", "OPT_AXIS_LINE", "OPT_AXIS_SAMP", 
   "PLANET_CENTER_L", "PLANET_CENTER_S", "SUB_SCRAFT_LATI", 
   "SUB_SCRAFT_LONG", "SPACECRAFT_DIST", "TARGET_CENTER_D",
   "A_AXIS_RADIUS", "B_AXIS_RADIUS", "C_AXIS_RADIUS", "TARGET_NAME",
   "TARGET_BODY", "MINIMUM_LATITUD", "MAXIMUM_LATITUD", "MINIMUM_LONGITU",
   "MAXIMUM_LONGITU"};
  char kwdnams[NKWDS][mpMAX_KEYWD_LENGTH] = { "MAP_SCALE", "MAP_RESOLUTION",
   "MAP_PROJECTION_TYPE",
   "POSITIVE_LONGITUDE_DIRECTION", "CENTER_LATITUDE", "CENTER_LONGITUDE",
   "SPHERICAL_AZIMUTH", "CARTESIAN_AZIMUTH", "LINE_PROJECTION_OFFSET",
   "SAMPLE_PROJECTION_OFFSET", "FIRST_STANDARD_PARALLEL", 
   "SECOND_STANDARD_PARALLEL", "FOCAL_LENGTH", "FOCAL_PLANE_SCALE", 
   "NORTH_ANGLE", "OPT_AXIS_INTERCEPT_LINE", "OPT_AXIS_INTERCEPT_SAMPLE", 
   "PLANET_CENTER_LINE", "PLANET_CENTER_SAMPLE", "SUB_SPACECRAFT_LATITUDE", 
   "SUB_SPACECRAFT_LONGITUDE", "SPACECRAFT_DISTANCE", "TARGET_CENTER_DISTANCE",
   "A_AXIS_RADIUS", "B_AXIS_RADIUS", "C_AXIS_RADIUS", "TARGET_NAME",
   "TARGET_BODY", "MINIMUM_LATITUDE", "MAXIMUM_LATITUDE", "MINIMUM_LONGITUDE",
   "MAXIMUM_LONGITUDE"};
  int ktypes[NKWDS] = { 1, 1, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
   1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 1, 1, 1, 1};	/* 0 = char, 1 = real */

  len = mpMAX_KEYWD_LENGTH+1;

  zvmessage(" program TMP_CREATE", "");

  zveaction("","");

  status = mpInit( &mp_obj);

  zvpcnt( "INP", &in);
  if (in>0) {
    zvunit( &unit1, "INP", 1, 0);
    zvopen( unit1, "OP", "READ", 0);
    status = mpLabelRead( mp_obj, unit1);
    zvget( unit1, "nl", &nl, "ns", &ns, 0);
  }
  else {nl=10; ns=10;}

  zvparm( "NL", &nl1, &cnt, &def, 1, 0);
  zvparm( "NS", &nl1, &cnt, &def, 1, 0);
  if (cnt) {nl=nl1; ns=ns1;}

  nkeys = 0;
  for (i=0; i<NKWDS; i++) {
    if (!ktypes[i]) {
      zvparm( pdfkwds[i], cval, &cnt, &def, 1, 0);
      if (cnt) {
	status = mpSetValues( mp_obj, kwdnams[i], cval, "");
	nkeys++;
      }
    }
    else {
      zvparmd( pdfkwds[i], &dval, &cnt, &def, 1, 0);
      if (cnt) {
	status = mpSetValues( mp_obj, kwdnams[i], dval, "");
	nkeys++;
      }
    }
  }

  /* first call with zero list to return num, to verify this works ... */
  status = mpGetKeywords( mp_obj, 0, &num, type, class);
  if (num!=nkeys) zmabend(" *** mpGetKeywords returned wrong count! ***");
  status = mpGetKeywords( mp_obj, keylist, &num, type, class);

  /* print out all the keywords: */
  for (i=0; i<num; i++) {
    if (type[i] == mpCHAR) {
      status = mpGetValues( mp_obj, keylist[i], cval1, "");
      strcpy( msg, keylist[i]);
      strcat( msg, " = ");
      strcat( msg, cval1);
      zvmessage( msg, "");
    }
    else if (type[i] == mpDBLE) {
      status = mpGetValues( mp_obj, keylist[i], &dval1, "");
      strcpy( msg, keylist[i]);
      strcat( msg, " = ");
      sprintf( cval1, " %10.3e", dval1);
      strcat( msg, cval1);
      zvmessage( msg, "");
    }
  }

	/* now open the file and write the map labels */
  zvpcnt( "OUT", &out);
  if (out>0) {
    zvunit( &unit2, "OUT", 1, 0);
    zvopen( unit2, "U_FORMAT", "BYTE", "OP", "write", "U_NL", nl, "U_NS", ns,
     0);
    status = mpLabelWrite( mp_obj, unit2, "HISTORY");
    status = mpLabelWrite( mp_obj, unit2, "PROPERTY");
    zvclose( unit2, 0);
  }
}
$!-----------------------------------------------------------------------------
$ create tstmp_routines.pdf
procedure
refgbl $echo
refgbl $becho
body
let _onfail="continue"
let $echo="no"
let $becho="no"
refgbl $syschar

local pckernel type=string	! variable for p_constants.ker

if ( $syschar(1) = "UNIX" )
	let pckernel = "/project/spice/ops/p_constants.ker"
else
	let pckernel = "SPICEKER:p_constants.ker"
end-if

write " "
write "***********************"
write "*** NOTE TO TESTERS ***"
write "***********************"
write " "
write "The image_lambert.map2 and pov.img must be in your working directory."
write "These files can be found on the MIPL Unix file server in the"
write "directory /project/it/testdata/."
write " "
write " "
write " "
write "***	TEST ONE	***"
write " "
write "Test SINUSOIDAL projection with NON-spherical model"
write "with center longitude of 90 degrees west."
write "Run this twice, the second time using alternative keywords:"
write "RESOLUTION (for SCALE), TARGET_BODY (obsolete for TARGET_NAME),"
write "and PLANETOGRAPHIC (for PLANETODETIC), to demonstrate that these"
write "are equivalent."
write " "
let $echo="yes"
let $becho="yes"

tmp_routines +
	LATITUDES=(45.0,45.0,45.0,45.0,45.0)	+
	LONGITUDES=(90.0,45.0,0.0,-45.0,-90.0)	+
	LINES=--			+
	SAMPLES=--			+
	LL_TYPE="PLANETOGRAPHIC"	+
	TARGET="MARS" 		+
	PROJ="SINUSOIDAL"	+
	A_AXIS=6380.0		+
	B_AXIS=6380.0		+
	C_AXIS=6280.0		+
	SCALE=25.0		+
	POS_LON_DIR="WEST"	+
	SPHERICAL_AZ=0.0	+
	CARTESIAN_AZ=0.0	+
	CTR_LON=90.0		+
	CTR_LAT=0.0		+
	LINE_OFFSET=0.0		+
	SAMPLE_OFFSET=0.0	+
	PARALLEL_ONE=0.0	+
	PARALLEL_TWO=0.0	+
	PCK_PATH=@pckernel

tmp_routines +
	LATITUDES=(45.0,45.0,45.0,45.0,45.0)	+
	LONGITUDES=(90.0,45.0,0.0,-45.0,-90.0)	+
	LINES=--			+
	SAMPLES=--			+
	LL_TYPE="PLANETODETIC"	+
	TGTBOD="MARS" 		+
	PROJ="SINUSOIDAL"	+
	A_AXIS=6380.0		+
	B_AXIS=6380.0		+
	C_AXIS=6280.0		+
	RESOLUTION=4.45408	+
	POS_LON_DIR="WEST"	+
	SPHERICAL_AZ=0.0	+
	CARTESIAN_AZ=0.0	+
	CTR_LON=90.0		+
	CTR_LAT=0.0		+
	LINE_OFFSET=0.0		+
	SAMPLE_OFFSET=0.0	+
	PARALLEL_ONE=0.0	+
	PARALLEL_TWO=0.0	+
	PCK_PATH=@pckernel

let $echo="no"
let $becho="no"
write " "
write "***	TEST TWO	***"
write " "
write "Test SINUSOIDAL projection with spherical model"
write "with center longitude of 270 degrees east and"
write "a longitude convention of positive east."
write " "
let $echo="yes"
let $becho="yes"

tmp_routines +
	LATITUDES=(45.0,45.0,45.0,45.0,45.0)	+
	LONGITUDES=(270.0,315.0,0.0,45.0,90.0)	+
	LINES=--			+
	SAMPLES=--			+
	LL_TYPE="PLANETOCENTRIC"	+
	TARGET="MARS" 		+
	PROJ="SINUSOIDAL"	+
	A_AXIS=6380.0		+
	B_AXIS=6380.0		+
	C_AXIS=6380.0		+
	SCALE=25.0		+
	POS_LON_DIR="EAST"	+
	SPHERICAL_AZ=0.0	+
	CARTESIAN_AZ=0.0	+
	CTR_LON=270.0		+
	CTR_LAT=0.0		+
	LINE_OFFSET=0.0		+
	SAMPLE_OFFSET=0.0	+
	PARALLEL_ONE=0.0	+
	PARALLEL_TWO=0.0	+
	PCK_PATH=@pckernel

let $echo="no"
let $becho="no"
write " "
write "***	TEST THREE	***"
write " "
write "Test SINUSOIDAL projection (same as test two)"
write "but with projection offsets of 400 by line"
write "and 400 by sample."
write " "
let $echo="yes"
let $becho="yes"

tmp_routines +
	LATITUDES=(45.0,45.0,45.0,45.0,45.0)	+
	LONGITUDES=(270.0,315.0,0.0,45.0,90.0)	+
	LINES=--			+
	SAMPLES=--			+
	LL_TYPE="PLANETOCENTRIC"	+
	TARGET="MARS" 		+
	PROJ="SINUSOIDAL"	+
	A_AXIS=6380.0		+
	B_AXIS=6380.0		+
	C_AXIS=6380.0		+
	SCALE=25.0		+
	POS_LON_DIR="EAST"	+
	SPHERICAL_AZ=0.0	+
	CARTESIAN_AZ=0.0	+
	CTR_LON=270.0		+
	CTR_LAT=0.0		+
	LINE_OFFSET=400.0	+
	SAMPLE_OFFSET=400.0	+
	PARALLEL_ONE=0.0	+
	PARALLEL_TWO=0.0	+
	PCK_PATH=@pckernel

let $echo="no"
let $becho="no"
write " "
write "***	TEST FOUR	***"
write " "

write "Numerical Example for auxiliary"
write "authalic latitude where"
write "eccentricity is 0.0822719 . . ."
write "from USGS Bulletin 1395, p. 263."
let $echo="yes"
let $becho="yes"

tmp_routines_verbose +
	LATITUDES=(40.0)  	+
	LONGITUDES=(0.0)  	+
	LL_TYPE="PLANETOGRAPHIC"	+
	TARGET="MARS" 		+
	PROJ="SINUSOIDAL"	+
	A_AXIS=1.0		+
	B_AXIS=1.0		+
	C_AXIS=0.9579812	+
	SCALE=1.0		+
	POS_LON_DIR="EAST"	+
	SPHERICAL_AZ=0.0	+
	CARTESIAN_AZ=0.0	+
	CTR_LON=0.0		+
	CTR_LAT=0.0		+
	LINE_OFFSET=0.0		+
	SAMPLE_OFFSET=0.0	+
	PARALLEL_ONE=0.0	+
	PARALLEL_TWO=0.0	+
	PCK_PATH=@pckernel


let $echo="no"
let $becho="no"
write " "
write "***	TEST FIVE	***"
write " "
write "Test MERCATOR projection with spherical model."
write " "
let $echo="yes"
let $becho="yes"

tmp_routines +
	LATITUDES=(45.0,45.0,45.0,45.0,45.0)	+
	LONGITUDES=(270.0,315.0,0.0,45.0,90.0)	+
	LINES=--			+
	SAMPLES=--			+
	LL_TYPE="PLANETOCENTRIC"	+
	TARGET="MARS" 		+
	PROJ="MERCATOR"		+
	A_AXIS=1.0		+
	B_AXIS=1.0		+
	C_AXIS=1.0		+
	SCALE=1.0		+
	POS_LON_DIR="EAST"	+
	SPHERICAL_AZ=0.0	+
	CARTESIAN_AZ=0.0	+
	CTR_LON=0.0		+
	CTR_LAT=0.0		+
	LINE_OFFSET=0.0		+
	SAMPLE_OFFSET=0.0	+
	PARALLEL_ONE=0.0	+
	PARALLEL_TWO=0.0	+
	PCK_PATH=@pckernel

let $echo="no"
let $becho="no"
write " "
write "***	TEST SIX 	***"
write " "

write "Numerical Example for auxiliary"
write "conformal latitude where"
write "eccentricity is 0.0822719 . . ."
write "from USGS Bulletin 1395, p. 263."
write " "
write "Test MERCATOR projection with oblate spheroid model"
write "in planetodetic latitude. "
write " "
let $echo="yes"
let $becho="yes"

tmp_routines_verbose +
	LATITUDES=(45.0) 	+
	LONGITUDES=(270.0) 	+
	LL_TYPE="PLANETOGRAPHIC" +
	TARGET="MARS" 		+
	PROJ="MERCATOR"		+
	A_AXIS=1.0		+
	B_AXIS=1.0		+
	C_AXIS=0.9579812	+
	SCALE=1.0		+
	POS_LON_DIR="EAST"	+
	SPHERICAL_AZ=0.0	+
	CARTESIAN_AZ=0.0	+
	CTR_LON=0.0		+
	CTR_LAT=0.0		+
	LINE_OFFSET=0.0		+
	SAMPLE_OFFSET=0.0	+
	PARALLEL_ONE=0.0	+
	PARALLEL_TWO=0.0	+
	PCK_PATH=@pckernel

let $echo="no"
let $becho="no"
write " "
write "***	TEST SEVEN	***"
write " "

write "Snyder-defined latitude for a test"
write "of the subroutine triaxcoef and"
write "triaxtran with mp_routines."
write " "
write "Test MERCATOR projection with triaxial"
write "ellipsoid model in planetocentric latitude."
write " "
let $echo="yes"
let $becho="yes"

tmp_forward +
	LATITUDES=(30.0)  		+
	LONGITUDES=(10.0)  		+
	LL_TYPE="PLANETOCENTRIC"	+
	TARGET="MARS" 		+
	PROJ="MERCATOR"		+
	A_AXIS=1.0		+
	B_AXIS=0.99		+
	C_AXIS=0.94		+
	SCALE=1.0		+
	POS_LON_DIR="EAST"	+
	SPHERICAL_AZ=0.0	+
	CARTESIAN_AZ=0.0	+
	CTR_LON=0.0		+
	CTR_LAT=0.0		+
	LINE_OFFSET=0.0		+
	SAMPLE_OFFSET=0.0	+
	PARALLEL_ONE=0.0	+
	PARALLEL_TWO=0.0	+
	PCK_PATH=@pckernel

let $echo="no"
let $becho="no"
write " "
write "***	TEST EIGHT	***"
write " "
let $echo="yes"
let $becho="yes"

tmp_forward +
	LATITUDES=(30.0)  		+
	LONGITUDES=(10.0)  		+
	LL_TYPE="SNYDER_DEFINED"	+
	TARGET="MARS" 		+
	PROJ="MERCATOR"		+
	A_AXIS=1.0		+
	B_AXIS=0.99		+
	C_AXIS=0.94		+
	SCALE=1.0		+
	POS_LON_DIR="EAST"	+
	SPHERICAL_AZ=0.0	+
	CARTESIAN_AZ=0.0	+
	CTR_LON=0.0		+
	CTR_LAT=0.0		+
	LINE_OFFSET=0.0		+
	SAMPLE_OFFSET=0.0	+
	PARALLEL_ONE=0.0	+
	PARALLEL_TWO=0.0	+
	PCK_PATH=@pckernel


let $echo="no"
let $becho="no"
write " "
write "***	TEST NINE	***"
write " "

write " "
write "Repeat of TEST ONE using FORTRAN test program."
write " "
let $echo="yes"
let $becho="yes"

tmp_routines_f +
	LATITUDES=(45.0) 	+
	LONGITUDES=(90.0) 	+
        LINES=--                         +
        SAMPLES=--                       +
	LL_TYPE="PLANETOCENTRIC"	+
	TARGET="MARS" 		+
	PROJ="SINUSOIDAL"	+
	A_AXIS=6380.0		+
	B_AXIS=6380.0		+
	C_AXIS=6380.0		+
	SCALE=25.0		+
	POS_LON_DIR="WEST"	+
	SPHERICAL_AZ=0.0	+
	CARTESIAN_AZ=0.0	+
	CTR_LON=0.0		+
	CTR_LAT=90.0		+
	LINE_OFFSET=0.0		+
	SAMPLE_OFFSET=0.0	+
	PARALLEL_ONE=0.0	+
	PARALLEL_TWO=0.0	+
	PCK_PATH=@pckernel

let $echo="no"
let $becho="no"
write " "
write "***	TEST TEN	***"
write " "
write " List the label of the image projected in a Lambert Conformal"
write " Conic projection.  This label is the old-style MAP00X format."
write " "
let $echo="yes"
let $becho="yes"
label-list image_lambert.map2

let $echo="no"
let $becho="no"
write " "
write " Translate this old-style label to a new MAP VICAR property"
write " and history label via the program tmp_label."
write " "
let $echo="yes"
let $becho="yes"
tmp_label image_lambert.map2 image_new_label.img

let $echo="no"
let $becho="no"
write " "
write " List the label of the image projected in a Lambert Conformal"
write " Conic projection with the new labels. Values in the new MAP"
write " labels are derived from the old-style label, and should"
write " agree, except that LINE_- and SAMPLE_PROJECTION_OFFSET are"
write " defined differently under the new system and should be"
write " offset from values in the old map labels by a value of 1.0."
write " "
let $echo="yes"
let $becho="yes"

label-list image_new_label.img

let $echo="no"
let $becho="no"

write " "
write "***	TEST ELEVEN	***"
write " "
write " "
write "Normal cylindrical test of sphere from"
write "numerical examples in USGS Paper 1395,"
write "Appendix A, page 278."
write " "
write " sample x should equal 3.343"
write " line y should equal 0.338"
write " "
let $echo="yes"
let $becho="yes"

tmp_routines +
	LATITUDES=(35.0)	+
	LONGITUDES=(80.0)	+
	LINES=--			+
	SAMPLES=--			+
	LL_TYPE="PLANETOCENTRIC"	+
	TARGET="MARS" 		+
	PROJ="CYLINDRICAL_EQUAL_AREA"		+
	A_AXIS=1.0		+ 
	B_AXIS=1.0		+
	C_AXIS=1.0		+
	SCALE=1.0		+
	POS_LON_DIR="EAST"	+
	SPHERICAL_AZ=0.0	+
	CARTESIAN_AZ=0.0	+
	CTR_LON=285.0		+
	CTR_LAT=0.0		+
	LINE_OFFSET=0.0		+
	SAMPLE_OFFSET=0.0	+
	PARALLEL_ONE=30.0	+
	PARALLEL_TWO=30.0	+
	PCK_PATH=@pckernel

let $echo="no"
let $becho="no"
write " "
write " "
write "***	TEST TWELVE	***"
write " "
write "Simple (equidistant) cylindrical test of sphere."
write " "
write " sample x should equal 2.2092 and 0.395"
write " line y should equal 0.389 for both x values"
write " "
let $echo="yes"
let $becho="yes"


tmp_routines +
	LATITUDES=(35.0,35.0)	+
	LONGITUDES=(80.0,-40.0)	+
	LINES=--			+
	SAMPLES=--			+
	LL_TYPE="PLANETOCENTRIC"	+
	TARGET="MARS" 		+
	PROJ="SIMPLE_CYLINDRICAL"		+
	A_AXIS=1.0		+ 
	B_AXIS=1.0		+
	C_AXIS=1.0		+
	SCALE=1.0		+
	POS_LON_DIR="EAST"	+
	SPHERICAL_AZ=0.0	+
	CARTESIAN_AZ=0.0	+
	CTR_LON=0.0		+
	CTR_LAT=0.0		+
	LINE_OFFSET=0.0		+
	SAMPLE_OFFSET=0.0	+
	PARALLEL_ONE=30.0	+
	PARALLEL_TWO=0.0	+
	PCK_PATH=@pckernel

write " "
write " "
write "***	TEST THIRTEEN	***"
write " "
write " Test mpMpo2Buf routine and dump to the computed OM matrix"
write " calculated from the POINT_PERSPECTIVE image pov.img."
write " "
write " NOTE: This test is only valid to test funcationality on"
write " AXP and VAX systems.  Difference in output will occur on"
write " the UNIX systems."
write " "
let $echo="yes"
let $becho="yes"

tmp_mpo2buf INP=pov.img LATLON=(-35.0,340.0)

let $echo="no"
let $becho="no"
write " "
write " "
write "*** 	TEST FOURTEEN	***"
write " "
write "Oblique Lambert azimuthal equal-area test of sphere."
write " "
write " projection origin and center at (lat,lon) = (30,30)"
write " "
write " input (lat,lon) of (80,70) should yield (line,samp) of (-.877,.124)"
write " input (lat,lon) of (20,50) should yield (line,samp) of ( .148,.327)"
write " "
let $echo="yes"
let $becho="yes"
tmp_routines +
	LATITUDES=(80.0,20.0)	+
	LONGITUDES=(70.0,50.0)	+
	LINES=--			+
	SAMPLES=--			+
	LL_TYPE="PLANETOCENTRIC"	+
	TARGET="MARS" 		+
	PROJ="LAMBERT_AZIMUTHAL"	+
	A_AXIS=1.0		+
	B_AXIS=1.0		+
	C_AXIS=1.0		+
	SCALE=1.0		+
	POS_LON_DIR="EAST"	+
	SPHERICAL_AZ=0.0	+
	CARTESIAN_AZ=0.0	+
	CTR_LON=30.0		+
	CTR_LAT=30.0		+
	LINE_OFFSET=0.0		+
	SAMPLE_OFFSET=0.0	+
	PARALLEL_ONE=0.0	+
	PARALLEL_TWO=0.0	+
	PCK_PATH=@pckernel

let $echo="no"
let $becho="no"
write " "
write "***	TEST FIFTEEN	***"
write " "
write "Test EQUIDISTANT CYLINDRICAL projection with oblate model"
write " "
write " Expected results are given below."
write " "
write " (Lat,Lon) -> (Line,Sample) -> (Lat',Lon')"
write " " 
write " (  85.000,  90.000) -> (-377.527,-399.867) -> (  85.000,  90.000)"
write " (  75.000,  80.000) -> (-332.856,-355.326) -> (  75.000,  80.000)"
write " (  65.000,  70.000) -> (-288.208,-310.786) -> (  65.000,  70.000)"
write " (  55.000,  60.000) -> (-243.598,-266.245) -> (  55.000,  60.000)"
write " (  45.000,  50.000) -> (-199.033,-221.704) -> (  45.000,  50.000)"
write " "
let $echo="yes"
let $becho="yes"
tmp_routines +
	LATITUDES=(85.0,75.0,65.0,55.0,45.0)	+
	LONGITUDES=(90.0,80.0,70.0,60.0,50.0)	+
	LINES=--			+
	SAMPLES=--			+
	LL_TYPE="PLANETOGRAPHIC"	+
	TARGET="MARS" 		+
	PROJ="EQUIDISTANT"	+
	A_AXIS=6380.0		+
	B_AXIS=6380.0		+
	C_AXIS=6370.0		+
	SCALE=25.0		+
	POS_LON_DIR="WEST"	+
	SPHERICAL_AZ=0.0	+
	CARTESIAN_AZ=0.0	+
	CTR_LON=0.0		+
	CTR_LAT=0.0		+
	LINE_OFFSET=0.0		+
	SAMPLE_OFFSET=0.0	+
	PARALLEL_ONE=0.0	+
	PARALLEL_TWO=0.0	+
	PCK_PATH=@pckernel
let $echo="no"
let $becho="no"

write " "
write "***	TEST SIXTEEN	***"
write " "
write "Test ORTHOGRAPHIC projection with spherical model"
write "with center latitude/longitude of 0 degrees ."
write "Non-zero center lat/lon s case is being reviewed "
write "with modifications in Euler rotations. "
write "Test case in USGS : (45.0,2.0) -> (6.2977416,180.45365)"
write "			   (lat,lon)  -> (sample(x),line(y))"
write "Note that above test is with radii=6380."
write " "
write " "
let $echo="yes"
let $becho="yes"

tmp_routines +
	LATITUDES=(30.0,45.0,75.0,60.0,50.0,40.0,10.0,35.0)	+
	LONGITUDES=(70.0,2.0,80.0,10.0,30.0,40.0,90.0,75.0)	+
	LINES=--			+
	SAMPLES=--			+
	LL_TYPE="PLANETOCENTRIC"	+
	TARGET="MARS" 		+
	PROJ="ORTHOGRAPHIC"	+
	A_AXIS=6380.0		+
	B_AXIS=6380.0		+
	C_AXIS=6380.0		+
	SCALE=25.0		+
	POS_LON_DIR="WEST"	+
	SPHERICAL_AZ=0.0	+
	CARTESIAN_AZ=0.0	+
	CTR_LON=0.0		+
	CTR_LAT=0.0		+
	LINE_OFFSET=0.0		+
	SAMPLE_OFFSET=0.0	+
	PARALLEL_ONE=0.0	+
	PARALLEL_TWO=0.0	+
	PCK_PATH=@pckernel

let $echo="no"
let $becho="no"

write " "
write "***	TEST SEVENTEEN	***"
write " "
write "Test ALBERS EQUAL AREA CONIC projection with spherical model"
write "with center latitude/longitude of 0 degrees , one or two parallels"
write "Non-zero center lat/lon s case is being reviewed "
write "with modifications in Euler rotations. "
write "Test-case USGS: 	(45.0,2.0) -> (6.1273263,187.96771) p_1=35 p_2=65"
write "		(lat,lon)  -> (sample(x), line(y))  2 parallels (degs)" 
write " (45.0,2.0) -> (6.4099229,192.81305)  p_1=35 degrees"
write "Note that above tests are with radii=6380."
write " "
let $echo="yes"
let $becho="yes"

tmp_routines +
	LATITUDES=(45.0,75.0,60.0,50.0,40.0,35.0)	+
	LONGITUDES=(2.0,80.0,10.0,30.0,40.0,75.0)	+
	LINES=--			+
	SAMPLES=--			+
	LL_TYPE="PLANETOCENTRIC"	+
	TARGET="MARS" 		+
	PROJ="ALBERS_TWO_PARALLELS"	+
	A_AXIS=6380.0		+
	B_AXIS=6380.0		+
	C_AXIS=6380.0		+
	SCALE=25.0		+
	POS_LON_DIR="WEST"	+
	SPHERICAL_AZ=0.0	+
	CARTESIAN_AZ=0.0	+
	CTR_LON=0.0		+
	CTR_LAT=0.0		+
	LINE_OFFSET=0.0		+
	SAMPLE_OFFSET=0.0	+
	PARALLEL_ONE=35.0	+
	PARALLEL_TWO=65.0	+
	PCK_PATH=@pckernel

let $echo="no"
let $becho="no"

write " "
write "***	TEST EIGHTEEN	***"
write " "
write "Test  LAMBERT conformal conic projection with spherical model"
write "with center latitude/longitude of 0 degrees,one or two parallels"
write "Non-zero center lat/lon s case is being reviewed "
write "with modifications in Euler rotations."
write "Test cases USGS: (45.0,2.0) -> (6.1115347,221.52138) p_1=35 p_2=65"
write "		(lat,lon) -> (sample(x), line(y)) 2 parallels degrees"
write "    (45.0,2.0) -> (6.4002125,210.37762) p_1=35 degrees"
write " Note that the above tests are with radii=6380."
write " "
let $echo="yes"
let $becho="yes"

tmp_routines +
	LATITUDES=(30.0,45.0,75.0,60.0,50.0,40.0,35.0)	+
	LONGITUDES=(110.0,2.0,80.0,10.0,30.0,40.0,75.0)	+
	LINES=--			+
	SAMPLES=--			+
	LL_TYPE="PLANETOCENTRIC"	+
	TARGET="MARS" 		+
	PROJ="LAMBERT_TWO_PARALLELS"	+
	A_AXIS=6380.0		+
	B_AXIS=6380.0		+
	C_AXIS=6380.0		+
	SCALE=25.0		+
	POS_LON_DIR="WEST"	+
	SPHERICAL_AZ=0.0	+
	CARTESIAN_AZ=0.0	+
	CTR_LON=0.0		+
	CTR_LAT=0.0		+
	LINE_OFFSET=0.0		+
	SAMPLE_OFFSET=0.0	+
	PARALLEL_ONE=35.0	+
	PARALLEL_TWO=65.0	+
	PCK_PATH=@pckernel

let $echo="no"
let $becho="no"

write " "
write "***	TEST NINETEEN	***"
write " "
write "Test SINUSOIDAL projection (Authalic) with triaxial"
write "ellipsoid model in planetocentric latitude."
write " "
let $echo="yes"
let $becho="yes"

tmp_routines +
	LATITUDES=(40.0)	+
	LONGITUDES=(20.0)  	+
	LINES=--		+
	SAMPLES=--		+
	LL_TYPE="PLANETOCENTRIC"	+
	TARGET="IDA" 		+
	PROJ="SINUSOIDAL"	+
	A_AXIS=1.0		+
	B_AXIS=0.9		+
	C_AXIS=0.8		+
	SCALE=1.0		+
	POS_LON_DIR="EAST"	+
	SPHERICAL_AZ=0.0	+
	CARTESIAN_AZ=0.0	+
	CTR_LON=0.0		+
	CTR_LAT=0.0		+
	LINE_OFFSET=0.0		+
	SAMPLE_OFFSET=0.0	+
	PARALLEL_ONE=0.0	+
	PARALLEL_TWO=0.0	+
	PCK_PATH=@pckernel

let $echo="no"
let $becho="no"

write " "
write "***	TEST TWENTY	***"
write " "
write "Test PERSPECTIVE projection with point_perspective"
write "parameters and input (line,sample) values."
write "Observe the correlation between cent_line,cent_samp values"
write "and the sub_space_lat, and sub_space_lon values, and the"
write "input line,sample value"
write "Model is Spherical, with zero center lat/lon"
write " "
let $echo="yes"
let $becho="yes"

tmp_routines +
	LATITUDES=--	+
	LONGITUDES=--	+
	LINES=(811.0,400.0,200.0)	+
	SAMPLES=(3.0,400.0,50.0)	+
	LL_TYPE="PLANETOCENTRIC"	+
	TARGET="JUPITER"		+
	PROJ="POINT_PERSPECTIVE"	+
	A_AXIS=1565.0		+
	B_AXIS=1565.0		+
	C_AXIS=1565.0		+
	SCALE=1.0		+
	POS_LON_DIR="WEST"	+
	SPHERICAL_AZ=0.0	+
	CARTESIAN_AZ=0.0	+
	CTR_LON=0.0		+
	CTR_LAT=0.0		+
	LINE_OFFSET=0.0		+
	SAMPLE_OFFSET=0.0	+
	PARALLEL_ONE=0.0	+
	PARALLEL_TWO=0.0	+
	TGT_DIST=156164.53125	+
	XYZ=1501.038940429688	+
	AXIS_LINE=400.0		+
	AXIS_SAMPLE=400.0	+
	FOCAL_SCALE=65.61679840087891	+
	SUB_SPACE_LAT=23.85908317565918	+
	SUB_SPACE_LON=217.3724365234375 +
	CENT_LINE=811.0722045898438	+
	CENT_SAMPLE=3.221923838125	+
	N_ANGLE=395.91015625		+
	PCK_PATH=@pckernel

let $echo="no"	
let $becho="no"

write " "
write "***	TEST TWENTY-ONE	   ***"
write " "
write "Test PERSPECTIVE projection with spherical model"
write "with center longitude/latitude of 0 "
write "This test is reverse of Test Twenty, with inputs"
write "being lat/lon s and output line,sample"
write " "
let $echo="yes"
let $becho="yes"

tmp_routines +
	LATITUDES=(23.855,58.318,53.026)	+
	LONGITUDES=(142.614,151.483,110.261)	+
	LINES=--	+
	SAMPLES=--	+
	LL_TYPE="PLANETOCENTRIC"	+
	TARGET="JUPITER" 		+
	PROJ="POINT_PERSPECTIVE"	+
	A_AXIS=1565.0		+
	B_AXIS=1565.0		+
	C_AXIS=1565.0		+
	SCALE=1.0		+
	POS_LON_DIR="WEST"	+
	SPHERICAL_AZ=0.0	+
	CARTESIAN_AZ=0.0	+
	CTR_LON=0.0		+
	CTR_LAT=0.0		+
	LINE_OFFSET=0.0		+
	SAMPLE_OFFSET=0.0	+
	PARALLEL_ONE=0.0	+
	PARALLEL_TWO=0.0	+
	TGT_DIST=156164.53125		+
	XYZ=1501.038940429688		+
	AXIS_LINE=400.0		+
	AXIS_SAMPLE=400.0	+
	FOCAL_SCALE=65.61679840087891	+
	SUB_SPACE_LAT=23.85908317565918  	+
	SUB_SPACE_LON=217.3724365234375   +
	CENT_LINE=811.0722045898438	+
	CENT_SAMPLE=3.221923838125	+
	N_ANGLE=395.91015625		+
	PCK_PATH=@pckernel

let $echo="no"
let $becho="no"

write " "
write "***	TEST TWENTY-TWO 	***"
write " "
write "Test MERCATOR projection with planet Mars spherical case"
write "for inclusion of body_long_axis parameter"
write " "
let $echo="yes"
let $becho="yes"

tmp_forward +
	LATITUDES=(40.0)	+
	LONGITUDES=(20.0)  	+
	LL_TYPE="PLANETOCENTRIC"	+
	TARGET="MARS" 		+
	PROJ="MERCATOR"		+
	A_AXIS=1.0		+
	B_AXIS=1.0		+
	C_AXIS=1.0		+
	SCALE=1.0		+
	POS_LON_DIR="EAST"	+
	SPHERICAL_AZ=0.0	+
	CARTESIAN_AZ=0.0	+
	CTR_LON=40.0		+
	CTR_LAT=0.0		+
	LINE_OFFSET=0.0		+
	SAMPLE_OFFSET=0.0	+
	PARALLEL_ONE=0.0	+
	PARALLEL_TWO=0.0	+
	LONG_AXIS=105.0  	+
	PCK_PATH=@pckernel

let $echo="no"
let $becho="no"

write " "
write "***	TEST TWENTY-THREE	***"
write " "
write "Test STEREOGRAPHIC projection (Conformal) with triaxial"
write "ellipsoid model in planetocentric latitude."
write " "
let $echo="yes"
let $becho="yes"

tmp_routines +
	LATITUDES=(35.0)	+
	LONGITUDES=(75.0)	+
	LINES=--	+
	SAMPLES=--	+
	LL_TYPE="PLANETOCENTRIC"	+
	TARGET="IDA" 		+
	PROJ="STEREOGRAPHIC"	+
	A_AXIS=1.0		+
	B_AXIS=0.9		+
	C_AXIS=0.8		+
	SCALE=1.0		+
	POS_LON_DIR="WEST"	+
	SPHERICAL_AZ=0.0	+
	CARTESIAN_AZ=0.0	+
	CTR_LON=0.0		+
	CTR_LAT=0.0		+
	LINE_OFFSET=0.0		+
	SAMPLE_OFFSET=0.0	+
	PARALLEL_ONE=0.0	+
	PARALLEL_TWO=0.0	+
	PCK_PATH=@pckernel
let $echo="no"
let $becho="no"

write " "
write "***	TEST TWENTY-FOUR	***"
write " "
write "Test CYLINDRICAL EQUAL AREA projection with oblate spheroid model"
write "Test numbers are taken from USGS manual p.281."
write "Output (sample,line) values for given input (lat,lon) actual are"
write "(sample,line)=(-332,699.8m;554,248.5m)"
write " "
let $echo="yes"
let $becho="yes"

tmp_routines +
	LATITUDES=(5.0)	+
	LONGITUDES=(78.0)	+
	LINES=--			+
	SAMPLES=--			+
	LL_TYPE="PLANETOCENTRIC"	+
	TARGET="MARS" 		+
	PROJ="CYLINDRICAL_EQUAL_AREA"	+
	A_AXIS=6378206.4		+
	B_AXIS=6378206.4		+
	C_AXIS=6356584.0		+
	SCALE=1.0		+
	POS_LON_DIR="WEST"	+
	SPHERICAL_AZ=0.0	+
	CARTESIAN_AZ=0.0	+
	CTR_LON=75.0		+
	CTR_LAT=0.0		+
	LINE_OFFSET=0.0		+
	SAMPLE_OFFSET=0.0	+
	PARALLEL_ONE=5.0	+
	PARALLEL_TWO=0.0	+
	PCK_PATH=@pckernel
let $echo="no"
let $becho="no"

write " "
write "***	TEST TWENTY-FIVE	***"
write " "
write "Test LAMBERT AZIMUTHAL projection with oblate spheroid model"
write "Test numbers are taken from the USGS manual p.333."
write "For given input (lat,lon) of (30,110), the actual output"
write "(line,sample) values are (-1056814.9m, -965932.1m)"
write " "
let $echo="yes"
let $becho="yes"

tmp_routines +
	LATITUDES=(30.0,45.0,75.0,60.0,50.0,40.0,35.0)	+
	LONGITUDES=(110.0,2.0,80.0,10.0,30.0,40.0,75.0)	+
	LINES=--			+
	SAMPLES=--			+
	LL_TYPE="PLANETOCENTRIC"	+
	TARGET="MARS" 		+
	PROJ="LAMBERT_AZIMUTHAL"	+
	A_AXIS=6378206.4		+
	B_AXIS=6378206.4		+
	C_AXIS=6356584.0		+
	SCALE=1.0		+
	POS_LON_DIR="WEST"	+
	SPHERICAL_AZ=0.0	+
	CARTESIAN_AZ=0.0	+
	CTR_LON=100.0		+
	CTR_LAT=40.0		+
	LINE_OFFSET=0.0		+
	SAMPLE_OFFSET=0.0	+
	PARALLEL_ONE=0.0	+
	PARALLEL_TWO=0.0	+
	PCK_PATH=@pckernel
let $echo="no"
let $becho="no"

write " "
write "***	TEST TWENTY-SIX	***"
write " "
write "Test STEREOGRAPHIC projection with oblate spheroid model"
write "Test numbers taken from USGS manual p.313."
write "For input (lat,lon) of (30,90) the actual output"
write "(line,sample) is (-1063049.3m, 971630.8m)"
write " "
let $echo="yes"
let $becho="yes"

tmp_routines +
	LATITUDES=(30.0,45.0,75.0,60.0,50.0,40.0,35.0)	+
	LONGITUDES=(90.0,2.0,80.0,10.0,30.0,40.0,75.0)	+
	LINES=--			+
	SAMPLES=--			+
	LL_TYPE="PLANETOCENTRIC"	+
	TARGET="MARS" 		+
	PROJ="STEREOGRAPHIC"	+
	A_AXIS=6378206.4		+
	B_AXIS=6378206.4		+
	C_AXIS=6356584.0		+
	SCALE=0.9999		+
	POS_LON_DIR="WEST"	+
	SPHERICAL_AZ=0.0	+
	CARTESIAN_AZ=0.0	+
	CTR_LON=100.0		+
	CTR_LAT=40.0		+
	LINE_OFFSET=0.0		+
	SAMPLE_OFFSET=0.0	+
	PARALLEL_ONE=0.0	+
	PARALLEL_TWO=0.0	+
	PCK_PATH=@pckernel

let $echo="no"
let $becho="no"
write " "
write "***	TEST TWENTY-SEVEN      ***"
write " "
write "Test that SPACECRAFT_DISTANCE gets automatically converted"
write "to TARGET_CENTER_DISTANCE and TARGET_BODY to TARGET_NAME,"
write "and that new MIN/MAX LAT/LONG keywords show up in the MP label."
let $echo="yes"
let $becho="yes"

tmp_create out=tst.lab +
 target_bod=VENUS +
 focal_len=10 +
 focal_plan=1000 +
 north=45 +
 opt_axis_l=5 +
 opt_axis_s=5 +
 planet_center_l=40 +
 planet_center_s=60 +
 a_axis =3393.4 +
 b_axis =3393.4 +
 c_axis =3393.4 +
 map_proj=point_persp +
 positive_lon =west +
 spacecraft_dist=10000 +
 minimum_lat=40 +
 maximum_lat=60 +
 minimum_lon=145 +
 maximum_lon=240

size tst.lab tst1.lab zoom=2

label-list tst.lab
label-list tst1.lab

let $echo="no"
let $becho="no"
write " "
write "***	TEST TWENTY-EIGHT      ***"
write " "
write "Test routine mpBuf2Mpo, using MP objects created above"
write "for all projections."
write "Note that this does not support Albers or Lambert Azimuthal"
write "projections, and that MAP_SCALE must be specified, not"
write "MAP_RESOLUTION."
let $echo="yes"
let $becho="yes"

tmp_buf2mpo +
	LL_TYPE="PLANETOCENTRIC"	+
	TARGET="MARS" 		+
	PROJ="SINUSOIDAL"	+
	A_AXIS=6380.0		+
	B_AXIS=6380.0		+
	C_AXIS=6380.0		+
	SCALE=25.0		+
	POS_LON_DIR="WEST"	+
	SPHERICAL_AZ=0.0	+
	CARTESIAN_AZ=0.0	+
	CTR_LON=90.0		+
	CTR_LAT=0.0		+
	LINE_OFFSET=0.0		+
	SAMPLE_OFFSET=0.0	+
	PARALLEL_ONE=0.0	+
	PARALLEL_TWO=0.0	+
	PCK_PATH=@pckernel

tmp_buf2mpo +
	LL_TYPE="PLANETOCENTRIC"	+
	TARGET="MARS" 		+
	PROJ="MERCATOR"		+
	A_AXIS=1.0		+
	B_AXIS=1.0		+
	C_AXIS=1.0		+
	SCALE=1.0		+
	POS_LON_DIR="EAST"	+
	SPHERICAL_AZ=0.0	+
	CARTESIAN_AZ=0.0	+
	CTR_LON=0.0		+
	CTR_LAT=0.0		+
	LINE_OFFSET=0.0		+
	SAMPLE_OFFSET=0.0	+
	PARALLEL_ONE=0.0	+
	PARALLEL_TWO=0.0	+
	PCK_PATH=@pckernel

tmp_buf2mpo +
	LL_TYPE="PLANETOCENTRIC"	+
	TARGET="MARS" 		+
	PROJ="CYLINDRICAL_EQUAL_AREA"		+
	A_AXIS=1.0		+ 
	B_AXIS=1.0		+
	C_AXIS=1.0		+
	SCALE=1.0		+
	POS_LON_DIR="EAST"	+
	SPHERICAL_AZ=0.0	+
	CARTESIAN_AZ=0.0	+
	CTR_LON=285.0		+
	CTR_LAT=0.0		+
	LINE_OFFSET=0.0		+
	SAMPLE_OFFSET=0.0	+
	PARALLEL_ONE=30.0	+
	PARALLEL_TWO=30.0	+
	PCK_PATH=@pckernel

tmp_buf2mpo +
	LL_TYPE="PLANETOCENTRIC"	+
	TARGET="MARS" 		+
	PROJ="SIMPLE_CYLINDRICAL"		+
	A_AXIS=1.0		+ 
	B_AXIS=1.0		+
	C_AXIS=1.0		+
	SCALE=1.0		+
	POS_LON_DIR="EAST"	+
	SPHERICAL_AZ=0.0	+
	CARTESIAN_AZ=0.0	+
	CTR_LON=0.0		+
	CTR_LAT=0.0		+
	LINE_OFFSET=0.0		+
	SAMPLE_OFFSET=0.0	+
	PARALLEL_ONE=30.0	+
	PARALLEL_TWO=0.0	+
	PCK_PATH=@pckernel

tmp_buf2mpo +
	LL_TYPE="PLANETOCENTRIC"	+
	TARGET="MARS" 		+
	PROJ="ORTHOGRAPHIC"	+
	A_AXIS=6380.0		+
	B_AXIS=6380.0		+
	C_AXIS=6380.0		+
	SCALE=25.0		+
	POS_LON_DIR="WEST"	+
	SPHERICAL_AZ=0.0	+
	CARTESIAN_AZ=0.0	+
	CTR_LON=0.0		+
	CTR_LAT=0.0		+
	LINE_OFFSET=0.0		+
	SAMPLE_OFFSET=0.0	+
	PARALLEL_ONE=0.0	+
	PARALLEL_TWO=0.0	+
	PCK_PATH=@pckernel

tmp_buf2mpo +
	LL_TYPE="PLANETOCENTRIC"	+
	TARGET="MARS" 		+
	PROJ="LAMBERT_TWO_PARALLELS"	+
	A_AXIS=6380.0		+
	B_AXIS=6380.0		+
	C_AXIS=6380.0		+
	SCALE=25.0		+
	POS_LON_DIR="WEST"	+
	SPHERICAL_AZ=0.0	+
	CARTESIAN_AZ=0.0	+
	CTR_LON=0.0		+
	CTR_LAT=0.0		+
	LINE_OFFSET=0.0		+
	SAMPLE_OFFSET=0.0	+
	PARALLEL_ONE=35.0	+
	PARALLEL_TWO=65.0	+
	PCK_PATH=@pckernel

tmp_buf2mpo +
	LL_TYPE="PLANETOCENTRIC"	+
	TARGET="JUPITER"		+
	PROJ="POINT_PERSPECTIVE"	+
	A_AXIS=1565.0		+
	B_AXIS=1565.0		+
	C_AXIS=1565.0		+
	SCALE=1.0		+
	POS_LON_DIR="WEST"	+
	SPHERICAL_AZ=0.0	+
	CARTESIAN_AZ=0.0	+
	CTR_LON=0.0		+
	CTR_LAT=0.0		+
	LINE_OFFSET=0.0		+
	SAMPLE_OFFSET=0.0	+
	RANGE=156164.53125	+
	XYZ=1501.038940429688	+
	AXIS_LINE=400.0		+
	AXIS_SAMPLE=400.0	+
	FOCAL_SCALE=65.61679840087891	+
	SUB_SPACE_LAT=23.85908317565918	+
	SUB_SPACE_LON=217.3724365234375 +
	CENT_LINE=811.0722045898438	+
	CENT_SAMPLE=3.221923838125	+
	N_ANGLE=395.91015625		+
	PCK_PATH=@pckernel

tmp_buf2mpo +
	LL_TYPE="PLANETOCENTRIC"	+
	TARGET="IDA" 		+
	PROJ="STEREOGRAPHIC"	+
	A_AXIS=1.0		+
	B_AXIS=0.9		+
	C_AXIS=0.8		+
	SCALE=1.0		+
	POS_LON_DIR="WEST"	+
	SPHERICAL_AZ=0.0	+
	CARTESIAN_AZ=0.0	+
	CTR_LON=0.0		+
	CTR_LAT=0.0		+
	LINE_OFFSET=0.0		+
	SAMPLE_OFFSET=0.0	+
	PARALLEL_ONE=0.0	+
	PARALLEL_TWO=0.0	+
	PCK_PATH=@pckernel

let $echo="no"
let $becho="no"
write " "
write "***	TEST TWENTY-NINE      ***"
write " "
write "Test PERSPECTIVE projection with NON-spherical model"
write "with center longitude/latitude of 0"
write " "
let $echo="yes"
let $becho="yes"

tmp_routines +
	LATITUDES=(23.855,58.318,53.026)	+
	LONGITUDES=(142.614,151.483,110.261)	+
	LINES=--	+
	SAMPLES=--	+
	LL_TYPE="PLANETOGRAPHIC"	+
	TARGET="JUPITER" 		+
	PROJ="POINT_PERSPECTIVE"	+
	A_AXIS=1565.0		+
	B_AXIS=1500.0		+
	C_AXIS=1465.0		+
	SCALE=1.0		+
	POS_LON_DIR="WEST"	+
	SPHERICAL_AZ=0.0	+
	CARTESIAN_AZ=0.0	+
	CTR_LON=0.0		+
	CTR_LAT=0.0		+
	LINE_OFFSET=0.0		+
	SAMPLE_OFFSET=0.0	+
	PARALLEL_ONE=0.0	+
	PARALLEL_TWO=0.0	+
	TGT_DIST=156164.53125		+
	XYZ=1501.038940429688		+
	AXIS_LINE=400.0		+
	AXIS_SAMPLE=400.0	+
	FOCAL_SCALE=65.61679840087891	+
	SUB_SPACE_LAT=23.85908317565918  	+
	SUB_SPACE_LON=217.3724365234375   +
	CENT_LINE=811.0722045898438	+
	CENT_SAMPLE=3.221923838125	+
	N_ANGLE=395.91015625		+
	PCK_PATH=@pckernel

let $echo="no"
let $becho="no"
write " "
write "***	TEST TWENTY-NINE 1/2      ***"
write " "
write "Repeat previous test, this time testing FORTRAN bridge"
write " "
let $echo="yes"
let $becho="yes"

tmp_routines_f +
	LATITUDES=(23.855,58.318,53.026)	+
	LONGITUDES=(142.614,151.483,110.261)	+
	LINES=--	+
	SAMPLES=--	+
	LL_TYPE="PLANETOGRAPHIC"	+
	TARGET="JUPITER" 		+
	PROJ="POINT_PERSPECTIVE"	+
	A_AXIS=1565.0		+
	B_AXIS=1500.0		+
	C_AXIS=1465.0		+
	SCALE=1.0		+
	POS_LON_DIR="WEST"	+
	SPHERICAL_AZ=0.0	+
	CARTESIAN_AZ=0.0	+
	CTR_LON=0.0		+
	CTR_LAT=0.0		+
	LINE_OFFSET=0.0		+
	SAMPLE_OFFSET=0.0	+
	PARALLEL_ONE=0.0	+
	PARALLEL_TWO=0.0	+
	TGT_DIST=156164.53125		+
	XYZ=1501.038940429688		+
	AXIS_LINE=400.0		+
	AXIS_SAMPLE=400.0	+
	FOCAL_SCALE=65.61679840087891	+
	SUB_SPACE_LAT=23.85908317565918  	+
	SUB_SPACE_LON=217.3724365234375   +
	CENT_LINE=811.0722045898438	+
	CENT_SAMPLE=3.221923838125	+
	N_ANGLE=395.91015625		+
	PCK_PATH=@pckernel

let $echo="no"
let $becho="no"
write " "
write "***	TEST THIRTY 	***"
write " "
write "Test SINUSOIDAL projection (Authalic) with triaxial"
write "ellipsoid model in planetocentric latitude with,"
write "radii in non-standard order"
write " "
let $echo="yes"
let $becho="yes"

tmp_routines +
	LATITUDES=(40.0)	+
	LONGITUDES=(20.0)  	+
	LINES=--		+
	SAMPLES=--		+
	LL_TYPE="PLANETOCENTRIC"	+
	TARGET="IDA" 		+
	PROJ="SINUSOIDAL"	+
	A_AXIS=1.0		+
	B_AXIS=0.6		+
	C_AXIS=0.8		+
	SCALE=1.0		+
	POS_LON_DIR="EAST"	+
	SPHERICAL_AZ=0.0	+
	CARTESIAN_AZ=0.0	+
	CTR_LON=0.0		+
	CTR_LAT=0.0		+
	LINE_OFFSET=0.0		+
	SAMPLE_OFFSET=0.0	+
	PARALLEL_ONE=0.0	+
	PARALLEL_TWO=0.0	+
	PCK_PATH=@pckernel

let $echo="no"
let $becho="no"

end-proc
$ Return
$!#############################################################################
$Other_File:
$ create mpinit.hlp
1 MIPS SUBROUTINE mpInit

PURPOSE
The function mpInit allocates memory for a map
projection (MP) data object to be used for subsequent
processing with other MP functions.

2 OPERATION

This function allocates memory for a specified 
MP data object using the C function malloc()
and initializes certain control variables and other
values in the data object. This function must be
called prior to the use of any other MP functions.

CALLING SEQUENCE:		

from C		status = mpInit( MP *mp );
from FORTRAN	call mp_init( mp,status ) 

Necessary include files
from calling routine 
or program:			mp_routines.h

INPUT

mp 		(MP type)

A pointer to the MP data structure as returned by mpInit. 
In FORTRAN, this variable is of type REAL*8.  MP type is 
defined in the include file mp_routines.h

RETURN

status		(integer)

Indicator of success or failure of the routine. Zero indicates
success and -1 failure. From FORTRAN, status is returned as an
argument of the function.


3 ENVIRONMENT and LANGUAGE

Software Platform:	Vicar 11.0
Hardware Platforms:	No particular hardware required; tested on 
			DEC Alpha, DEC VAX-VMS, Sun Sparcstation
			(Solaris and SunOS), Silicon Graphics (SGI),
			and HP 700.
Programming Language:	ANSI C

3 BACKGROUND

Author:			Justin McNeill, JPL
Cognizant Engineer:	Justin McNeill (jfm059@ipl.jpl.nasa.gov)
Written:		October 1993
Revision history:	Original
Background and References:	

1. Map Projection Software Set Software Specification Document,
	JPL, April 28, 1993;
2. "Conformal Mapping of the Triaxial Ellipsoid," Survey Review,
	vol. 28, July 1985.
3. MIPS Map Projection Software Users' Guide, D-11810, version 1.0,
	May 1994.

$!-----------------------------------------------------------------------------
$ create mpfree.hlp
1 MIPS SUBROUTINE mpFree

PURPOSE
The function mpFree deallocates memory reserved for 
a map projection data object by mpInit.

2 OPERATION

This function deallocates memory for a specified 
map projection data object using the C function free().
This function should be called once an application
program has completed work with all MP functions.

CALLING SEQUENCE:		

from C		status = mpFree( MP mp );
from FORTRAN	call mp_free( mp ) 

Necessary include files
from calling routine 
or program:			mp_routines.h

INPUT

mp 		(MP type as defined in include file mp_routines.h)

Variable for the address of the map projection data object
as returned by mpInit.

OUTPUT

status		(int)

Returns mpSUCCESS, the value 0.


3 ENVIRONMENT and LANGUAGE

Software Platform:	Vicar 11.0
Hardware Platforms:	No particular hardware required; tested on 
			DEC Alpha, DEC VAX 8650, Sun Sparcstation
			(Solaris and SunOS), Silicon Graphics (SGI),
			and HP 750.
Programming Language:	ANSI C

3 BACKGROUND

Author:			Justin McNeill, JPL
Cognizant Engineer:	Justin McNeill (jfm059@ipl.jpl.nasa.gov)
Written:		October 1993
Revision history:	Original
Background and References:	

1. Map Projection Software Set Software Specification Document,
	JPL, April 28, 1993;
2. "Conformal Mapping of the Triaxial Ellipsoid," Survey Review,
	vol. 28, July 1985.
3. MIPS Map Projection Software Users' Guide, D-11810, version 1.0,
	May 1994.
$!-----------------------------------------------------------------------------
$ create mpll2xy.hlp
1 MIPS SUBROUTINE mpll2xy 

Purpose				

The function mpll2xy projects points from planet surface
latitude and longitude to line and sample for a specified
map projection.

2 OPERATION

This function uses one of three target body models - sphere,
oblate spheroid, triaxial ellipsoid - to perform the necessary
map transformations. Spherical and auxiliary conformal and
authalic latitude formulae are based on formulae from the USGS
Bulletin 1395. Auxiliary conformal latitude and longitude 
formulae for the triaxial ellipsoid are coded from formulae in
John Snyder's paper "Conformal Mapping of the Triaxial 
Ellipsoid" from the journal Survey Review of July 1985, v. 28.

Supported map projections are ...

		ALBERS 
		CYLINDRICAL EQUAL-AREA
		EQUIDISTANT CYLINDRICAL
		LAMBERT AZIMUTHAL EQUAL-AREA
		LAMBERT CONFORMAL CONIC
		MERCATOR (includes Transverse Mercator)
		MOLLWEIDE (homalographic)
		ORTHOGRAPHIC
		SINUSOIDAL
		STEREOGRAPHIC

Libraries and subroutines required to run this
routine: mp_routines suite


CALLING SEQUENCE

from C:		status = mpll2xy( MP mp, double *line, 
			double *sample, double latitude, 
			double longitude, int ll_type );
from FORTRAN:	call mp_ll2xy( mp, line, sample, latitude,
			longitude, ll_type, status )

Necessary include files
from calling routine 
or program:			mp_routines.h 
				(or mp_for_defs.fin for 
				FORTRAN applications)

ARGUMENTS

INPUT

mp 		(MP type)

A pointer to the MP data structure as returned by mpInit.
In FORTRAN, this variable is of type REAL*8. MP type is 
defined in the include file mp_routines.h.

latitude	(double)

Latitude value of point on a target body in one of three
coordinate systems as specified by the user in the 
argument ll_type.

longitude	(double)

Longitude value of point on a target body in one of three
coordinate systems as specified by the user in the 
argument ll_type.

ll_type		(integer)

Type of latitude and longitude input:

	1 - Planetocentric
	2 - Planetodetic
	3 - Snyder-defined for triaxial ellipsoid


OUTPUT

line		(double)

Address of the y position of the map projected point in an image.

sample		(double)

Address of the x position of the map projected point in an image.


RETURN

status 		(integer)

This is an indicator of the success or failure of
retrieving various values for the VICAR pdf file
and initializing the map projection data object.

	0  - successful call to mpll2xy

	-1 - invalid latitude value or failure in 
	     conversion of latitude, longitude values

	-2 - invalid ll_type argument

3 ENVIRONMENT and LANGUAGE

Software Platform:	VICAR 11.0 (VMS/UNIX)
Hardware Platforms:	No particular hardware required; 
			tested on DEC Alpha, DEC VAX-VMS,
			Sun Sparcstation (Solaris and SunOS),
			Silicon Graphics (SGI), and HP 700.
Programming Language:	ANSI C

3 BACKGROUND

Author:			Justin McNeill, JPL
Cognizant Engineer:	Justin McNeill (jfm059@ipl.jpl.nasa.gov)
Written:		October 1993
Revision history:	Original
Background and References:	

1. Map Projection Software Set Software Specification Document,
	JPL, April 28, 1993;
2. "Conformal Mapping of the Triaxial Ellipsoid," Survey Review,
	vol. 28, July 1985.
3. MIPS Map Projection Software Users' Guide, D-11810, version 1.0,
	May 1994.
$!-----------------------------------------------------------------------------
$ create mpxy2ll.hlp
1 MIPS SUBROUTINE mpxy2ll

Purpose				

The function mpxy2ll projects points from line and sample
of a particular map projected image to the planet surface
latitude and longitude.

2 OPERATION

This function uses one of three target body models - sphere,
oblate spheroid, triaxial ellipsoid - to perform the necessary
map transformations. Spherical and auxiliary conformal and
authalic latitude formulae are based on formulae from the USGS
Bulletin 1395. Auxiliary conformal latitude and longitude 
formulae for the triaxial ellipsoid are coded from formulae in
John Snyder's paper "Conformal Mapping of the Triaxial 
Ellipsoid" from the journal Survey Review of July 1985, v. 28.

Supported map projections are ...

		ALBERS 
		CYLINDRICAL EQUAL-AREA
		EQUIDISTANT CYLINDRICAL
		LAMBERT AZIMUTHAL EQUAL-AREA
		LAMBERT CONFORMAL CONIC
		MERCATOR (includes Transverse Mercator)
		MOLLWEIDE (homalographic)
		ORTHOGRAPHIC
		SINUSOIDAL
		STEREOGRAPHIC

Libraries and subroutines required to run this
routine: mp_routines suite


CALLING SEQUENCE

from C:		status = mpxy2ll( MP mp, double line, 
			double sample, double *latitude, 
			double *longitude, int ll_type );
from FORTRAN:	call mp_xy2ll( mp, line, sample, latitude,
			longitude, ll_type, status )

Necessary include files
from calling routine 
or program:			mp_routines.h 
				(or mp_for_defs.fin for 
				FORTRAN applications)

ARGUMENTS

INPUT

mp 		(MP type)

A pointer to the MP data structure as returned by mpInit.
In FORTRAN, this variable is of type REAL*8. MP type is 
defined in the include file mp_routines.h.

line		(double)

Y position of the map projected point in an image.

sample		(double)

X position of the map projected point in an image.

ll_type		(integer)

Type of latitude and longitude output:

	1 - Planetocentric
	2 - Planetodetic
	3 - Snyder-defined for triaxial ellipsoid


OUTPUT

latitude	(double)

Address of latitude value of point on a target body in one of 
three coordinate systems as specified by the user in the 
argument ll_type.

longitude	(double)

Address of longitude value of point on a target body in one of 
three coordinate systems as specified by the user in the 
argument ll_type.


RETURN

status 		(integer)

This is an indicator of the success or failure of
retrieving various values for the VICAR pdf file
and initializing the map projection data object.

	0  - successful call to mpll2xy

	-1 - invalid latitude value or failure in 
	     conversion of latitude, longitude values

	-2 - invalid ll_type argument

3 ENVIRONMENT and LANGUAGE

Software Platform:	VICAR 11.0
Hardware Platforms:	No particular hardware required; 
			tested on DEC Alpha, DEC VAX-VMS,
			Sun Sparcstation (Solaris and SunOS),
			Silicon Graphics (SGI), and HP 700.
Programming Language:	ANSI C

3 BACKGROUND

Author:			Justin McNeill, JPL
Cognizant Engineer:	Justin McNeill (jfm059@ipl.jpl.nasa.gov)
Written:		October 1993
Revision history:	Original
Background and References:	

1. Map Projection Software Set Software Specification Document,
	JPL, April 28, 1993;
2. "Conformal Mapping of the Triaxial Ellipsoid," Survey Review,
	vol. 28, July 1985.
3. MIPS Map Projection Software Users' Guide, D-11810, version 1.0,
	May 1994.
$!-----------------------------------------------------------------------------
$ create mpsetvalues.hlp
1 MIPS SUBROUTINE mpSetValues

PURPOSE

The function mpSetValues accepts values of any data type 
for specific map projection keyword values and sets them 
in the map projection (MP) data object.

2 OPERATION

This function allows controlled access to the modification 
of MP data object values. Every call to this function causes 
the MP software to recalculate all possible precalculated 
values used for a specific map projection.  The function only
sets values which are valid for a particular projection.

CALLING SEQUENCE

from C:	status = mpSetValues( MP mp, char *keyword, 
  		<any-type> *value ... char *keyword, 
		<any-type> *value, NULL )

from FORTRAN:	(see functions mp_set_value and mp_set_value_str)

Necessary include files from calling routine or program: mp_routines.h


INPUT 

mp		(MP type)

A pointer to the MP data structure as returned by mpInit. In FORTRAN,
this variable is of type REAL*8.  MP type is defined in include file
mp_routines.h.

keyword		(character string)

Character string which contains the keyword name of the MP data object
value to be set.

value 		(any type)

Value to be stored for the associated map projection 
keyword in the MP data object.


RETURN

status 		(integer)

Status flag for the operation of the routine. 
Zero (0) signifies success and -1 failure.

3 ENVIRONMENT

Software platforms:	VICAR 11.0 (VMS/UNIX)
Hardware platforms:	This routine has been tested on the
			DEC Alpha, DEC VAX-VMS, Sun 
			Sparcstation (Solaris and SunOS), 
			Silicon Graphics (SGI), and HP 700.
			No particular hardware requirements.

3 BACKGROUND

Author:			Lucas Kamp (lwk059@ipl.jpl.nasa.gov)
Cognizant Engineer:	Justin McNeill (jfm059@ipl.jpl.nasa.gov)
Date written:		October 1993
Revision history:	(see release_notes.txt in mp_routines.com)
Background and References:	

1. Map Projection Software Set Software Specification Document,
	JPL, April 28, 1993;
2. "Conformal Mapping of the Triaxial Ellipsoid," Survey Review,
	vol. 28, July 1985.
3. MIPS Map Projection Software Users' Guide, D-11810, version 1.0,
	May 1994.
$!-----------------------------------------------------------------------------
$ create mp_set_value.hlp
1 MIPS SUBROUTINE mp_set_value

PURPOSE

The function mp_set_value accepts double precision numerical values
for insertion in the map projection (MP) data object for a specific map 
projection keyword value from FORTRAN application programs.

2 OPERATION

This function allows controlled access to MP data object double
precision values.

CALLING SEQUENCE

from C:		(see mpSetValues)

from FORTRAN:	call mp_set_value( mp, keyword, value, status )

Necessary include files from calling routine or program: mp_for_defs.fin


INPUT 

mp		(MP type)

A pointer to the MP data structure as returned by mpInit.  In FORTRAN,
this variable is of type REAL*8.  MP type is defined in include file
mp_routines.h.

keyword		(character string)

Character string which contains the keyword name of the MP data object
value to be set.

value 		(double)

Double precision numerical value to be stored for the associated map 
projection keyword in the MP data object.


OUTPUT

status 		(integer)

Status flag for the operation of the routine. Zero (0) signifies
success.  Negative one (-1) signifies failure.


3 ENVIRONMENT

Software platforms:	VICAR 11.0
Hardware platforms:	This routine has been tested on 
			DEC Alpha, DEC VAX-VMS, Sun Sparcstation
			(Solaris and SunOS), Silicon Graphics (SGI), 
			and HP 700. No particular hardware requirements.

3 BACKGROUND

Author:			Lucas Kamp (lwk059@ipl.jpl.nasa.gov)
Cognizant Engineer:	Justin McNeill (jfm059@ipl.jpl.nasa.gov)
Date written:		October 1993
Revision history:	Original
Background and References:	

1. Map Projection Software Set Software Specification Document,
	JPL, April 28, 1993;
2. "Conformal Mapping of the Triaxial Ellipsoid," Survey Review,
	vol. 28, July 1985.
3. MIPS Map Projection Software Users' Guide, D-11810, version 1.0,
	May 1994.
$!-----------------------------------------------------------------------------
$ create mp_set_value_str.hlp
1 MIPS SUBROUTINE mp_set_value_str

PURPOSE

The function mp_set_value_str stores character string values in the
map projection (MP) data object from FORTRAN application programs.

2 OPERATION

This function allows controlled access to MP data object character 
string values. 

CALLING SEQUENCE

from C:		(see mpSetValues)

from FORTRAN:	call mp_set_value_str( mp, keyword, value, status )

Necessary include files from calling routine or program: mp_for_defs.fin


INPUT 

mp		(MP type)

A pointer to the MP data structure as returned by mpInit.  In FORTRAN,
this variable is of type REAL*8.  MP type is defined in include file
mp_routines.h.

keyword		(character string)

Character string which contains the keyword name of the MP data object
value to be set.

value 		(character string)

Character string value to be stored in the MP data object.


OUTPUT

status 		(integer)

Status flag for the operation of the routine. Zero (0) signifies
success and -1 failure.

3 ENVIRONMENT

Software platforms:	VICAR 11.0 
Hardware platforms:	This routine has been tested on 
			DEC Alpha, DEC VAX-VMS, Sun Sparcstation
			(Solaris and SunOS), Silicon Graphics (SGI), 
			and HP 700. No particular hardware requirements.

3 BACKGROUND

Author:			Lucas Kamp (lwk059@ipl.jpl.nasa.gov)
Cognizant Engineer:	Justin McNeill (jfm059@ipl.jpl.nasa.gov)
Date written:		October 1993
Revision history:	Original
Background and References:	

1. Map Projection Software Set Software Specification Document,
	JPL, April 28, 1993;
2. "Conformal Mapping of the Triaxial Ellipsoid," Survey Review,
	vol. 28, July 1985.
3. MIPS Map Projection Software Users' Guide, D-11810, version 1.0,
	May 1994.
$!-----------------------------------------------------------------------------
$ create mpgetvalues.hlp
1 MIPS SUBROUTINE mpGetValues

PURPOSE

The function mpGetValues retrieves desired map projection 
(MP) data object values given a PDS standard map projection 
keyword.

2 OPERATION

This function allows controlled access to the MP data object
values. Given a set of valid MP keywords, the function checks
internally set bits in the MP software that specify which valid
MP data object keyword values have been set by mpSetValues,
mp_set_value, and/or mp_set_value_str. It returns the values
for previously set keywords.


CALLING SEQUENCE

from C:		status = mpGetValues( MP mp, char *keyword, 
			<any-type> *value, ... char *keyword, 
			<any-type> *value, NULL );

from FORTRAN:	(see functions mp_get_value and mp_get_value_str)

Necessary include files from calling routine or program: mp_routines.h


INPUT 

mp		(MP type)

A pointer to the MP data structure as returned by mpInit.
In FORTRAN, this variable is of type REAL*8.  MP type is defined
in include file mp_routines.h.

keyword		(character string)

Character string which contains the keyword name of the MP data object
value to be set.

OUTPUT 

value 		(any-type)

Pointer to value retrieved from the MP data object for the associated
keyword.

RETURN

status 		(integer)

Status flag for the operation of the routine. 
Zero (0) signifies success.  Negative one (-1) signifies failure.


3 ENVIRONMENT

Software platforms:	VICAR 11.0 (VMS/UNIX)
Hardware platforms:	This routine has been tested on 
			DEC Alpha, DEC VAX-VMS, Sun Sparcstation 
			(Solaris and SunOS), Silicon Graphics (SGI), 
			and HP 700. No particular hardware requirements.

3 BACKGROUND

Author:			Lucas Kamp (lwk059@ipl.jpl.nasa.gov)
Cognizant Engineer:	Justin McNeill (jfm059@ipl.jpl.nasa.gov)
Date written:		October 1993
Revision history: 	(see release_notes.txt in mp_routines.com)
Background and References:	

1. Map Projection Software Set Software Specification Document,
	JPL, April 28, 1993;
2. "Conformal Mapping of the Triaxial Ellipsoid," Survey Review,
	vol. 28, July 1985.
3. MIPS Map Projection Software Users' Guide, D-11810, version 1.0,
	May 1994.
$!-----------------------------------------------------------------------------
$ create mp_get_value.hlp
1 MIPS SUBROUTINE mp_get_value

PURPOSE

The function mp_get_value retrieves double precision numerical values
from the map projection data object from FORTRAN application programs.

2 OPERATION

This function allows controlled access to map projection data object 
double precision numerical values. 


CALLING SEQUENCE

from C:		(see mpGetValues)

from FORTRAN:	call mp_get_value( mp, keyword, value, status )

Necessary include files from calling routine or program: mp_for_defs.fin


INPUT 

mp		(MP type)

A pointer to the MP data structure as returned by mpInit. In FORTRAN, 
this variable is of type REAL*8.  MP type is defined in include file
mp_routines.h.

keyword		(character string)

Character string which contains the keyword name of the MP data object
value to be retrieved.

OUTPUT

value 		(double)

Double precision value to be retrieved from the MP data object.

status 		(integer)

Status flag for the operation of the routine. Zero (0) signifies
success.  Negative one (-1) signifies failure.


3 ENVIRONMENT

Software platforms:	VICAR 11.0 (VMS/UNIX)
Hardward platforms:	This routine has been tested on 
			DEC Alpha, DEC VAX-VMS, Sun Sparcstation
			(Solaris and SunOS), Silicon Graphics
			(SGI), and HP 700.
			No particular hardware requirements.

3 BACKGROUND

Author:			Lucas Kamp (lwk059@ipl.jpl.nasa.gov)
Cognizant Engineer:	Justin McNeill (jfm059@ipl.jpl.nasa.gov)
Date written:		October 1993
Revision history:	Original
Background and References:	

1. Map Projection Software Set Software Specification Document,
	JPL, April 28, 1993;
2. "Conformal Mapping of the Triaxial Ellipsoid," Survey Review,
	vol. 28, July 1985.
3. MIPS Map Projection Software Users' Guide, D-11810, version 1.0,
	May 1994.
$!-----------------------------------------------------------------------------
$ create mp_get_value_str.hlp
1 MIPS SUBROUTINE mp_get_value_str

PURPOSE

The function mp_get_value_str retrieves character string values
from the map projection data object from FORTRAN application programs.

2 OPERATION

This function allows controlled access to map projection data object 
character string values. 

CALLING SEQUENCE

from C:		(see mpGetValues)

from FORTRAN:	call mp_get_value_str( mp, keyword, value, status )

Necessary include files from calling routine or program: mp_for_defs.fin


INPUT 

mp		(MP type)

A pointer to the MP data structure as returned by mpInit. In FORTRAN, 
this variable is of type REAL*8.  MP type is defined in include file
mp_routines.h.

keyword		(character string)

Character string which contains the keyword name of the MP data object
value to be retrieved.

keyword		(character string)

Character string for the map projection keyword to be set.

OUTPUT

value 		(character string)

Character string value associated with its map projection keyword
retrieved from the map projection data object.

status 		(integer)

Status flag for the operation of the routine. Zero (0) signifies
success.  Negative one (-1) signifies failure.


3 ENVIRONMENT

Software platforms:	VICAR 11.0 
Hardware platforms:	This routine has been tested on 
			DEC Alpha, DEC VAX-VMS, Sun Sparcstation
			(Solaris and SunOS), Silicon Graphics
			(SGI), and HP 700.
			No particular hardware requirements.

3 BACKGROUND

Author:			Lucas Kamp (lwk059@ipl.jpl.nasa.gov)
Cognizant Engineer:	Justin McNeill (jfm059@ipl.jpl.nasa.gov)
Date written:		October 1993
Revision history:	Original
Background and References:	

1. Map Projection Software Set Software Specification Document,
	JPL, April 28, 1993;
2. "Conformal Mapping of the Triaxial Ellipsoid," Survey Review,
	vol. 28, July 1985.
3. MIPS Map Projection Software Users' Guide, D-11810, version 1.0,
	May 1994.
$!-----------------------------------------------------------------------------
$ create mplabelread.hlp
1 MIPS SUBROUTINE mpLabelRead

PURPOSE

The function mpLabelRead reads map projection property labels
of VICAR images and loads an existing map projection data object
with values from the label.

2 OPERATION

This function takes as input a VICAR image file unit number and
the address of a map projection data object and fills the data object
with information from the map projection (MAP) property label. This
function calls mpSetValues to load the data object. 


CALLING SEQUENCE

from C:		status = mpLabelRead( MP *mp, int file_unit );
from FORTRAN:	call mp_label_read( mp,file_unit,status )

INPUT 

file_unit	(integer)

VICAR file unit number as returned by x/zvunit().

mp		(MP type)

A pointer to the MP data structure as returned by mpInit. 
In FORTRAN, this variable is of type REAL*8.  MP type is defined
in include file mp_routines.h.

RETURN

status 		(integer)

Status flag for the operation of the routine.
Zero (0) signifies success and -1 failure.


3 ENVIRONMENT

Software platforms:	VICAR 11.0 (VMS/UNIX)
Hardware platforms:	This routine has been tested on 
			Sun Sparcstation running SunOS, the
			Silicon Graphics, and the VAX 8650.
			No particular hardware requirements.

3 BACKGROUND

Author:			Lucas Kamp (lwk059@ipl.jpl.nasa.gov)
Cognizant Engineer:	Justin McNeill (jfm059@ipl.jpl.nasa.gov)
Date written:		October 1993
Revision history:	Original
Background and References:	

1. Map Projection Software Set Software Specification Document,
	JPL, April 28, 1993;
2. "Conformal Mapping of the Triaxial Ellipsoid," Survey Review,
	vol. 28, July 1985.
3. MIPS Map Projection Software Users' Guide, D-11810, version 1.0,
	May 1994.
$!-----------------------------------------------------------------------------
$ create mplabelwrite.hlp
1 MIPS SUBROUTINE mpLabelWrite

PURPOSE

The function mpLabelWrite writes map projection property 
and history labels of VICAR images using the information 
stored in the map projection data object.

2 OPERATION

This function takes as input a VICAR file unit number and 
a pointer to the map projection data object and writes 
appropriate values to the MAP property or history label
of the VICAR image file.

When a map projection value is not present in the data object, 
the associated value in the label is not written. 
The description of the map projection (MAP_PROJECTION_DESC) 
is predefined text which is written to the label based on the 
map projection type stored in the data object. The label values 
MAXIMUM_LATITUDE, MAXIMUM_LONGITUDE, MINIMUM_LATITUDE, 
MINIMUM_LONGITUDE are calculated and written when they are 
appropriate for the image (e.g. if the pole is visible in the 
image, these values will not be calculated.)

CALLING SEQUENCE

from C:		status = mpLabelWrite( MP mp, int file_unit,
					char label_type );
from FORTRAN:	call mp_label_write( mp, file_unit, label_type )

Necessary include files from calling routine or program: 
		mp_routines.h or mp_for_defs.fin


INPUT 

mp		(MP type)

A pointer to the MP data structure as returned by mpInit.
In FORTRAN, this variable is of type REAL*8.  MP type is defined
in include file mp_routines.h.

file_unit	(integer)

VICAR file unit number as returned by x/zvunit().

label_type	(character array)

Specifies whether the label is written as a VICAR history or property
label.  Valid values are "HISTORY" and "PROPERTY".


RETURN

status 		(integer)

Status flag for the operation of the routine. 
Zero (0) signifies success. Negative one (-1) 
signifies failure.

3 ENVIRONMENT

Software platforms:	VICAR 11.0 (VMS/UNIX)
Hardware platforms:	This routine has been tested on the
			DEC Alpha, DEC VAX-VMS, Sun 
			Sparcstation running SunOS and Solaris,
			Silicon Graphics (SGI), and HP 700.
			No particular hardware requirements.
3 BACKGROUND

Author:			Lucas Kamp (lwk059@ipl.jpl.nasa.gov)
Cognizant Engineer:	Justin McNeill (jfm059@ipl.jpl.nasa.gov)
Date written:		October 1993
Revision history:	(see release_notes.txt in mp_routines.com)
Background and References:	

1. Map Projection Software Set Software Specification Document,
	JPL, April 28, 1993;
2. "Conformal Mapping of the Triaxial Ellipsoid," Survey Review,
	vol. 28, July 1985.
3. MIPS Map Projection Software Users' Guide, D-11810, version 1.0,
	May 1994.
$!-----------------------------------------------------------------------------
$ create mpgetkeywords.hlp
1 MIPS SUBROUTINE mpGetKeywords

PURPOSE

The function mpGetKeywords obtains the names of all keywords 
in the map projection data object for which values have been 
specified.

2 OPERATION

This function checks internally set bits in the MP software 
that specify which valid map projection data object keyword
values have been set by mpSetValues, mp_set_value, and/or
mp_set_value_str.  It compiles a list of these and returns
them to the application program.  Note that not all keywords
are valid for all projections, thus only valid keywords are
returned.

CALLING SEQUENCE

from C:		status = mpGetKeywords( MP mp, char *keywords, 
			int *number_of_keyowrds, int *type, 
			int *class );
from FORTRAN:	call mp_get_keywords( mp,keywords,
			number_of_keywords,type,class,status )

Necessary include files from calling routine or program:
		
	mp_routines.h for C programs
	mp_for_defs.fin for FORTRAN programs

INPUT 

mp			(MP type)

A pointer to the MP data structure as returned by mpInit.
In FORTRAN, this variable is of type REAL*8.  Mp type is defined
in the include file mp_routines.h.

OUTPUT

keywords		(character array)

One dimensional character array which contains all vaild map 
projection keywords that are currently set in the map projection 
data object referenced by the MP argument.

number_of_keywords	(integer)

Pointer to an integer value which gives the number of keyword names
returned by the function.

type			(integer)

Integer array of codes for the data types of the keywords returned.

		1 is character string;
		2 is short integer (INTEGER*2);
		3 is long integer (INTEGER*4);
		4 is real (REAL*4);
		5 is double precision (REAL*8).

class		(integer)

Integer array of the class types of the keywords returned.
Class type of 0 is CORE and 1 is SUPPLEMENTARY.

RETURN

status 		(integer)

Status flag for the operation of the routine. 
Zero (0) signifies success. Negative one (-1) signifies
failure.  Error value  -1002 can also be returned, which 
signifies an internal structure error.


3 ENVIRONMENT

Software platforms:	VICAR 11.0 (VMS/UNIX)
Hardware platforms:	This routine has been tested on
			DEC Alpha, DEC VAX-VMS, Sun Sparcstation 
			(Solaris and SunOS), Silicon Graphics (SGI), 
			and HP-700.
			No particular hardware requirements.

3 BACKGROUND

Author:			Lucas Kamp (lwk059@ipl.jpl.nasa.gov)
Cognizant Engineer:	Justin McNeill (jfm059@ipl.jpl.nasa.gov)
Date written:		October 1993
Revision history:	Original
$!-----------------------------------------------------------------------------
$ create mpsetdebugflag.hlp
1 MIPS SUBROUTINE mpSetDebugFlag 

PURPOSE				

To set a global value to print internal calculations 
of the MP routines to standard output.


2 OPERATION

Sets mp_debug global external variable to TRUE.


CALLING SEQUENCE:		

from C		status = mpSetDebugFlag( int flag );
from FORTRAN	CALL MP_SET_DEBUG_FLAG( FLAG, STATUS ) 


Necessary include files
from calling routine 
or program:			mp_routines.h


INPUT	
	
Flag		(int)

Value to which the mp_debug flag should be set.  
Valid values are TRUE (1) or FALSE (0).


OUTPUT

Status		(int)	

Status flag for routine. Return values are
mpSUCCESS (0)  or mpFAILURE (-1).


3 ENVIRONMENT

Software Platform:		VICAR 11.0
Hardware Platforms:		No particular hardware required.
Programming Language:		ANSI C


3 BACKGROUND

Author:				Justin McNeill, JPL.
Cognizant Programmer:		Justin McNeill, JPL
				(jfm059@ipl.jpl.nasa.gov)
Date:				May 1994
History:			Original
Background and References:	

1. Map Projection Software Set Software Specification Document,
	JPL, April 28, 1993;
2. "Conformal Mapping of the Triaxial Ellipsoid," Survey Review,
	vol. 28, July 1985.
3. MIPS Map Projection Software Users' Guide, D-11810, version 1.0,
	May 1994.
   
$!-----------------------------------------------------------------------------
$ create release_notes.txt
-------------------------------------------------------------------------------

	Release Notes on MP_ROUTINES.COM

-------------------------------------------------------------------------------

	Multimission Image Processing Laboratory (MIPL)
	Jet Propulsion Laboratory
	4800 Oak Grove Drive
	Pasadena, California 91109


	Lastest Update	June 1996	
	
	Author		Justin McNeill
			Mail Stop 168-414
			818-354-7975 (TEL)
			818-393-6962 (FAX)
			Justin.McNeill@jpl.nasa.gov (Email)

			Prabhu Ambatipudi
			Mail Stop 168-514
			818-354-4199 (TEL)
			818-393-6962 (FAX)
			Prabhu.Ambatipudi@jpl.nasa.gov (Email)

			Lucas Kamp
			Mail Stop 168-514
			818-354-4461 (TEL)
			818-393-6962 (FAX)
			Lucas.Kamp@jpl.nasa.gov (Email)

-------------------------------------------------------------------------------

The notes below are ordered by software release dates to the MIPS Development 
System.  Note that not all versions are delivered to the MIPS Operational 
System.  Versions that are delivered to the MIPS Operational System have a 
delivery number, subsystem name, and version number placed in parentheses.

-------------------------------------------------------------------------------


Original version (Delivery 12.0, MGEN V28.7)

	Initial release on November 16, 1993.


Version December 1993 (Delivery 12.0, MGEN V30.2)

	1) 	A few lines in the mpSphere function's sinusoidal projection
		arithmetic were corrected to avoid access violations.
	2)	MP routines global flags changed to mpSUCCESS and 
		mpFAILURE in the mp.h include file.  All references to FAILURE 
		and SUCCESS changed to mpFAILURE and mpSUCCESS, respectively.
	3)	Function mpInit was modified to initialize all MP data object 
		values for added robustness.

	Failure reports closed in this version: 76817.


Version May 1994

	1)	Revised include file mp.h to add a define constant for success 
		status for VICAR RTL calls.  Portions of MP source code revised
 		to use this new define constant.
	2)	Added FORTRAN language bridge mp_get_keywords to the file
		mpgetkeywords.c.
	3)	Added FORTRAN test program tmp_routines_f to testing procedure.
	4)	Made NORMAL_CYLINDRICAL and CYLINDRICAL_EQUAL_AREA equivalent
		projections in the function determine_map_projection_number.
	5)	Corrected help file (.hlp) of mpLabelWrite.
	6)	Created a new public member function mpSetDebugFlag to handle
		automatically the setting of the internal debug flag mp_debug.
		This resolved the issue of FORTRAN language handling of 
		mp_debug.
	7)	LINE_PROJECTION_OFFSET and SAMPLE_PROJECTION_OFFSET
		handling was corrected to add +1 to these values in mpMpo2Buf 
		and -1 to these values in mpBuf2Mpo.
	8)	Changed "REAL" to "DOUB" in calls to zlget in mpLabelRead and
		zladd in mpLabelWrite.
	9)	Changed the max_length argument of sc2for in mp_get_value_str 
		to a value of zero.
	10)	Added BODY_LONG_AXIS as a part of the global structure within 
		the larger MP data object structure in the include file 
		mp_private.h.
	11)	Function prototypes file mp_prototypes.h formalized and MP 
		routines compiled now with the USES_ANSI_C option 
		(see mp_routines.imake).

	Failure reports fixed in this version: 76820, 82916, 83089.


Version June 1994

	1)	Corrected help file (.hlp)  Originally, the help file
		was missing the argument label_type in the sample calling
		sequence and in the argument descriptions.
	2)	Moved all dependencies on the NAIF SPICE library to the
		file mppconstants.c.  This allows programs which do not call
		mpPConstants to link to the MP routines without dependence on
		the SPICE library.
	3)	Tests for function call success or failure were corrected in 
		the following functions: mpLabelRead, mpLabelWrite.  
		Previously, mpLabelWrite and mpLabelRead would fail on 
		correct input.
	4)	Corrected the .imake file for the FORTRAN test program
		tmp_routines_f.  A define for FTNINC_LIST was added to 
		reference mp_for_defs.  Also, the source code line in 
		tmp_routines_f was revised to remove .fin file extension.
	5)	FORTRAN function dsimq of function ztricoef renamed to
		mp_dsimq to avoid symbol conflicts in the P2 sublib on 
		the UNIX machines.
	6)	Added C test program tmp_label to test label reading and 
		writing functions.
	
	Failure reports fixed in this version: 85074, 85078.


Version July 1994

	1)	Simplified FORTRAN routines mp_set_value, mp_set_value_str,
		mp_get_value, mp_get_value_str to first complete FORTRAN
		to C variable conversions and then call the C routines
		mpSetValues and mpGetValues, respectively.
	2) 	mpLabelWrite revised to return mpSUCCESS instead of the
		local variable status as the routine's status flag.
	3)	Incorporated BODY_LONG_AXIS keyword handling into the
		routines mpSetValues and mpGetValues.
	
	Failure reports fixed in this version: 85068, 85090.
	Failure reports for which work is related: 85186.


Version October 1994

	1) 	Released a renamed, duplicate version of mp.h called
		mp_routines.h.  This was released to initiate the 
		process of obsoleting mp.h, which conflicts with
		a system level include file on the UNIX platforms.
	2) 	Include of file "mp_init.h" moved from mpgetkeywords.c
		to mpinit.c.  This avoids a possible uninitialized mp
		data object.
	3)	Euler and Cartesian rotation calculations corrected.
	4)	Inverse mode of Sinusoidal projection corrected.
	5)	Projection descriptions added for all projections.
	6) 	Longitude range checking within formulae revised to
		add or subtract multiples of 360 degrees instead of
		returning failure status values.
	7) 	Mercator and sinusoidal projections for spherical model
		compared successfully against TRANV and hand calculations.

	Failure reports fixed in this version: 82922, 85093, 85094, 
					       85645, 85668.

Version December 1994

	1) 	Cylindrical Equal Area and Equidistant Cylindrical projections
		corrected and verified for spherical body model.
	2) 	Test programs revised to reference mp_routines.h and to use
		ANSI C.
	3) 	mpLabelRead, mpLabelWrite both revised to override error
		handling action used by application programs through ZVEACTION.

	Failure reports fixed in this version: 85665


Version January 1995

	1) 	Corrected calls to zvfilename in test programs.
	2)	Changed call to momati to zmomati in mpMpo2buf.c.

	Failure reports fixed in this version: 85699


Version February 1995

	1)	Updated mp_for_defs.fin to contain all defines that
		are within mp_routines.h.
	2)	Corrected interpretation of Cylindrical Equal Area and
		Simple Cylindrical projection information (added setting
		of FIRST_STANDARD_PARALLEL keywords).
	3)	Map projection descriptions were revised to include a 
		statement about latitude type.

	Failure reports fixed in this version: 85700


Version March 1995

	1) 	Changed mapping in mpBuf2Mpo of Cylindrical Equal-Area
		projection such that XC or SAMPLE_PROJECTION_OFFSET is
		set to 0.0; this required modification of cases in the
		switch statement for most all map projections.  Code
		was modified to use one mail switch statement and return
		status flag set to variable status.  Also, mpBuf2Mpo
		revised to handle the return of map resolution value
		for projections MERCATOR, TRANSVERSE MERCATOR, 
		CYLINDRICAL EQUAL AREA, and SIMPLE CYLINDRICAL, per
		changes to SEARCV3.
	2)	Return status flag checks revised within and after calls
		to mpSphere, mpOblate, and mpEllipsoid for modules 
		mpll2xy and mpxy2ll.  These modifications made to ensure
		lower level error status flags are passed properly to
		the calling program and not simply mapped to mpFAILURE.
	3) 	Corrected argument values passed into ZMOMATI within
		mpMpo2Buf.

	Failure reports fixed in this version: 85803, 85805


Version September 1995

        1)      Modified how function handles model set testing within
		mpxy2ll and mpll2xy in order to allow multiple data
		objects within one application. 
	2)	Revised mp.h to have MAX define in an identical syntax
		to Motif include file's MAX define.

	Failure report fixed in this verion: 87378, 87358 

Version December 1995
	(These changes made by Prabhu Ambatipudi)

	1) 	Corrected Lambert Azimuthal Equal-area projection and
		included unit test of spherical case in test pdf.

	2) 	Added projection formulae numbers from USGS manual
		to MAP_DESC keyword so that users can have a direct
		trace back to mathematics behind the projections.
		Note that USGS Professional Paper 1395, second
		printing 1989 was used as a reference.

	3) 	Revised mp.h and mp_routines.h to include statements
		#ifndef MP_ROUTINES_H, #define MP_ROUTINES_H, and #endif.

	4) 	Revised the implementation of equidistant cylindrical to
		be available for nonspherical bodies, and removed oblique
		simple cylindrical support due to a revision of how the simple
		cylindrical CENTER_LATITUDE and CENTER_LONGITUDE values
		are handled.
	
	Failure reports fixed in this version: 87367, 85758

Version June 1996
	(These changes made by Prabhu Ambatipudi)

	1)	Corrected spherical model formulae for the following 
		projections : Lambert Conformal Conic (both one and two
		parallel cases), Albers Equal Area (one and two parallels
		cases), and Orthographic. Verified line,sample output based
		on input latitudes/longitudes for these projections in 
		testpdf.

	2) 	Included unit tests of spherical case for each projection
		listed above in the main tstmp_routines testpdf.

	3)	Changed the number of precalc_values limit to 6. Changes
		were made in mpinit.c and mp_private.h to accomplish this.

	4) 	Made the Oblique Simple Cylindrical projection case a part
		of the map description set, in mpinternals.c

	5) 	Corrected errors relating to the inclusion of 'PROPERTY' 
		label in functionality of mplabelwrite.

	6)	Included 'FTN_STRING' parameter as part of the mp_routines
		imake file list.  

	7)	Put in calls to 'check_longitude_radians' before references
		to the use of *longitude, in line,sample calculations for
		the projections Mercator, Mollweide, Cylindrical Equal Area.

	8) 	Added in keywords related to perspective projection in all
		relevant files, and added cases of perspective projection
		options in mpinternals.c

	9) 	Implemented functionality of triaxial ellipsoid model for
		authalic projections. This includes reincorporating the
		file 'mp_internals.f' to now contain relevant code for
		authalic auxillary coefficient generation, as well as
		conformal. Also put in appropriate code changes for calls
		to 'triaxcoef' routines, and C-Fortran Bridge interfaces.

	10)	Implemented test case for sinusoidal projection for 
		triaxial ellipsoid case , in the main testpdf.

	11)	Changed the initial value setting for center_latitude and
		center_longitude, in 'mpinit.c' to -999.0 instead of 0.
		This is in case of user not setting these values, but not
		wanting default to zero.

	12)	Assigned B_AXIS_RADIUS parameter to mpSetValues call and
		mpGetValues call in mpbuf2mpo.c and mpmpo2buf.c respectively.
		This inorder to assign it to a buffer value for use in
		call to perspective projection (persp_proj.f)
	           
	13)	Added perspective projection related keywords to pdf_parms
		and pds_keys list, in files tmp_forward.c, tmp_routines.c,
		tmp_routines_verbose.c

	14)	Modified 'ztriaxcoef' call prototype definition in file  
		mp_prototypes.h, to deal with added argument list.
		Added call definition for zpproj_mp() in same file, for
		perspective projection.

	15)	Changed the definition of auxillary lat/lon term 'J'(NLIMIT) 
		in file mpinternals.c. This change is needed in order for 
		computational compatability with 'tricoef' program parameters.
		Also, introduced the term 'K' to define KLIMIT, needed for
		authalic auxillary lat/lon coefficient computation.

	16)	Included code to take care of oblate spheroid and triaxial
		ellipsoid cases of perspective projection, by routing them
		to mpSphere. Perspective is non-conformal/authalic.

	17)	Added C-Fortran bridge for routine zpproj_mp().

	18)	Incorporated program 'persp_proj.f' filename into the module
		list of mp_routines.imake. This contains the routine
		zpproj_mp(), the perspective projection implemented adopted
		from program written by Lucas Kamp.

	19)	Included 'LIB_SPICE' define in tmp_label.imake, to account
		for call to spice library functions in zpproj_mp().

	20)	Updated 'tmp_routines.pdf' to account for perspective 
		projection keywords.

	21)	Implemented functionality of triaxial ellipsoid for authalic
		projections.

	22)	Modified parameter assignment for variables in file 
		'mp_internals.f' (tricoef routine), and changed declaration
		of 'ran1' variable to real*4, based on changes made by J.Lorre
	
	Failure reports fixed in this version: 89294,82967,85758,89152,88207
	Electronic failure reports from DLR Germany addressed in this
		version: D00104,D00024,D00001	

Version July 1996
	(These changes made by Prabhu Ambatipudi)

	23)	For build#16.1, incorporated changes Jean Lorre made in 
		'tricoef.com', into mp_internals.f. These affected the 
		random number generation which in turn affected auxillary
		coefficient generation.

	24) 	Removed naif/spice dependencies in mp_routines, specifically
		as related to persp_proj.f. All naif dependent routine calls
		were replaced with relevant in-line code, for perspective
		projection. 	

Version August 1996
	(These changes made by Prabhu Ambatipudi)

	25) 	Again modified/added code in mp_internals.f, to correct
		problem with tricoef routine on sgi machine. The randum
		number generator had to be changed; changes made by Jean
		Lorre incorporated.

	26)	BODY_LONG_AXIS parameter implementation included with this
		delivery. Thorough testing not complete as of yet, but
		sample test case with it assigned in the parameter list is
		included in tstmp_routines.pdf

	27)	In persp_proj.f, input line/sample when both are positive
		in mpinternals, was being input as negative to this program.
		Thus value of line negated after call to Cartestian_
		translation routine in mpinternals.
   
	This delivery did not address any specific FR numbers, but had to
	be delivered to accomodate other programs dependent on mp_routines.

        28)	Redelivered module with a fix to DLR electronic FR D00113
		The 'addressing' of authalic_latitude and authalic_longitude
		were removed from call to ztriaxtran in reverse calculation
		portion (LINE_SAMPLE_TO_LAT_LON) of authalic projections
		case in 'mpEllipsoid', to correct a format number problem
		occurring in triaxtran.com interface.

	29)	Added another test case for stereographic projection  
		(conformal example) and triaxial body model. Also changed
		test case program from 'tmp_forward' to 'tmp_routines' for
		triaxial test case dealing with sinusoidal projection. 

Version October 1996
	(These changes were made by Lucas Kamp and Frank Scholten)

	1) 	The new keyword COORDINATE_SYSTEM_NAME was added to the 
		Global group.

	2)	+1 was added to L/S offsets in Cartesian transformations.

	3)	Ensure that CHECKif does not replace an MP error 
		(status < -1000) with simple failure to project (status = -1).

	4) 	For perspective projection, replaced map_scale by appropriate 
		formula in mpinternals.c.  (Note:  an alternative would be 
		to add map_scale to the Global group).  Also: changed E.lon 
		to West before calling PPROJ;  return immediately when PPROJ 
		returns 1;  don't call perform_precalcs for this case;
		removed confusing comments and commented-out code pertaining
		to USGS "Vertical Perspective" equations;  added reference to 
		mpGetDesc for this case 

	5)	Albers:  fixed algorithm when n<0

	6)	Lambert:  fixed algorithm when n<0 and at Poles;
		(two_par only:) fixed problem in longitude -> sample 
		computation.

	7)	Enforced check on longitude range (-180 -> 180) in inverse mode
		for several projections.

	8)	Added Orthographic projection to mpOblate using ORTHO_OBL.F,
		revised mpGetDesc accordingly

	9)	Fixes to Orthographic & Stereographic for rho=0 and in 
		checking limits

	10)	Fixed errors in precalcs for Lambert one_par and in
		planetodetic_authalic_trans

Version November 1996
	(These changes made by Prabhu Ambatipudi)

	1)	Added three more tests in the main tstpdf (tstmp_routines.pdf)
		that deal with oblate spheroid testing. The projections 
                tested in tests 24,25,and 26 are Cylindrical Equal Area,
		Lambert Azimuthal equal area, and Stereographic. Values used
 		in testing obtained from USGS Snyder manual.
	  
	2)	Modified mpSetValues call in mppconstants.c , for parameter
		LONG_AXIS, to value[0], to read in the proper value from
		the SPICE kernel file. 

Version March 1997
	(Changes were made by Frank Scholten and implemented by Lucas Kamp)

	1)  mp_load_EULER_rotation_matrix was revised to use the auxiliary
	    (authalic/conformal) latitude instead of planetodetic.

	2)  code was added to load_coefficients to calculate 
	    precalc.oblate_value's.

	3)  mpMAX_DESCRIPTION_LINES was increased from 20 to 30, to allow
	    for the LAMBERT_AZIMUTHAL case.

	4)  checks for latitudes very close to 90 degrees were added to
	    mpMpo2Buf and to ORTHO_OBL.

	5)  ERR_ACT was disabled in the zlget calls to History in mpLabelRead.

	6)  COORDINATE_SYSTEM_NAME = PLANETODETIC was made the default in
	    mpLabelRead.

Version September 1997
	(Changes were made by Bob Deen)

	1)  Changed FORTRAN calling standard for the MP object to be
	    REAL*8 instead of just INTEGER.  This was necessary for
	    compatibility with 64-bit operating systems, because the MP
	    object ID is used internally as a pointer.  Only 4 bytes
	    are used on 32-bit systems, but since FORTRAN has no typedef
	    capability, all must use REAL*8.

Version March 1997
	(Changes were made by Frank Scholten and Lucas Kamp)

	1)  Corrected transformation_direction in mp_load_Euler...
	    before conformat/authalic transformation

	2)  Added a check for longitude range to code for Mollweide
	    in mpSphere

	3)  Changed SPACECRAFT_DISTANCE to TARGET_CENTER_DISTANCE,
	    added MINIMUM/MAXIMUM LATITUDE/LONGITUDE

Version June 1997
	(Changes were made by Lucas Kamp and Bob Deen)

	1)  Replaced function RANGEN() in triaxcoef() with RAN1(), because
	    it caused problems on DEC Unix systems.  RAN1 was modified to
	    fix previous problems encountered on the SGI.

	2)  Initialized buffer in mplabelread().

	3)  Removed ztime() from prototypes and mp_time() from mpinternals,
	    as neither is used anywhere.

	4)  Fixed bug in mpSphere for Stereographic case when rho=0.0.

	6)  Fixed planetodetic case for Equidistant Cylindrical.

	7)  Replaced Galileo-specific SPICE call zbodn2c_g (with zbodn2c).

	8)  Suppressed messages from triaxcoef() unless the debug flag
	    is specified.

	9)  Changed prototypes for use with C++;  changed mpGetKeywords
	    calling sequence in consequence.

Version Sept. 1998
	(Changes made by Matt McNeely))

	Moved mppconstants.c from mp_routines.com to mp_pconstants.com, to 
	eliminate SPICE dependencies;  mp_routines.com moved to P1 while
	mp_pconstants.com remained in P2

Version Nov. - Dec. 1999

	(Changes made by Thomas Roatsch:)

	1. mp_routines.com was broken into three parts:
	 - mp_routines contains only the routines which are necessary for
	  the mp initialization and calculations
	 - mp_labels contains the label I/O routines and mpbuf2mpo.c
	  (but mpmpo2buf.c is still in mp_routines, because it is called
	  in mpinternals);  this com-file also contains all test scripts,
	  docs, etc.
	 - mp_bridges contains the Fortran bridges to the C-routines
	  (both in mp_routines and mp_labels)

	2. call abend in mp_internals was changed to status -401,-402, -403

	3. The statement:	
	 #define mpMAX_DESCRIPTION_LINES 30
	was moved from mp_private to mp_routines

	4. The unused routines copy_coef and zget_ellipsoid were removed.

	5. Code was added to mpsetvalues to automatically compute
	map_scale and map_resolution from each other.

	(Changes made by Lucas Kamp:)

	6. mpgetvalues was revised to remove code supplying scale or
	resolution if only the other was supplied

	7. tstmp_routines.pdf was revised to be consistent with change #5

	8. tmp_buf2mpo was added to test procedure

	9. mpbuf2mpo was revised to remove call to set map_resolution, as
	this was causing an error for some projections due to above changes

	10. some synonyms for projection names were added to mpmpo2buf


Version July 2000
	(Changes were made by Lucas Kamp)

	1. "target_body", which is not a PDS standard, was changed to 
	"target_name" in all instances (both internally in the code and 
	in the external interface), but target_body was retained as an 
	alias for target_name, for compatibility with old versions.  
	This required changes to mpgetvalues.c, mpsetvalues.c, mp_init.h, 
	and mp_private.h (all in mp_routines.com), mpgetpar.com, 
	mp_routines.h, and mp_for_defs.fin.

	2. "planetodetic", which is not a PDS standard, was changed to 
	"planetographic" in the external interface (only), and code was 
	added to mpgetvalues.c to check to ensure that the values of 
	coordinate_system_name are valid, and to change the value
	"planetodetic" (which is still acceptable as an alias) to 
	"planetographic".

	3. Code was added to mpgetvalues.c to check to ensure that the 
	values of positive_longitude_direction are valid (East/West).
	A new error code, mpINVALID_LONG_DIRECTION, was added to 
	mp_routines.h to be used there.

	4. Code was added to mpll2xy.c and mpxy2ll.c to check that the
	variable defining the coordinate system falls in the valid range.
	A new error code (mpINVALID_COORD_SYSTEM) was added to 
	mp_routines.h to be used there and in mpgetvalues.c.

	5. In mp_internals.f, the common block named c1 was changed to
	triaxcoef_common, to avoid conflicts with other Vicar subroutine 
	common blocks on the Solaris (after upgrade to a new compiler?).
	Code was also added to initialize this common block and to save
	variables in the function ran1() called there, in order to fix 
	problems encountered on the SGI.


Version October 2000
	(Changes were made by Lucas Kamp)

	mpll2xy and mpxy2ll were revised so as not to transform between
	planetographic and planetocentric coordinates for the case of
	Equidistant projections, since the equations for this case are 
	independent of those systems.

Version July 2001

	1. (Change made by Thomas Roatsch)
	Replaced all remaining Fortran code by C.

	(Changes made by Lucas Kamp:)

	2. Fixed a bug in persp_proj_c.c (had been in persp_proj.f since
	1996).

	3. Added back switch mp_debug and related code (except in triaxtran),
	removed sub 1.

	4. Removed spurious check that A/B/C radii must be in descending
	order from triaxcoef (& added a test of this to tst_mp....pdf)

	5. Forced output coordinate_system_name to be planetodetic 
	instead of -graphic in mpgetvalues, per change of July 2000.  
	(This had been reversed sub 1.)

-----------------------------------------------------------------------------

	End of Release Notes

-----------------------------------------------------------------------------
$ Return
$!#############################################################################
