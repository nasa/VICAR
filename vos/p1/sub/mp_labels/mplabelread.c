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
