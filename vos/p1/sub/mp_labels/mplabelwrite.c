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
