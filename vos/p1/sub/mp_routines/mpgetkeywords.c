				/*					*/
				/* INCLUDE FILES NEEDED FOR ROUTINES 	*/
				/*					*/
#include <math.h>		/* Standard math library include	*/
#include <stdio.h>		/* Standard C I/O Include File		*/
#include <stdlib.h>		/* C Memory Management Include File	*/
#include <string.h>
#include <time.h>
#include "mp_private.h"		/* Private Map Projection Include File	*/
#include "mp_prototypes.h"

/********************************************************************/
	/*
	 * Function mp_get_keywords:
	 *
	 * routine to return a list of MP keywords whose values have
	 * been set (by previous calls to mp_set_value);  if the
	 * projection type has been set, then only keywords that are
	 * valid for that type are returned
	 *
	 * The FORTRAN calling sequence is:
	 *
	 *	INCLUDE MP_FOR_DEFS
	 *	INTEGER MP, NUM, STATUS
	 *	INTEGER KTYPE(MP_NUM_KEYWDS), KCLASS(MP_NUM_KEYWDS)
	 *	CHARACTER*MP_MAX_KEY_LEN KEYWDS(MP_NUM_KEYWDS)
	 *	....
	 *	CALL MP_GET_KEYWORDS( MP, KEYWDS, NUM, KTYPE, KCLASS, STATUS)
	 *
	 * (Note that the arrays returned by MP_GET_KEYWORDS must be declared
	 * using their maximum length, as given in MP_FOR_DEFS.FIN.)
	 *
	 *  3sep93  -lwk-  intial version
         * 09mar94  -lwk-  changed declaration of mp from (MP_STRUCTURE *)
         *              to MP, since private version not needed here
	 * 18may94  -jfm-  added mp_get_keywords to mpgetkeywords.c
         * 22aug98  -lwk-  changes for revised mpGetKeywords calling sequence
	 */

/********************************************************************/
	/*
	 * Function mpGetKeywords:
	 *
	 * routine to return a list of MP keywords whose values have
	 * been set (by previous calls to mpSetValues);  if the
	 * projection type has been set, then only keywords that are
	 * valid for that type are returned
	 *
	 * It is the reponsibility of the calling program to allocate
	 * memory to hold the arrays returned.  If the program is
	 * called with no allocation for the keyword array, then this
	 * routine only returns the number of keywords (num_kwd).
	 * The user can then allocate and make a second call.
	 * Alternatively, the user can allocate all arrays to their
	 * maximum length, which is mpNUMBER_OF_KEYWORDS (in mp_public.h).
	 *
	 * this is the C-callable version;  it should be maintained
	 * in parallel with the Fortran-callable mp_get_keywords
	 *
	 *   3aug93  -lwk-  intial version
	 *  23aug93  -lwk-  added restriction that returned keywords be
	 *		  valid for the projection
	 *   3sep93  -lwk-  remove mallocs from this routine;  allow
	 *		  mode in which only num_kwd is returned
         * 22aug98  -lwk-  changes for revised mpGetKeywords calling sequence
	 *
	 * FOR FURTHER CHANGES SEE RELEASE_NOTES.TXT
	 */

int mpGetKeywords(
  MP mp,			/* in:  user's MP structure id */
  char keywds[mpNUMBER_OF_KEYWORDS][mpMAX_KEYWD_LENGTH+1],
				/* out:  array of keywords set */
  int *num_kwd,                 /* out: outer dimension of keywds */
  int *kwd_type,		/* out: array of types, of length num_kwd */
				/*      (type = CHAR, DOUBLE, etc.) */
  int *kwd_class)		/* out: array of classes, of length num_kwd */
				/*      (class = CORE or SUPPLEMENTARY) */
{
  int grp, i, kvmask, kvalid, mset, n;
  struct MP_STRUCTURE *pmp;
  extern struct mpKEYWORD_TO_CODE mpKwds_Codes[];
  extern int mpMAP_PROJ_BITS[];
  extern char mpMAP_PROJ_TYPES[mpNUMBER_OF_PROJECTIONS][mpMAX_KEYWD_LENGTH];

  pmp = (struct MP_STRUCTURE *) mp;

	/* if map projection is set, find the valid keywords mask: */
  if (pmp->kwd_set[0] & mpMAP_PROJECTION_TYPE_BIT) {
    mset = 1;
    for (i=0, kvmask = -1; i<mpNUMBER_OF_PROJECTIONS; i++) {
      if ( EQUAL( mpMAP_PROJ_TYPES[i],  pmp->glob.map_projection_type) ) {
        kvmask = mpMAP_PROJ_BITS[i];
	break;
      }
    }
    if (kvmask == -1) return mpINVALID_PROJECTION;
  }
  else mset = 0;
  for (i=0, n=0; i<mpNUMBER_OF_KEYWORDS ;i++) { 
    grp = mpKwds_Codes[i].group_no;
    kvalid = 1;
    if (mset) {
      if (grp==1 && (kvmask & mpKwds_Codes[i].bit_code) == 0)
	kvalid = 0;
      if (grp==2 && strcmp( pmp->glob.map_projection_type, mpPOINT_PERSPECTIVE))
	kvalid = 0;
    }
    if ( (pmp->kwd_set[grp] & mpKwds_Codes[i].bit_code) && kvalid ) {
      if (keywds) {
	strcpy( keywds[n], mpKwds_Codes[i].label_kwd );
	*(kwd_type+n) = mpKwds_Codes[i].kwd_type;
	if (grp <= 2) *(kwd_class+n) = mpCORE;
	else *(kwd_class+n) = mpSUPPL;
      }
      n++;
    }
  }
  *num_kwd = n;

  return mpSUCCESS;
}
