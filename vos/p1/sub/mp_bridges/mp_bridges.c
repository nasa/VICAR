#include "xvmaininc.h"
#include "ftnbridge.h"		/* FORTRAN bridge Include FIle 		*/
#include "mp_routines.h"

/*****************************************************************/

void FTN_NAME2_(mp_init, MP_INIT) (MP *mp, int *fstat )
{

*fstat = mpInit( mp );

}

/*****************************************************************/

int FTN_NAME2_(mp_free, MP_FREE) ( MP *mp )
{

mpFree( *mp );

return mpSUCCESS;
}

/*****************************************************************/

void FTN_NAME2_(mp_get_keywords, MP_GET_KEYWORDS) ( MP *mp,
	char fkeywds[mpNUMBER_OF_KEYWORDS][mpMAX_KEYWD_LENGTH+1],
	int *num_kwd, int *kwd_type, int *kwd_class, int *fstat,
	ZFORSTR_PARAM)
#if 0
MP *mp;				/* in:  user's MP structure id */
char fkeywds[mpNUMBER_OF_KEYWORDS][mpMAX_KEYWD_LENGTH+1];
				/* out:  array of keywords set */
int *num_kwd;  			/* out: outer dimension of keywds */
int *kwd_type;			/* out: array of types, of length num_kwd */
				/*      (type = CHAR, DOUBLE, etc.) */
int *kwd_class;			/* out: array of classes, of length num_kwd */
				/*      (class = CORE or SUPPLEMENTARY) */
int *fstat;			/* out: Fortran status */
#endif
{
  ZFORSTR_BLOCK
  int i, status;
  int types[mpNUMBER_OF_KEYWORDS], classes[mpNUMBER_OF_KEYWORDS];
  char keywds[mpNUMBER_OF_KEYWORDS][mpMAX_KEYWD_LENGTH+1];

  status = mpGetKeywords( *mp, keywds, num_kwd, types, classes);
  if (status != mpSUCCESS) 
   	*fstat = status;
  else
	{
  	for (i=0; i<*num_kwd; i++) {
    	kwd_type[i] = types[i];
    	kwd_class[i] = classes[i];
  	}

	/* convert the string array to FORTRAN format: */
  	i = 0;
  	zsc2for_array( (char *)keywds, mpMAX_KEYWD_LENGTH+1, *num_kwd,
			(char *)fkeywds, &i, &mp, 6, 2, 1, fstat);

	*fstat = mpSUCCESS;
	}

}

/*****************************************************************/

	/*
	 * Function mp_get_value:
	 *
	 * FORTRAN version of mpGetValues for double precision values.
	 *
	 *  1aug93  -lwk-  intial version
	 *  july94  -jfm-  simplified to call C version mpGetValues.
	 *
	 */

void FTN_NAME2_(mp_get_value, MP_GET_VALUE) ( MP *mp,
		char *fkey, double *val, int *fstat, ZFORSTR_PARAM)
{
  ZFORSTR_BLOCK
  char key[41];

        /* convert the FORTRAN string to C: */
  zsfor2c( key, 40, fkey, &mp, 4, 2, 1, fstat);

  *fstat = mpGetValues( *mp, key, val, NULL );
}

/*****************************************************************/

	/*
	 * Function mp_get_value_str:
	 *
	 * FORTRAN version of mpGetValues for string values.
 	 *
	 *  2aug93  -lwk-  intial version
	 *  july94  -jfm-  simplified to call C version mpGetValues
	 *
	 */

void FTN_NAME2_(mp_get_value_str, MP_GET_VALUE_STR) ( MP *mp,
		char *fkey, char *fval, int *fstat, ZFORSTR_PARAM)
{
  ZFORSTR_BLOCK
  char key[mpMAX_KEYWD_LENGTH+1], val[mpMAX_KEYWD_LENGTH+1];

        /* convert the FORTRAN string to C: */
  zsfor2c( key, mpMAX_KEYWD_LENGTH, fkey, &mp, 4, 2, 1, fstat);

  *fstat = mpGetValues( *mp, key, val, NULL );
  if ( *fstat > mpFAILURE )
	/* convert the string to FORTRAN format: */
	zsc2for( val, 0, fval, &mp, 4, 3, 2, fstat);
}

/*****************************************************************/

void FTN_NAME2_(mp_ll2xy, MP_LL2XY) ( MP *mp, double *y, double *x,
		double *lat, double *lon, int *ll_type, int *status)
{
*status = mpll2xy( *mp, y, x, *lat, *lon, *ll_type );
}

/*****************************************************************/

void FTN_NAME2_(mp_set_debug_flag, MP_SET_DEBUG_FLAG) ( int *flag, int *status)
{
*status = mpSetDebugFlag( *flag );
}

/*****************************************************************/

	/*
	 * Function mp_set_value:
	 *
	 * routine to allow user to set a value in the MP buffer
	 * for a given keyword;
	 * this is one of two FORTRAN-callable versions, one for string
	 * values, one for numerical;  it should be maintained in parallel
	 * with the C-callable version mpSetValue() 
	 *
         * The FORTRAN calling sequence is:
         *
         *      INTEGER MP, STATUS
         *      CHARACTER*32 KEYWD
         *      DOUBLE PRECISION VALUE
         *      ....
         *      CALL MP_SET_VALUE( MP, KEYWD, VALUE, STATUS)
         *
	 *  1aug93  -lwk-  intial version
	 *  july94  -jfm-  change routine to call C version mpSetValues.
	 *
	 */
void FTN_NAME2_(mp_set_value, MP_SET_VALUE) ( MP *mp, char *fkey, double *val,
		int *fstat, ZFORSTR_PARAM)
{
  ZFORSTR_BLOCK
  char key[31];

	/* convert the FORTRAN string to C: */
  zsfor2c( key, 30, fkey, &mp, 4, 2, 1, fstat );

  *fstat = mpSetValues( *mp, key, *val, NULL );
}

/*****************************************************************/

	/*
	 * Function mp_set_value_str:
	 *
	 * routine to allow user to set a string value in the MP buffer
	 * for a given keyword;
	 * this is one of two FORTRAN-callable versions, one for string
	 * values, one for numerical;  it should be maintained in parallel
	 * with the C-callable version mpSetValue() 
	 *
         * The FORTRAN calling sequence is:
         *
         *      INTEGER MP, STATUS
         *      CHARACTER*32 KEYWD
         *      CHARACTER*32 VALUE
         *      ....
         *      CALL MP_SET_VALUE_STR( MP, KEYWD, VALUE, STATUS)
         *
	 *  2aug93  -lwk-  intial version
	 *  july94  -jfm-  change routine to call C version mpSetValues.
	 *
	 */

void FTN_NAME2_(mp_set_value_str, MP_SET_VALUE_STR) ( MP *mp, char *fkey,
		char *fval, int *fstat, ZFORSTR_PARAM)
{
  ZFORSTR_BLOCK
  char key[mpMAX_KEYWD_LENGTH+1], val[mpMAX_KEYWD_LENGTH+1];

	/* convert the FORTRAN strings to C: */
  zsfor2c( key, mpMAX_KEYWD_LENGTH, fkey, &mp, 4, 2, 1, fstat);
  zsfor2c( val, mpMAX_KEYWD_LENGTH, fval, &mp, 4, 3, 2, fstat);

  *fstat = mpSetValues( *mp, key, val, NULL );
}

/*****************************************************************/


void FTN_NAME2_(mp_xy2ll, MP_XY2LL) ( MP *mp, double *y, double *x,
		double *lat, double *lon, int *ll_type, int *status )
{
*status = mpxy2ll( *mp, *y, *x, lat, lon, *ll_type );
}

/*****************************************************************/

void FTN_NAME2_(mp_label_read, MP_LABEL_READ)
( 
  MP *mp,				/* IN:  user's MP structure */
  int *unit,				/* IN:  VICAR file unit number */
  int *fstat				/* OUT:  Fortran status return */
)
{
  *fstat = mpLabelRead( *mp, *unit);
}

/*****************************************************************/

void FTN_NAME2_(mp_label_write, MP_LABEL_WRITE) ( MP *mp, int *unit,
		char *flabtyp, int *fstat, ZFORSTR_PARAM)
#if 0
MP *mp;					/* IN:  user's MP structure */
int *unit;				/* IN:  VICAR file unit number */
char *flabtyp;				/* IN:  "HISTORY" or "PROPERTY" */
int *fstat;				/* OUT:  Fortran status return */
#endif
{
  ZFORSTR_BLOCK
  char labtype[9];

	/* convert the Fortran string to C: */
  zsfor2c( labtype, 8, flabtyp, &mp, 4, 3, 1, fstat);

	/* and call the C routine: */
  *fstat = mpLabelWrite( *mp, *unit, labtype);
}

/*****************************************************************/

void FTN_NAME2_(mp_buf2mpo, MP_BUF2MPO) ( void *buf, MP *mp, int *fstat)
{
  *fstat = mpBuf2Mpo( buf, *mp);
}

/*****************************************************************/

void FTN_NAME2_(mp_mpo2buf, MP_MPO2BUF) ( MP *mp, void *buf, int *fstat)
{
  *fstat = mpMpo2Buf( *mp, buf);
}


