$!****************************************************************************
$!
$! Build proc for MIPL module mp_bridges
$! VPACK Version 1.9, Monday, December 07, 2009, 15:58:16
$!
$! Execute by entering:		$ @mp_bridges
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
$ write sys$output "*** module mp_bridges ***"
$!
$ Create_Source = ""
$ Create_Repack =""
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
$ if primary .eqs. "IMAKE" then Create_Imake = "Y"
$ if (f$locate("CLEAN", primary) .eqs. 0) then Do_Make = "Y"
$!
$ if (Create_Source .or. Create_Repack .or. Create_Imake .or. Do_Make) -
        then goto Parameter_Okay
$ write sys$output "Invalid argument given to mp_bridges.com file -- ", primary
$ write sys$output "For a list of valid arguments, please see the header of"
$ write sys$output "of this .com file."
$ exit
$!
$Parameter_Okay:
$ if Create_Repack then gosub Repack_File
$ if Create_Source then gosub Source_File
$ if Create_Imake then gosub Imake_File
$ if Do_Make then gosub Run_Make_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_Source = "Y"
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
$   if F$SEARCH("mp_bridges.imake") .nes. ""
$   then
$      vimake mp_bridges
$      purge mp_bridges.bld
$   else
$      if F$SEARCH("mp_bridges.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake mp_bridges
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @mp_bridges.bld "STD"
$   else
$      @mp_bridges.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create mp_bridges.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack mp_bridges.com -mixed -
	-s mp_bridges.c -
	-i mp_bridges.imake
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create mp_bridges.c
$ DECK/DOLLARS="$ VOKAGLEVE"
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


$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create mp_bridges.imake
#define SUBROUTINE  	mp_bridges

#define MODULE_LIST 	mp_bridges.c

#define P1_SUBLIB

#define FTN_STRING	
#define USES_ANSI_C
$ Return
$!#############################################################################
