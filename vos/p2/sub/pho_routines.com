$!****************************************************************************
$!
$! Build proc for MIPL module pho_routines
$! VPACK Version 1.9, Monday, December 07, 2009, 16:31:36
$!
$! Execute by entering:		$ @pho_routines
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
$ write sys$output "*** module pho_routines ***"
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
$ write sys$output "Invalid argument given to pho_routines.com file -- ", primary
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
$   if F$SEARCH("pho_routines.imake") .nes. ""
$   then
$      vimake pho_routines
$      purge pho_routines.bld
$   else
$      if F$SEARCH("pho_routines.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake pho_routines
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @pho_routines.bld "STD"
$   else
$      @pho_routines.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create pho_routines.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack pho_routines.com -mixed -
	-s pho_routines.c pho_bidirefs.c pho_routinesf.f pho_private.h -
	   pho_init.h -
	-i pho_routines.imake -
	-o pho_free.hlp pho_get_func.hlp pho_get_keys.hlp pho_get_parms.hlp -
	   pho_get_val.hlp pho_init.hlp pho_set_func.hlp pho_set_val.hlp -
	   pho_correct.hlp pho_func.hlp pho_bidi_ref.hlp -
	-t tzpho_routines.c tzpho_routines.imake tzpho_routines.pdf -
	   txpho_routines.f txpho_routines.imake txpho_routines.pdf -
	   tstpho_routines.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create pho_routines.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include <stdio.h>		/* Standard C I/O Include File		    */
#include <string.h>		/* efficient operations on memory areas     */

#include "xvmaininc.h"		/* Standard VICAR Include File		    */
#include "errdefs.h"		/* VICAR Standard error actions		    */
#include "ftnbridge.h"		/* VICAR-FORTRAN bridge Include File 	    */

#include "pho_private.h"	/* definitions of the pho_object and ...    */
#include "pho_init.h"		/* areas of keywords, ...		    */
#include <stdlib.h>

/*************************************************************************

				phoFree

**************************************************************************
	 *
	 * To release memory which was previously allocated by phoInit  
	 * for a photometric function data object.
	 *
	 * NECESSARY INCLUDE files for calling routine or program: 
	 * 	pho.h (for C routines )
	 *	pho.fin (for FORTRAN routines )
	 *
	 * CALLING SEQUENCE:		
	 *
	 * calling from C		include "pho.h"
	 *				PHO PHO_DATA;
	 *				int status;
	 *				...
	 *				status = phoFree( PHO_DATA );
	 *
	 * calling from FORTRAN		INCLUDE 'pho'
	 *				INTEGER PHO_DATA, STATUS
	 * 				...
	 *				call pho_free( PHO_DATA, status ) 
	 *
	 *
	 * PARAMETERS:
	 *
	 * PHO_DATA	PHO_STRUCT	(Input)	Address of photometric 
	 *					function data object
	 *
	 * status	int		(Output) Error status:
	 *					   phoSUCCESS - success
	 *
	 * HISTORY : Jan. '94  - F. Oschuetz, DLR -  intial version
	 *
	 * 
	 *
*************************************************************************/

/*************************************************************************

FORTRAN Callable Version	pho_free

**************************************************************************/

int FTN_NAME2_(pho_free, PHO_FREE) ( pho, status )
PHO_STRUCT **pho;
int	 *status;
{

free( *pho );

*status = phoSUCCESS;

return phoSUCCESS;

}

/*************************************************************************

C Callable Version		phoFree

*************************************************************************/

int phoFree( PHO pho_obj ) 

{

/* Deallocate memory for pointer to structure PHO_OBJECT		*/

free( pho_obj );

return phoSUCCESS;

}


/*************************************************************************

				phoGetFunc

**************************************************************************
	 *
	 * routine to allow user to retrieve the photometric function name
	 * from the PHO data object;  a separate call, phoGetVal, is
	 * used to retrieve parameter values.
	 *
	 * Necessary include files for calling routine or program: 
	 * 	pho.h (for C routines )
	 * 	pho.fin (for FORTRAN routines )
	 *
	 * Other requirements: 	need to run routines phoInit, phoSetFunc 
	 *			first
	 *
	 * Necessary include files from calling routine or program: 
	 * 		pho.h (in C routines) 
	 *		pho.fin ( in FORTRAN routines)
	 *
	 * Calling Sequence:		
	 *
	 * calling from C:	include "pho.h"
	 *			PHO PHO_DATA;
	 *			char PHO_FUNC[phoMAX_FUNC_NAME_LENGTH+1];
	 *			int status;
	 *			...
	 *			status = phoGetFunc( PHO_DATA, PHO_FUNC); 
	 *
	 * calling from FORTRAN:
	 *			INCLUDE 'pho'
	 *			CHARACTER*(PHO_MAX_FUNC_NAME_LENGTH) PHO_FUNC
	 *			INTEGER PHO_DATA, status
	 *			...
	 *			call pho_get_func( PHO_DATA, PHO_FUNC, status )
	 *
	 * where:
	 *  PHO_STRUCT *pho	(input)  is the PHO object being referenced
	 *  char *func_name	(output) is the name of the phot.function
	 *  int status		(Output) Error status:
	 *				   phoSUCCESS - success
	 *				   phoFUNC_NOT_SET - failed: 
	 *					photometric function not 
	 *					set in the pho_object
	 *				   phoInvalid_Function - failed :
	 *					invalid photometric function 
	 *					name has been passed
	 *
	 * History : 05jan94  -lwk-  intial version
	 * 
	 *
*************************************************************************/

/*************************************************************************

FORTRAN Callable Version	pho_get_func

**************************************************************************/

int FTN_NAME2_(pho_get_func, PHO_GET_FUNC) ( PHO_STRUCT **pho,
				char *func_name, int *fstat, ZFORSTR_PARAM)
{
  ZFORSTR_BLOCK
  char f_nam[phoMAX_FUNC_NAME_LENGTH+1];

	/* call the C-callable version: */

  *fstat = phoGetFunc( *pho, f_nam);

  if (*fstat != phoSUCCESS) return *fstat;

	/* convert the C string to FORTRAN: */
/*  zsc2for( f_nam, phoMAX_FUNC_NAME_LENGTH, func_name, &pho, 3, 2, 1, fstat);*/
  zsc2for( f_nam, 0, func_name, &pho, 3, 2, 1, fstat);

  return phoSUCCESS;
}

/*************************************************************************

C Callable Version		phoGetFunc

*************************************************************************/

int phoGetFunc( PHO_STRUCT *pho,
	char func_name[phoMAX_FUNC_NAME_LENGTH+1])
{
  int i, fcn;

	/* check if function has been set: */

  if (EQUAL( pho->func_name, "")) return phoFUNC_NOT_SET;

	/* check that this is a valid function name: */

  for (i=0, fcn = -1; i<phoFUNCTION_COUNT && fcn==-1; i++)
    if (EQUAL( phoFUNC_NAMES[i], pho->func_name)) fcn = i;
  if (fcn == -1) return phoINVALID_FUNCTION;

	/* get the function name: */

  strcpy( func_name, pho->func_name);

  return phoSUCCESS;
}

/********************************************************************/
	/*
	 * Function phoGetKeys:
	 *
	 * routine to return a list of all PHO keywords that are
	 * valid for the current photometric function, which must
	 * already have been set by a call to phoSetFunc.
	 *
	 * It is the reponsibility of the calling program to allocate
	 * memory to hold the arrays returned.  If the program is
	 * called with no allocation for the keyword array, then this
	 * routine only returns the number of keywords (num_kwd).
	 * The user can then allocate and make a second call.
	 * Alternatively, the user can allocate the arrays to its
	 * maximum length, which is phoMAX_PARAM_PER_FUNC *
	 * (phoMAX_KEYWD_LENGTH+1) (these are defined in pho.h).
	 *
	 * this is the C-callable version;  it should be maintained
	 * in parallel with the Fortran-callable pho_get_keys
	 *
	 * the calling sequence is:
	 *
	 *  int phoGetKeys( *pho, *keywds, *num_kwd);
	 *
	 * arguments:
	 *  PHO_STRUCT *pho (in)  = user's PHO structure id 
	 *  char *keywds (out) =  array of valid keywords
	 *  int *num_kwd (out) = outer dimension of keywds array
	 *
	 *  05jan94  -lwk-  intial version
	 */
/*
/********************************************************************

int phoGetKeys( 
  PHO_STRUCT *pho,
  char keywds[phoMAX_PARAM_PER_FUNC][phoMAX_KEYWD_LENGTH+1],
  int *num_kwd)
{
  int i, j, fcn;
*/
	/* first determine the photometric function: */

/*  for (i=0, fcn = -1; i<phoFUNCTION_COUNT && fcn==-1; i++)
    if (EQUAL( phoFUNC_NAMES[i], pho->func_name)) fcn = i;
  if (fcn == -1) return phoFUNC_NOT_SET;
*/
	/* copy number & all valid names for this function: */

/*  *num_kwd = phoPARAM_COUNT[fcn];

    for (i=0; i < *num_kwd;  i++)
      {
      j = phoFUNC_PARAM_CODES[fcn][i];
      strcpy( keywds[i], phoPARAM_KEYWORDS[j]);
      };

  return phoSUCCESS;
}
*/

/* if no memory for the keywords it returnes the number of keywords */ 

/*************************************************************************

C Callable Version		phoGetKeys

*************************************************************************/

int phoGetKeys( 
  PHO_STRUCT *pho,
  char *keywds,
  int *num_kwd)
{
  int i, j, fcn;

	/* first determine the photometric function: */

  for (i=0, fcn = -1; i<phoFUNCTION_COUNT && fcn==-1; i++)
    if (EQUAL( phoFUNC_NAMES[i], pho->func_name)) fcn = i;
  if (fcn == -1) return phoFUNC_NOT_SET;

	/* copy number & all valid names for this function: */

  *num_kwd = phoPARAM_COUNT[fcn];

  if (keywds!=0) 
  { 
    for (i=0; i < *num_kwd;  i++)
      {
      j = phoFUNC_PARAM_CODES[fcn][i];
      strcpy( keywds+i*(phoMAX_KEYWD_LENGTH+1), phoPARAM_KEYWORDS[j]);

      };
  } 

  return phoSUCCESS;
}


/********************************************************************/
	/*
	 * Function pho_get_keys:
	 *
	 * routine to allow user to retrieve a value from the PHO data 
	 * object for a given parameter keyword.
	 *
	 * This is the Fortran-callable version;  it should be maintained
	 * in parallel with the C-callable version phoGetKeys().
	 *
         * The FORTRAN calling sequence is:
         *
         *      INTEGER PHO, NUM_KWD, STATUS
         *      CHARACTER*(phoMAX_KEYWD_LENGTH) KEYWDS(phoMAX_PARAM_PER_FUNC)
         *      ....
         *      CALL PHO_GET_KEYS( PHO, KEYWDS, NUM_KWDS, STATUS)
	 *
	 * 05jan94 -lwk-  intial version
	 * Feb '94 - F. Oschuetz,DLR - sc2for_array - correction
	 */
/********************************************************************/

/*************************************************************************

FORTRAN Callable Version	pho_get_keys

*************************************************************************/

int FTN_NAME2_(pho_get_keys, PHO_GET_KEYS) ( PHO_STRUCT **pho,
			char *fkeywds, int *num_kwd, int *fstat, ZFORSTR_PARAM)
{
  ZFORSTR_BLOCK
  int len, max, i;
  char *pkeywds;
  char keywds[phoMAX_PARAM_PER_FUNC][phoMAX_KEYWD_LENGTH+1];

	/* call the C-callable version: */

  pkeywds = (char *)malloc( phoMAX_PARAM_PER_FUNC * ( phoMAX_KEYWD_LENGTH+1 ) * sizeof(char));
  if( pkeywds == NULL ) 
		    {
			zvmessage(" ","");
	    		zvmessage("***pho_get_keys error***","");
	    		zvmessage("*** memmoty allocation failed ***","");
			free(pkeywds);
	    		zmabend("pho_routines abend");
		    }


  *fstat = phoGetKeys( *pho, pkeywds, num_kwd);
  if( *fstat!=phoSUCCESS ) 
		    {
			zvmessage(" ","");
	    		zvmessage("***pho_get_keys error***","");
	    		zvmessage("*** phoGetKeys failed ***","");
			free(pkeywds);
	    		zmabend("pho_routines abend");
		    }


  for (i=0; i<*num_kwd; i++) 
  {
    strncpy( keywds[i], pkeywds+i*(phoMAX_KEYWD_LENGTH+1),
		phoMAX_KEYWD_LENGTH+1 );
  }


  if (*fstat != phoSUCCESS) return *fstat;

	/* convert the C strings to FORTRAN: */

  len = phoMAX_KEYWD_LENGTH+1;
  max = phoMAX_KEYWD_LENGTH;
  zsc2for_array( (char *)keywds, len, *num_kwd, fkeywds, &max, &pho, 4, 2, 1, fstat);
  free(pkeywds);

  return phoSUCCESS;
}



/********************************************************************/
	/*
	 * Function phoGetMode:
	 *
	 * routine to allow user to get the flag from the PHO data object
	 * for the compution mode of phoBidiRef;	 
	 * This is the C-callable version;  it should be maintained
	 * in parallel with the Fortran-callable version pho_get_mode().
	 *
	 * oct95  -F.Oschuetz-  initial version
	 *
	 * the calling sequence is:
	 *
	 *    phoSetMode( pho, mode)
	 *
	 * where:
	 *  PHO_STRUCT *pho	(input)  is the PHO object being referenced
	 *  char *mode		(input)  is the key of the mode
	 */

/********************************************************************/
/*************************************************************************

C Callable Version	phoGetMode

*************************************************************************/

int phoGetMode( 
  PHO_STRUCT *pho,
  char mode[phoMax_MODE_NAME_LENGTH+1])

{
  strcpy(mode, pho->flag_mode);

  return phoSUCCESS;

}


/*************************************************************************

				phoGetMode

**************************************************************************
	 *
	 * routine to allow user to get the flag from the PHO data object
	 * for the compution mode of pho_Bidi_Ref;	 
	 *
	 * Necessary include files for calling routine or program: 
	 * 	pho.h (for C routines )
	 * 	pho.fin (for FORTRAN routines )
	 *
	 * Other requirements: 	need to run routines phoInit first
	 *
	 * Necessary include files from calling routine or program: 
	 * 		pho.h (in C routines) 
	 *		pho.fin ( in FORTRAN routines)
	 *
	 * Calling Sequence:		
	 *
	 * calling from C:	include "pho.h"
	 *			PHO PHO_DATA;
	 *			char MODE[phoMax_MODE_NAME_LENGTH+1];
	 *			int status;
	 *			...
	 *			status = phoGetMode( PHO_DATA, MODE); 
	 *
	 * calling from FORTRAN:
	 *			INCLUDE 'pho'
	 *			CHARACTER*(phoMax_MODE_NAME_LENGTH) MODE
	 *			INTEGER PHO_DATA, status
	 *			...
	 *			call pho_get_mode( PHO_DATA, MODE, status )
	 *
	 * where:
	 *  PHO_STRUCT *pho	(input)  is the PHO object being referenced
	 *  char *mode	(output) is the name of compution mode of phoBidiRef
	 *  int status		(Output) Error status:
	 *				   phoSUCCESS - success
	 *				   no error check in this subroutine!
	 *
	 * History : oct95  -F.Oschuetz-  initial version
	 * 
	 *
*************************************************************************/

/*************************************************************************

FORTRAN Callable Version	pho_get_mode

**************************************************************************/

int FTN_NAME2_(pho_get_mode, PHO_GET_MODE) ( PHO_STRUCT **pho, char *mode,
						int *fstat, ZFORSTR_PARAM)
{
  ZFORSTR_BLOCK
  char f_mode[phoMax_MODE_NAME_LENGTH+1];

	/* call the C-callable version: */

  *fstat = phoGetMode( *pho, f_mode);

  if (*fstat != phoSUCCESS) return *fstat;

	/* convert the C string to FORTRAN: */
/*  zsc2for( f_mode, phoMAX_FUNC_NAME_LENGTH, mode, &pho, 3, 2, 1, fstat);*/
  zsc2for( f_mode, 0, mode, &pho, 3, 2, 1, fstat);

  return phoSUCCESS;
}

/*************************************************************************

FORTRAN Callable Version

*************************************************************************/

int FTN_NAME2_(pho_get_parms, PHO_GET_PARMS) ( pho_obj, fstat )
 PHO *pho_obj;
 int *fstat;

{

*fstat = phoGetParms( *pho_obj );

return *fstat;

}

/*************************************************************************

C Callable Version

*************************************************************************/

int phoGetParms( PHO pho_obj )

{
  
  int i;
  int status, rstatus, count;
  char FuncName[phoMAX_FUNC_NAME_LENGTH+1];
  int KeywordCount;
  char *pkeywds;  
  char keywords[phoMAX_PARAM_PER_FUNC][phoMAX_KEYWD_LENGTH+1];
  float temp;
  double phoParms[phoMAX_PARAM_PER_FUNC];

  rstatus = phoSUCCESS;



/*  get the photometric Function from the PDF : */

  status = zvp( "PHO_FUNC", FuncName, &count );
  status = zvpcnt( "PHO_FUNC", &count );
  if ( count == 0 ) 
  {
	zvmessage(" ","");
	zvmessage("*** ERROR in phoGetPar : pho FUNC NOT SPECIFIED ***","");
	zvmessage(" ","");
	return phoFUNC_NOT_SPECIFIED;
  }



/* set the Function name in the photometric object : */
   
  status = phoSetFunc( pho_obj, FuncName);
  if ( status == phoINVALID_FUNCTION ) 
  {
	zvmessage(" ","");
	zvmessage("*** ERROR in phoGetPar : invalid phot function ***","");
	zvmessage(" ","");
	return status;
  }
  if ( status == phoFUNC_CHANGED ) rstatus = status;



/* get the list of valid parameter keywords for the current function : */

    pkeywds = (char *)malloc( phoMAX_PARAM_PER_FUNC * ( phoMAX_KEYWD_LENGTH+1 ) * sizeof(char));
  if( pkeywds == NULL ) 
	{
	  zvmessage(" ", "");
	  zvmessage("***phoGetParms error***","");
	  zvmessage("*** memory allocation failed ***","");
	  free(pkeywds);
	  zmabend("pho_routines abend");
	}

  status = phoGetKeys(pho_obj, pkeywds, &KeywordCount );

  for (i=0; i<KeywordCount; i++) 
  {
    strncpy( keywords[i], pkeywds+i*(phoMAX_KEYWD_LENGTH+1), \
		phoMAX_KEYWD_LENGTH+1 );
  }


/* get the parameter values for these valid keywords from the PDF and */
/* set them in the photometric object : */

  for( i=0; i < KeywordCount; i++)
  {
	/* this construction with 'temp' is needed because of 
	 * problems with zvp on the Sun (perhaps it works zvparamd too) : */
	status = zvp( keywords[i], &temp, &count);
	status = zvpcnt( keywords[i], &count);
	if ( count != 0 )
	{ 
	   phoParms[i] = (double)temp;
	   status = phoSetVal( pho_obj, keywords[i], phoParms[i]); 
  	   if ( status == phoKEYWD_CHANGED ) rstatus = status;
	} 
 	if ( count == 0 ) rstatus = phoKEYWD_NOT_SPECIFIED;	
  }
  free(pkeywds);
  return rstatus;
  
}


/********************************************************************/
	/*
	 * Function phoGetVal:
	 *
	 * routine to allow user to retrieve a value from the PHO data 
	 * object for a given parameter keyword; a separate call, 
	 * phoGetFunc, is used to retrieve the photometric function.
	 *
	 * This is the C-callable version;  it should be maintained
	 * in parallel with the Fortran-callable version pho_get_val().
	 *
	 * 05jan94  -lwk-  intial version
	 * March '94 - F. Oschuetz - error messages
	 *
	 * the calling sequence is:
	 *
	 *    phoGetVal( pho, key, val)
	 *
	 * where:
	 *  PHO_STRUCT *pho	(input)  is the PHO object being referenced
	 *  char *key		(input)  is the name of the keyword
	 *  double *val		(output) is the value for the keyword 
	 */
/********************************************************************/

/*************************************************************************

C Callable Version	phoGetVal

*************************************************************************/

int phoGetVal( 
  PHO_STRUCT *pho,
  char key[phoMAX_KEYWD_LENGTH+1],
  double *val)

{
  int i, j, fcn, kwd;


	/* first determine the photometric function: */

  for (i=0, fcn = -1; i<phoFUNCTION_COUNT && fcn==-1; i++)
    if (EQUAL( phoFUNC_NAMES[i], pho->func_name)) fcn = i;
  if (fcn == -1) return phoFUNC_NOT_SET;


	/* search all valid keywords for this function: */

  for (i=0, kwd = -1; i<phoPARAM_COUNT[fcn] && kwd==-1; i++) 
  {
    j = phoFUNC_PARAM_CODES[fcn][i];
    if (EQUAL( phoPARAM_KEYWORDS[j], key)) kwd = i;
  };

  if (kwd == -1) return phoINVALID_KEYWD;


	/* check the "value set" flag for this parameter: */

  if (!pho->flag_set[kwd]) return phoKEYWD_NOT_SET;


	/* and retrieve the value: */

  *val = pho->func_params[kwd];


  return phoSUCCESS;
}


/********************************************************************/
	/*
	 * Function pho_get_val:
	 *
	 * routine to allow user to retrieve a value from the PHO data 
	 * object for a given parameter keyword.
	 *
	 * This is the Fortran-callable version;  it should be maintained
	 * in parallel with the C-callable version phoGetVal().
	 *
         * The FORTRAN calling sequence is:
         *
         *      INTEGER PHO, STATUS
         *      CHARACTER*8 KEYWD	! (length = phoMAX_KEYWD_LENGTH)
         *      DOUBLE PRECISION VAL
         *      ....
         *      CALL pho_get_VAL( PHO, KEYWD, VAL, STATUS)
         *
	 * 05jan94 -lwk-  intial version
	 *
	 */
/********************************************************************/

/*************************************************************************

FORTRAN Callable Version	pho_get_val

*************************************************************************/

int FTN_NAME2_(pho_get_val, PHO_GET_VAL) ( PHO_STRUCT **pho, char *fkey,
				double *val, int *fstat, ZFORSTR_PARAM)
{
  ZFORSTR_BLOCK
  char key[phoMAX_KEYWD_LENGTH+1];


	/* convert the FORTRAN string to C: */
  zsfor2c( key, phoMAX_KEYWD_LENGTH, fkey, &pho, 4, 2, 1, fstat);


	/* and call the C-callable version: */
  *fstat = phoGetVal( *pho, key, val);


 return *fstat;
}


/*************************************************************************

FORTRAN Callable Version  pho_init

*************************************************************************/

int FTN_NAME2_(pho_init, PHO_INIT) ( ptr, fstat )
int *ptr;
int *fstat;

{
PHO	pho_obj;

*fstat = phoInit( &pho_obj );

if (*fstat != phoSUCCESS) return *fstat;

*ptr = (int)pho_obj;

return *fstat;

}


/*************************************************************************

C Callable Version	phoInit

*************************************************************************/

int phoInit( 
PHO *pho_obj)

{
  int i;
  PHO_STRUCT *ppho;


	/* Allocate memory for pointer to structure PHO_STRUCT :	*/


  *pho_obj = (PHO *) malloc (sizeof(PHO_STRUCT));


 	/* If memory allocation of PHO Data Object fails, print error	*/
	/* message;  else, return status equals phoSUCESS and set       */
	/* pointer to PHO Data Object.					*/

  if ( *pho_obj == NULL ) 
  {
	zvmessage(" ","");
	zvmessage("*** ERROR in PHO Data Object Initialization","");
	zvmessage("*** Memory allocation failed","");
	zmabend("pho_routines abend");
  }
  else 
  {
	ppho = (PHO_STRUCT *) *pho_obj;
	strcpy( ppho->func_name, "");
	for (i=0; i<phoMAX_PARAM_PER_FUNC; i++) {
	  ppho->flag_set[i] = 0;
	  ppho->flag_chg[i] = 0;
	}
	ppho->flag_func_set[0] = 0;
	ppho->flag_func_chg[0] = 0;
  	strcpy( ppho->flag_mode, phoVALUE); 

	return phoSUCCESS;
  }

}

/********************************************************************/
	/*
	 * Function phoSetFunc:
	 *
	 * routine to allow user to set the photometric function name
	 * in the PHO data object;  a separate call, phoSetVal, is
	 * used to set the parameter values, which must be done after
	 * this function is called.
	 *
	 * This is the C-callable version;  it should be maintained
	 * in parallel with the Fortran-callable version pho_set_func().
	 *
	 * 05jan94  -lwk-  initial version
	 *
	 * the calling sequence is:
	 *
	 *    phoSetFunc( pho, func_name)
	 *
	 * where:
	 *  PHO_STRUCT *pho	(input)  is the PHO object being referenced
	 *  char *func_name	(input)  is the name of the phot.function
	 */

/********************************************************************/

/*************************************************************************

C Callable Version	phoSetFunc

*************************************************************************/

int phoSetFunc(  
  PHO_STRUCT *pho,
  char func_name[phoMAX_FUNC_NAME_LENGTH+1])

{
  int i, fcn, status;

  status = phoSUCCESS;


	/* first check that a valid function name has been passed: */

  for (i=0, fcn = -1; i<phoFUNCTION_COUNT && fcn==-1; i++)
    if (EQUAL( phoFUNC_NAMES[i], func_name)) fcn = i;
  if (fcn == -1) return phoINVALID_FUNCTION;


	/* check whether the function name was already set before : */

  if ( pho->flag_func_set[0] == 1) 
  {
    pho->flag_func_chg[0]= 1;
    status = phoFUNC_CHANGED;
   }
   else pho->flag_func_chg[0]= 0;

  pho->flag_func_set[0] = 1;


	/* set the function name: */

  strcpy( pho->func_name, func_name);


	/* clear all set/changed flags: */

  for (i=0; i<phoMAX_PARAM_PER_FUNC; i++) {
    pho->flag_set[i] = 0;
    pho->flag_chg[i] = 0;
  } 


  return status;
}


/********************************************************************/
	/*
	 * Function pho_set_func:
	 *
	 * routine to allow user to set the photometric function name
	 * in the PHO data object.
	 *
	 * This is the Fortran-callable version;  it should be maintained
	 * in parallel with the C-callable version phoSetFunc().
	 *
         * The FORTRAN calling sequence is:
         *
         *      INTEGER PHO, STATUS
         *      CHARACTER*(phoMAX_FUNC_NAME_LENGTH) FUNC_NAME	
	 *	! (length of the function name keyword= phoMAX_FUNC_NAME_LENGTH)
         *      ....
         *      CALL PHO_SET_FUNC( PHO, FUNC_NAME, STATUS)
         *
	 * 05jan94 -lwk-  initial version
	 *
	 */
/********************************************************************/

/*************************************************************************

FORTRAN Callable Version	pho_set_func

*************************************************************************/

int FTN_NAME2_(pho_set_func, PHO_SET_FUNC) ( PHO_STRUCT **pho,
				char *func_name, int *fstat, ZFORSTR_PARAM)
{
  ZFORSTR_BLOCK
  char f_nam[phoMAX_FUNC_NAME_LENGTH+1];


	/* convert the FORTRAN string to C: */
  zsfor2c( f_nam, phoMAX_KEYWD_LENGTH, func_name, &pho, 3, 2, 1, fstat);


	/* and call the C-callable version: */
  *fstat = phoSetFunc( *pho, f_nam);


  return *fstat;
}


/********************************************************************/
	/*
	 * Function phoSetMode:
	 *
	 * routine to allow user to set the flag in the PHO data object
	 * for the compution mode of phoBidiRef;	 *
	 * This is the C-callable version;  it should be maintained
	 * in parallel with the Fortran-callable version pho_set_mode().
	 *
	 * oct95  -F.Oschuetz-  initial version
	 *
	 * the calling sequence is:
	 *
	 *    phoSetMode( pho, mode)
	 *
	 * where:
	 *  PHO_STRUCT *pho	(input)  is the PHO object being referenced
	 *  char *mode		(input)  is the key of the mode
	 */

/********************************************************************/
/*************************************************************************

C Callable Version	phoSetMode

*************************************************************************/

int phoSetMode( 
  PHO_STRUCT *pho,
  char mode[phoMax_MODE_NAME_LENGTH+1])

{
  strcpy( pho->flag_mode, mode);

  return phoSUCCESS;

}

/********************************************************************/
	/*
	 * Function pho_set_mode:
	 *
	 * routine to allow user to set the key for the compution mode of 
	 * phoBidiRef into the PHO data object.
	 *
	 * This is the Fortran-callable version;  it should be maintained
	 * in parallel with the C-callable version phoSetMode().
	 *
         * The FORTRAN calling sequence is:
         *
         *      INTEGER PHO, STATUS
         *      CHARACTER*(phoMAX_FUNC_NAME_LENGTH) MODE	
	 *	! (length of the mode keyword = phoMax_MODE_NAME_LENGTH)
         *      ....
         *      CALL PHO_SET_MODE( PHO, MODE, STATUS)
         *
	 * oct95 -F.Oschuetz-  initial version
	 *
	 */
/********************************************************************/
/*************************************************************************

FORTRAN Callable Version	pho_set_mode

*************************************************************************/

int FTN_NAME2_(pho_set_mode, PHO_SET_MODE) ( PHO_STRUCT **pho, char *mode,
						int *fstat, ZFORSTR_PARAM)
{
  ZFORSTR_BLOCK
  char f_mode[phoMax_MODE_NAME_LENGTH+1];


	/* convert the FORTRAN string to C: */
  zsfor2c( f_mode, phoMax_MODE_NAME_LENGTH, mode, &pho, 3, 2, 1, fstat);


	/* and call the C-callable version: */
  *fstat = phoSetMode( *pho, f_mode);


  return *fstat;
}


/********************************************************************/
	/*
	 * Function phoSetVal:
	 *
	 * routine to allow user to set a value in the PHO data object
	 * for a given parameter keyword; a separate call, phoSetFunc,
	 * is used to set the photometric function, which must be done
	 * before this function is called.
	 *
	 * This is the C-callable version;  it should be maintained
	 * in parallel with the Fortran-callable version pho_set_val().
	 *
	 * 05jan94  -lwk-  intial version
	 *
	 * the calling sequence is:
	 *
	 *    phoSetVal( pho, key, val)
	 *
	 * where:
	 *  PHO_STRUCT *pho	(input)  is the PHO object being referenced
	 *  char *key		(input)  is the name of the keyword
	 *  double val		(input)  is the value for the keyword 
	 */

/********************************************************************/

/*************************************************************************

C Callable Version	phoSetVal

*************************************************************************/

int phoSetVal( 
  PHO_STRUCT *pho,
  char key[phoMAX_KEYWD_LENGTH+1],
  double val)

{
  int i, j, fcn, kwd, status;


	/* first check that function has been set */

 for (i=0, fcn = -1; i<phoFUNCTION_COUNT && fcn==-1; i++)
    if (EQUAL( phoFUNC_NAMES[i], pho->func_name)) fcn = i;
  if (fcn == -1) return phoFUNC_NOT_SET;


	/* search all valid keywords for this function: */

  for (i=0, kwd = -1; i<phoPARAM_COUNT[fcn] && kwd==-1; i++) 
  {
    j = phoFUNC_PARAM_CODES[fcn][i];
    if (EQUAL( phoPARAM_KEYWORDS[j], key)) kwd = i;
  };
  if (kwd == -1) return phoINVALID_KEYWD;


	/* checks if the values has been set previously : */

  status = phoSUCCESS;
  if (pho->flag_set[kwd] == 1)
  {
     pho->flag_chg[kwd]=1;
     status = phoKEYWD_CHANGED;
   }


	/* set the "value set" and "value changed" flags : */

  pho->flag_set[kwd] = 1;
  pho->flag_chg[kwd] = 1;


	/* assign value to the correct parameter: */

  pho->func_params[kwd] = val;


  return status ;
}


/********************************************************************/
	/*
	 * Function pho_set_val:
	 *
	 * routine to allow user to set a value in the PHO data object
	 * for a given parameter keyword.
	 *
	 * This is the Fortran-callable version;  it should be maintained
	 * in parallel with the C-callable version phoSetVal().
	 *
         * The FORTRAN calling sequence is:
         *
         *      INTEGER PHO, STATUS
         *      CHARACTER*8 KEYWD	! (length = phoMAX_KEYWD_LENGTH)
         *      DOUBLE PRECISION VAL
         *      ....
         *      CALL PHO_SET_VAL( PHO, KEYWD, VAL, STATUS)
         *
	 * 05jan94 -lwk-  intial version
	 *
	 */
/********************************************************************/

/*************************************************************************

FORTRAN Callable Version	pho_set_val

*************************************************************************/

int FTN_NAME2_(pho_set_val, PHO_SET_VAL) ( PHO_STRUCT **pho, char *fkey,
					double *val, int *fstat, ZFORSTR_PARAM)
{
  ZFORSTR_BLOCK
  char key[phoMAX_KEYWD_LENGTH+1];


	/* convert the FORTRAN string to C: */
  zsfor2c( key, phoMAX_KEYWD_LENGTH, fkey, &pho, 4, 2, 1, fstat);


	/* and call the C-callable version: */
  *fstat = phoSetVal( *pho, key, *val);


  return *fstat;
}

/*******************************************************************************

	FORTRAN Callable Version of phoCorrect

*******************************************************************************/

/*==============================================================================

	Returns the photometric correction factor of 
	a given photometric data object (containing a
	photometric function) to change radiance 
	values from meassured viewing and illumination 
	conditions to new artificial target viewing 
	and illumintion conditions.

==============================================================================*/


int FTN_NAME2_(pho_correct, PHO_CORRECT) (pho_obj,MillMode,MSunShadow,
		MViewShadow,MilluArray,TillMode,TSunShadow,TViewShadow,
		TilluArray,phofuncval,fstat)

 PHO_STRUCT **pho_obj; 
/* PHO *pho_obj; */
 double MilluArray[], TilluArray[];
 double *phofuncval;
 int    *fstat;
 int	 *MillMode, *TillMode;
 int	 *MSunShadow, *MViewShadow, *TSunShadow, *TViewShadow;

{
   PHO_ILLUM Millum, Tillum;

   Millum.mode = *MillMode;
   Millum.type.sunshadow = *MSunShadow;
   Millum.type.viewshadow = *MViewShadow;

   Tillum.mode = *TillMode;
   Tillum.type.sunshadow = *TSunShadow;
   Tillum.type.viewshadow = *TViewShadow;

   switch (*MillMode)
   {
   	case illEllCos:

	   Millum.cos.inc =  MilluArray[0];
	   Millum.cos.em = MilluArray[1];
	   Millum.cos.phas = MilluArray[2];
	   
   	break;

   	case illDTMCos:
	   Millum.cos.inc =  MilluArray[0];
	   Millum.cos.em = MilluArray[1];
	   Millum.cos.phas = MilluArray[2];
	   Millum.cos.inc_surf =  MilluArray[3];
	   Millum.cos.em_surf = MilluArray[4];	 
  
   	break;

   	case illEllDir:

	   memcpy(&(Millum.direction.inc), MilluArray, 3 * sizeof(double));
	   memcpy(&(Millum.direction.em), MilluArray+3, 3 * sizeof(double));
	   memcpy(&(Millum.direction.ellips), MilluArray+6, 3 * sizeof(double));
	   memcpy(&(Millum.direction.surf), MilluArray+6, 3 * sizeof(double));
  
   	break;

   	case illDTMDir:

	   memcpy(&(Millum.direction.inc), MilluArray, 3 * sizeof(double));
	   memcpy(&(Millum.direction.em), MilluArray+3, 3 * sizeof(double));
	   memcpy(&(Millum.direction.ellips), MilluArray+6, 3 * sizeof(double));
	   memcpy(&(Millum.direction.surf), MilluArray+9, 3 * sizeof(double));
  
   	break;

   	default:

	zvmessage(" ","");
	zvmessage("*** ERROR in pho_correct: phoINVALID_MILL_MODE: ***","");
	zvmessage(" ","");
	*fstat = phoINVALID_ILL_MODE;

  
   }

   switch (*TillMode)
   {
   	case illEllCos:

	   Tillum.cos.inc =  TilluArray[0];
	   Tillum.cos.em = TilluArray[1];
	   Tillum.cos.phas = TilluArray[2];
	   
   	break;

   	case illDTMCos:

	   Tillum.cos.inc =  TilluArray[0];
	   Tillum.cos.em = TilluArray[1];
	   Tillum.cos.phas = TilluArray[2];
	   Tillum.cos.inc_surf =  TilluArray[3];
	   Tillum.cos.em_surf = TilluArray[4];	 
  
   	break;

   	case illEllDir:

	   memcpy(&(Tillum.direction.inc), TilluArray, 3 * sizeof(double));
	   memcpy(&(Tillum.direction.em), TilluArray+3, 3 * sizeof(double));
	   memcpy(&(Tillum.direction.ellips), TilluArray+6, 3 * sizeof(double));
	   memcpy(&(Tillum.direction.surf), TilluArray+6, 3 * sizeof(double));
  
    	case illDTMDir:

	   memcpy(&(Tillum.direction.inc), TilluArray, 3 * sizeof(double));
	   memcpy(&(Tillum.direction.em), TilluArray+3, 3 * sizeof(double));
	   memcpy(&(Tillum.direction.ellips), TilluArray+6, 3 * sizeof(double));
	   memcpy(&(Tillum.direction.surf), TilluArray+9, 3 * sizeof(double));
  
   	break;


   	default:

	zvmessage(" ","");
	zvmessage("*** ERROR in pho_correct: phoINVALID_TILL_MODE: ***","");
	zvmessage(" ","");
	*fstat = phoINVALID_ILL_MODE;

  
   }

   *fstat = phoCorrect( *pho_obj, &Millum, &Tillum, phofuncval );

return *fstat;
}

/*************************************************************************

	C Callable Version of phoCorrect

*************************************************************************/


int phoCorrect ( 
PHO_STRUCT *pho,
PHO_ILLUM *pMillum,
PHO_ILLUM *pTillum,
double *phoCorVal)

{
  int status, return_status;
  double *MeaBidiR, *TarBidiR; 
  char mode_temp[phoMax_MODE_NAME_LENGTH+1];
  PHO_ILLUM Millum, Tillum;
  PHO_STRUCT *pho_obj; 

  pho_obj = pho;
  return_status = phoSUCCESS;

  status = phoGetMode( pho_obj, mode_temp);
        if(!(status == phoSUCCESS ))
        {
			zvmessage(" ","");
	    		zvmessage("***phoCorrect error***","");
	    		zvmessage("*** phoGetMode failed ***","");
	    		zmabend("pho_routines abend");
        }
  status = phoSetMode( pho_obj, phoVALUE);
        if(!(status == phoSUCCESS ))
        {
			zvmessage(" ","");
	    		zvmessage("***phoCorrect error***","");
	    		zvmessage("*** phoSetMode 1 failed ***","");
	    		zmabend("pho_routines abend");
        }


  MeaBidiR = (double *)malloc((4 + phoMAX_PARAM_PER_FUNC) * sizeof(double));
  if( MeaBidiR == NULL ) 
  {
    zvmessage("*** phoCorrect: memory allocation failed***","");
    free(MeaBidiR);
    free(TarBidiR);
    zmabend("pho_routines abend");
  }
  if( TarBidiR != NULL ) 
  TarBidiR = (double *)malloc((4 + phoMAX_PARAM_PER_FUNC) * sizeof(double));
  if( TarBidiR == NULL ) 
  {
    zvmessage("*** phoCorrect: memory allocation failed***","");
    free(TarBidiR);
    free(MeaBidiR);
    zmabend("pho_routines abend");
  }
				


			
/* The illMode Parameter indicates how illumination is entered in the routine.*/
/* phoCorrect accepts four different representations of the meassured and     */
/* independently target illumination conditions and : 			      */
/*	a) the cosine of angles of incidence or emission, and phase;	      */
/*	b) the cosine of angles of incidence or emission in relation to the   */
/*         local ellipsoid normal and to the local surface normal,	      */
/*         and cosine of phase; 					      */
/* 	c) the directions of incidence (directed to the planet),	      */
/*	   of emission (directed back from the planet) and		      */
/*	   of ellisoid normal (directed back from the planet		      */
/*	d) the directions of incidence (directed to the planet),	      */
/*	   of emission (directed back from the planet),			      */
/*	   of ellisoid normal (directed back from the planet and	      */
/*	   of surface normal (directed back from the planet)		      */
/*									      */
/* Millum indicates the measured illumination conditions and		      */
/* Tillum the target ones.						      */


  memcpy(&Millum, pMillum, sizeof(PHO_ILLUM));
  memcpy(&Tillum, pTillum, sizeof(PHO_ILLUM));



  switch (Millum.mode)
  {

    case illEllCos:
	break;

    case illDTMCos:
	break;

    case illEllDir:
	break;

    case illDTMDir:
	break;

    default:
	zvmessage(" ","");
	zvmessage("*** ERROR in phoCorrect: phoINVALID_MILL_MODE: ***","");
	zvmessage(" ","");
	*phoCorVal = 1.0;
  	free(TarBidiR);
  	free(MeaBidiR);
	return phoINVALID_ILL_MODE;

  }


  switch (Tillum.mode)
  {

    case illEllCos:
	break;

    case illDTMCos:
	break;

    case illEllDir:
	break;

    case illDTMDir:
	break;

    default:
	zvmessage(" ","");
	zvmessage("*** ERROR in phoCorrect: phoINVALID_TILL_MODE: ***","");
	zvmessage(" ","");
	*phoCorVal = 1.0;
  	free(TarBidiR);
  	free(MeaBidiR);
	return phoINVALID_ILL_MODE;

  } 



  if ( EQUAL( phoATMO_CORR_REGNER, pho_obj->func_name) )
	{
	  zvmessage(" ","");
	  zvmessage("phoCorrect : ATMO_CORR_REGNER is not build in yet","");
	  *phoCorVal = 1.0;
  	  free(TarBidiR);
  	  free(MeaBidiR);
  	  return phoFAILURE;

	}	



/* The routine calls twice the routine 					*/
/* phoBidiRef for the meassured and the targeted			*/
/* illumination condition and divide them.				*/

 switch (Millum.type.sunshadow)
  {

    case illShadow:

  	if ( EQUAL( phoATMO_CORR_REGNER, pho_obj->func_name) )
	   {
	  	zvmessage(" ","");
	  	zvmessage("phoCorrect : ATMO_CORR_REGNER is not build in yet","");
	  	*phoCorVal = 1.0;
  		free(TarBidiR);
  		free(MeaBidiR);
  	  	return phoFAILURE;

	   }
   	else
	   {
	  	zvmessage(" ","");
	  	zvmessage("*** ERROR in phoCorrect : pixel is in the sunshadow ***","");
	  	zvmessage(" ","");
	  	*phoCorVal = 1.0;
  	  	return_status = phoFAILURE;
  		free(TarBidiR);
  		free(MeaBidiR);
		return return_status;
	   }	
    break;  		     /* end of Millum.type.sunshadow : case illShadow */

    case illNoShadow:

	 switch (Millum.type.viewshadow)
	  {

	    case illShadow:

	  	if ( EQUAL( phoATMO_CORR_REGNER, pho_obj->func_name) )
		   {
		  	zvmessage(" ","");
		  	zvmessage("phoCorrect : ATMO_CORR_REGNER is not build in yet","");
		  	*phoCorVal = 1.0;
	  	  	return_status = phoFAILURE;
  			free(TarBidiR);
  			free(MeaBidiR);
			return return_status;

		   }
	   	else
		   {
		  	zvmessage(" ","");
		  	zvmessage("*** ERROR in phoCorrect : pixel is in the viewshadow ***","");
		  	zvmessage(" ","");
		  	*phoCorVal = 1.0;
	  	  	return_status = phoFAILURE;
  			free(TarBidiR);
  			free(MeaBidiR);
			return return_status;
		   }
	    break;  	    /* end of Millum.type.viewshadow : case illShadow */

	    case illNoShadow:

	  	status = phoBidiRef(pho_obj, &Millum, MeaBidiR );
	  	switch (status)
	  	{
	    	   case phoFAILURE:
			return_status = phoFAILURE;
			break;

	    	   case phoFUNC_NOT_SET:
		  	*phoCorVal = 1.0;
  			free(TarBidiR);
  			free(MeaBidiR);
			return phoFUNC_NOT_SET;

	    	   case phoKEYWD_NOT_SET:
		  	*phoCorVal = 1.0;
  			free(TarBidiR);
  			free(MeaBidiR);
			return phoKEYWD_NOT_SET;

	   	   case phoINVALID_KEYWD:
		  	*phoCorVal = 1.0;
  			free(TarBidiR);
  			free(MeaBidiR);
			return phoINVALID_KEYWD;

	    	   case phoARGUMENT_OUT_OF_RANGE:
		  	*phoCorVal = 1.0;
  			free(TarBidiR);
  			free(MeaBidiR);
			return phoARGUMENT_OUT_OF_RANGE;
	  	}
	    break;  	  /* end of Millum.type.viewshadow : case illNoShadow */

	   } 			    /* end of switch (Millum.type.viewshadow) */

    break;  		   /* end of Millum.type.sunshadow : case illNoShadow */

  } 				     /* end of switch (Millum.type.sunshadow) */				   



 switch (Tillum.type.sunshadow)
  {

    case illShadow:

  	if ( EQUAL( phoATMO_CORR_REGNER, pho_obj->func_name) )
	   {
	  	zvmessage(" ","");
	  	zvmessage("phoCorrect : ATMO_CORR_REGNER is not build in yet","");
	  	*phoCorVal = 1.0;
  	  	return_status = phoFAILURE;
  		free(TarBidiR);
  		free(MeaBidiR);
		return return_status;

	   }
   	else
	   {
	  	zvmessage(" ","");
	  	zvmessage("*** ERROR in phoCorrect : pixel is in the target sunshadow ***","");
	  	zvmessage(" ","");
	  	*phoCorVal = 1.0;
  	  	return_status = phoFAILURE;
  		free(TarBidiR);
  		free(MeaBidiR);
		return return_status;
	   }	
    break;	/* end of Tillum.type.sunshadow : case illShadow */

    case illNoShadow:

 	switch (Tillum.type.viewshadow)
  	   {

	    case illShadow:

	  	if ( EQUAL( phoATMO_CORR_REGNER, pho_obj->func_name) )
		   {
		  	zvmessage(" ","");
		  	zvmessage("phoCorrect : ATMO_CORR_REGNER is not build in yet","");
		  	*phoCorVal = 1.0;
  			free(TarBidiR);
  			free(MeaBidiR);
	  	  	return phoFAILURE;

		   }
	   	else
		   {
		  	zvmessage(" ","");
		  	zvmessage("*** ERROR in phoCorrect : pixel is in the target viewshadow ***","");
		  	zvmessage(" ","");
	  	  	return_status = phoFAILURE;

/*			switch (Tillum.mode)
  			{

			    case illEllCos:
			  	*phoCorVal = 1.0;
  				free(TarBidiR);
  				free(MeaBidiR);
				return return_status;
			    break;

			    case illEllDir:
			  	*phoCorVal = 1.0;
  				free(TarBidiR);
  				free(MeaBidiR);
				return return_status;
			    break;

			    case illDTMCos:
			    break;

			    case illDTMDir:
			    break;

			 } 
*/
			
		   }              /* end of phoATMO_REGNER in target viewshadow 
					     		     but no sunShadow */	
/*	   break; */

    	   case illNoShadow:

	  	status = phoBidiRef(pho_obj, &Tillum, TarBidiR);
	  	switch (status)
	  	   {
	    		case phoFAILURE:
			return_status = phoFAILURE;
			break;
	
	    		case phoFUNC_NOT_SET:
	 	  	   *phoCorVal = 1.0;
  			   free(TarBidiR);
  			   free(MeaBidiR);
			   return phoFUNC_NOT_SET;
	
	    		case phoKEYWD_NOT_SET:
		  	   *phoCorVal = 1.0;
  			   free(TarBidiR);
  			   free(MeaBidiR);
			   return phoKEYWD_NOT_SET;

	    		case phoINVALID_KEYWD:
	  		   *phoCorVal = 1.0;
  			   free(TarBidiR);
  			   free(MeaBidiR);
			   return phoINVALID_KEYWD;

	    		case phoARGUMENT_OUT_OF_RANGE:
	  		   *phoCorVal = 1.0;
  			   free(TarBidiR);
  			   free(MeaBidiR);
			   return phoARGUMENT_OUT_OF_RANGE;
	  	   }
	    break;             /* end of case illNoShadow (target viewshadow) */


	   } 			    /* end of switch (Tillum.type.viewshadow) */

    break;			       /* end of case illNoShadow (sunshadow) */

  }				      /* end of switch (Tillum.type.sunshadow */


  *phoCorVal = TarBidiR[0] / MeaBidiR[0];

  status = phoSetMode( pho_obj, mode_temp);
        if(!(status == phoSUCCESS ))
        {
			zvmessage(" ","");
	    		zvmessage("***phoCorrect error***","");
	    		zvmessage("*** phoSetMode failed ***","");
  			free(TarBidiR);
  			free(MeaBidiR);
	    		zmabend("pho_routines abend");
        }

  free(TarBidiR);
  free(MeaBidiR);


  return return_status;

}

/*******************************************************************************

	FORTRAN Callable Version of phoFunc

*******************************************************************************/
/*==============================================================================

	Returns the value of a bidirectional 
	reflectance for a given photometric data object 
	(containing a photometric function) and the 
	angles of incidence, emission and the 
	phase/azimuth angle.

==============================================================================*/


int FTN_NAME2_(pho_func, PHO_FUNC) (pho_obj, illMode, SunShadow, ViewShadow,
					illuArray, phofuncval, fstat)

 PHO_STRUCT **pho_obj;
 double illuArray[];
 double *phofuncval;
 int    *fstat, *illMode, *SunShadow, *ViewShadow;

{
   PHO_ILLUM illum;
   double *phoFuncVal;

   phoFuncVal = (double *)malloc((4 + phoMAX_PARAM_PER_FUNC) * sizeof(double));
   if( phoFuncVal == NULL ) 
   {
     zvmessage("*** pho_func: memory allocation failed***","");
     free(phoFuncVal);
     zmabend("pho_func abend");
   }

   illum.mode = *illMode;
   illum.type.sunshadow = *SunShadow;
   illum.type.viewshadow = *ViewShadow;

   switch (*illMode)
   {
   	case illEllCos:

	   illum.cos.inc =  illuArray[0];
	   illum.cos.em = illuArray[1];
	   illum.cos.phas = illuArray[2];
	   
   	break;

   	case illDTMCos:

	   illum.cos.inc =  illuArray[0];
	   illum.cos.em = illuArray[1];
	   illum.cos.phas = illuArray[2];
	   illum.cos.inc_surf =  illuArray[3];
	   illum.cos.em_surf = illuArray[4];	 
  
   	break;

    	case illEllDir:

	   memcpy(&(illum.direction.inc), illuArray, 3 * sizeof(double));
	   memcpy(&(illum.direction.em), illuArray+3, 3 * sizeof(double));
	   memcpy(&(illum.direction.ellips), illuArray+6, 3 * sizeof(double));
	   memcpy(&(illum.direction.surf), illuArray+6, 3 * sizeof(double));
  
   	break;

    	case illDTMDir:

	   memcpy(&(illum.direction.inc), illuArray, 3 * sizeof(double));
	   memcpy(&(illum.direction.em), illuArray+3, 3 * sizeof(double));
	   memcpy(&(illum.direction.ellips), illuArray+6, 3 * sizeof(double));
	   memcpy(&(illum.direction.surf), illuArray+9, 3 * sizeof(double));
  
   	break;

   	default:

	zvmessage(" ","");
	zvmessage("*** ERROR in pho_bidi_ref: phoINVALID_ILL_MODE: ***","");
	zvmessage(" ","");
	*fstat = phoINVALID_ILL_MODE;

  
   }

   *fstat = phoFunc( *pho_obj, &illum, phoFuncVal );
   *phofuncval=phoFuncVal[0];

return *fstat;
}

/*************************************************************************

	C Callable Version of phoFunc


*************************************************************************/


int phoFunc ( 
PHO_STRUCT *pho_obj,
PHO_ILLUM *illum,
double *phoFuncVal)

{
  int status, return_status;
  double *bidiref, *bidiref0, Phase0;
  char mode_temp[phoMax_MODE_NAME_LENGTH+1];
  PHO_ILLUM Nillum;

  return_status = phoSUCCESS;

  status = phoGetMode( pho_obj, mode_temp);
    if(status!=phoSUCCESS) 
		    {
			zvmessage(" ","");
	    		zvmessage("***phoFunc error***","");
	    		zvmessage("*** phoGetMode failed ***","");
	    		zmabend("pho_routines abend");
		    }
  status = phoSetMode( pho_obj, phoVALUE);
    if(status!=phoSUCCESS) 
		    {
			zvmessage(" ","");
	    		zvmessage("***phoFunc error***","");
	    		zvmessage("*** phoSetMode 1 failed ***","");
	    		zmabend("pho_routines abend");
		    }


  bidiref = (double *)malloc((4 + phoMAX_PARAM_PER_FUNC) * sizeof(double));
  if( bidiref == NULL ) 
  {
    zvmessage("*** phoFunc: memory allocation failed***","");
    free(bidiref);
    zmabend("pho_routines abend");
  }
  bidiref0 = (double *)malloc((4 + phoMAX_PARAM_PER_FUNC) * sizeof(double));
  if( bidiref0 == NULL ) 
  {
    zvmessage("*** phoFunc: memory allocation failed***","");
    free(bidiref);
    free(bidiref0);
    zmabend("pho_routines abend");
  }
				

/* The illMode Parameter indicates how illumination is entered in the routine.*/
/* phoCorrect accepts four different representations of the meassured and     */
/* independently target illumination conditions and : 			      */
/*	a) the cosine of angles of incidence or emission, and phase;	      */
/*	b) the cosine of angles of incidence or emission in relation to the   */
/*         local ellipsoid normal and to the local surface normal,	      */
/*         and cosine of phase; 					      */
/* 	c) the directions of incidence (directed to the planet),	      */
/*	   of emission (directed back from the planet) and		      */
/*	   of ellisoid normal (directed back from the planet		      */
/*	d) the directions of incidence (directed to the planet),	      */
/*	   of emission (directed back from the planet),			      */
/*	   of ellisoid normal (directed back from the planet and	      */
/*	   of surface normal (directed back from the planet)		      */
 

 switch (illum->mode)
  {

    case illEllCos:

	memcpy(&Nillum, illum, sizeof(PHO_ILLUM));
	Nillum.cos.inc = illum->cos.em;
	Nillum.cos.phas = 1.0;

    break;

    case illDTMCos:

	memcpy(&Nillum, illum, sizeof(PHO_ILLUM));
	Nillum.cos.inc = illum->cos.em;
	Nillum.cos.phas = 1.0;
	Nillum.cos.inc_surf = illum->cos.em_surf;

    break;

    case illEllDir:

	memcpy(&Nillum, illum, sizeof(PHO_ILLUM));
	memcpy(&Nillum.direction.inc, &illum->direction.em, 3 * sizeof(double));

    break;

    case illDTMDir:

	memcpy(&Nillum, illum, sizeof(PHO_ILLUM));
	memcpy(&Nillum.direction.inc, &illum->direction.em, 3 * sizeof(double));

    break;

    default:
	zvmessage(" ","");
	zvmessage("*** ERROR in phoFunc: INVALID_ILL_MODE: ***","");
	zvmessage(" ","");
    	free(bidiref);
   	free(bidiref0);
	return phoINVALID_ILL_MODE;

  }



  if ( EQUAL( phoATMO_CORR_REGNER, pho_obj->func_name) )
	{
	  zvmessage(" ","");
	  zvmessage("*** ERROR in phoFunc : invalid phot function ATMO_CORR_REGNER ","");
	  zvmessage(" ","");

	  *phoFuncVal = 1.0;
    	  free(bidiref);
    	  free(bidiref0);
  	  return phoFAILURE;

	}	


/* 	The routine calls twice the routine 				*/
/*	phoBidiRef to compute the ratio of the				*/ 
/*	bidirectional reflectance for a fixed emission 			*/
/*	angle, but varying incidence and phase angles, 			*/
/*	to its value at phase-angle=0 as defined by 			*/
/*	Bruce Hapke, Theory of Reflectance Spectroscopy, 		*/
/*	(Topics in Remote Sensing;3), 					*/
/*	Cambridge University Press 1993, p. 272.			*/


  status = phoBidiRef(pho_obj, illum, bidiref);
  switch (status)
  {
    case phoFAILURE:
	return_status = phoFAILURE;
	break;

    case phoFUNC_NOT_SET:
	*phoFuncVal = 1.0;
	return phoFUNC_NOT_SET;

    case phoKEYWD_NOT_SET:
	*phoFuncVal = 1.0;
    	free(bidiref);
    	free(bidiref0);
	return phoKEYWD_NOT_SET;

    case phoINVALID_KEYWD:
	*phoFuncVal = 1.0;
    	free(bidiref);
    	free(bidiref0);
	return phoINVALID_KEYWD;

    case phoARGUMENT_OUT_OF_RANGE:
	*phoFuncVal = 1.0;
    	free(bidiref);
    	free(bidiref0);
	return phoARGUMENT_OUT_OF_RANGE;
  }


  status = phoBidiRef(pho_obj, &Nillum, bidiref0);
  switch (status)
  {
    case phoFAILURE:
	return_status = phoFAILURE;
	break;

    case phoFUNC_NOT_SET:
	*phoFuncVal = 1.0;
    	free(bidiref);
    	free(bidiref0);
	return phoFUNC_NOT_SET;

    case phoKEYWD_NOT_SET:
	*phoFuncVal = 1.0;
    	free(bidiref);
    	free(bidiref0);
	return phoKEYWD_NOT_SET;

    case phoINVALID_KEYWD:
	*phoFuncVal = 1.0;
    	free(bidiref);
    	free(bidiref0);
	return phoINVALID_KEYWD;

    case phoARGUMENT_OUT_OF_RANGE:
    	free(bidiref);
    	free(bidiref0);
	return phoARGUMENT_OUT_OF_RANGE;
  }



  *phoFuncVal = (*bidiref) / (*bidiref0);

  status = phoSetMode( pho_obj, mode_temp);
    if(status!=phoSUCCESS) 
		    {
			zvmessage(" ","");
	    		zvmessage("***phoFunc error***","");
	    		zvmessage("*** phoSetMode 2 failed ***","");
    			free(bidiref);
    			free(bidiref0);
	    		zmabend("pho_routines abend");
		    }

  free(bidiref);
  free(bidiref0);
  

  return return_status;
}

/*******************************************************************************

	FORTRAN Callable Version of phoBidiRef

*******************************************************************************/

/*==============================================================================

	Returns the value of a bidirectional 
	reflectance for a given photometric data object 
	(containing a photometric function) and the 
	angles of incidence, emission and the 
	phase/azimuth angle.

==============================================================================*/


int FTN_NAME2_(pho_bidi_ref, PHO_BIDI_REF) (pho_obj,illMode,SunShadow,
				ViewShadow,illuArray,phofuncval,fstat)

 PHO_STRUCT **pho_obj;
 double illuArray[];
 double *phofuncval;
 int    *fstat, *illMode, *SunShadow, *ViewShadow;

{
   PHO_ILLUM illum;
   double *phoFuncVal;

   phoFuncVal = (double *)malloc((4 + phoMAX_PARAM_PER_FUNC) * sizeof(double));
   if( phoFuncVal == NULL ) 
   {
     zvmessage("*** pho_func: memory allocation failed***","");
     free(phoFuncVal);
     zmabend("pho_func abend");
   }

   illum.mode = *illMode;
   illum.type.sunshadow = *SunShadow;
   illum.type.viewshadow = *ViewShadow;



   switch (*illMode)
   {
   	case illEllCos:

	   illum.cos.inc =  illuArray[0];
	   illum.cos.em = illuArray[1];
	   illum.cos.phas = illuArray[2];
	   
   	break;

   	case illDTMCos:

	   illum.cos.inc =  illuArray[0];
	   illum.cos.em = illuArray[1];
	   illum.cos.phas = illuArray[2];
	   illum.cos.inc_surf =  illuArray[3];
	   illum.cos.em_surf = illuArray[4];	 
  
   	break;

    	case illEllDir:

	   memcpy(&(illum.direction.inc), illuArray, 3 * sizeof(double));
	   memcpy(&(illum.direction.em), illuArray+3, 3 * sizeof(double));
	   memcpy(&(illum.direction.ellips), illuArray+6, 3 * sizeof(double));
	   memcpy(&(illum.direction.surf), illuArray+6, 3 * sizeof(double));
  
   	break;

    	case illDTMDir:

	   memcpy(&(illum.direction.inc), illuArray, 3 * sizeof(double));
	   memcpy(&(illum.direction.em), illuArray+3, 3 * sizeof(double));
	   memcpy(&(illum.direction.ellips), illuArray+6, 3 * sizeof(double));
	   memcpy(&(illum.direction.surf), illuArray+9, 3 * sizeof(double));
  
   	break;

   	default:

	zvmessage(" ","");
	zvmessage("*** ERROR in pho_bidi_ref: phoINVALID_ILL_MODE: ***","");
	zvmessage(" ","");
	*fstat = phoINVALID_ILL_MODE;

  
   }


   *fstat = phoBidiRef( *pho_obj, &illum, phoFuncVal );
   *phofuncval=phoFuncVal[0];


return *fstat;
}
/*************************************************************************

	C Callable Version of phoBidiRef

*************************************************************************/


int phoBidiRef ( 
PHO_STRUCT *pho_obj,
PHO_ILLUM *illum,
double *phoFuncVal)

{

  void (*func)();
  PHO_STRUCT *pho;
  int i, fcn;
  double radInc, radEm, caz;
  int status, count;
  char FuncName[phoMAX_FUNC_NAME_LENGTH+1];  
  int KeywordCount;
  char *pkeywds;
  char keywords[phoMAX_PARAM_PER_FUNC][phoMAX_KEYWD_LENGTH+1];
  char phoMode[phoMax_MODE_NAME_LENGTH+1];
  double phoParms[phoMAX_PARAM_PER_FUNC];
  double arguments[phoMAX_ARGUMENTS_PER_FUNC];

  status = phoSUCCESS;


	/* first determine the photometric function: */

  pho = pho_obj;
  for (i=0, fcn = -1; i<phoFUNCTION_COUNT && fcn==-1; i++)
    if (EQUAL( phoFUNC_NAMES[i], pho->func_name)) fcn = i;
  if (fcn == -1) 
  {
	zvmessage(" ","");
	zvmessage("*** ERROR in phoBidiRef : invalid phot function ***","");
	zvmessage(" ","");
	return phoFUNC_NOT_SET;
  }

  if (EQUAL( phoATMO_CORR_REGNER, pho->func_name)) 
  {
	zvmessage(" ","");
	zvmessage("*** ERROR in phoBidiRef : invalid phot function phoATMO_CORR_REGNER ***","");
	zvmessage(" ","");
        phoFuncVal[0] = 1.0;
	return phoFAILURE;
  }

  func = phoFUNC_CALL_NAME[fcn];


	/* get the parameter keyword list : */
  
  pkeywds = (char *)malloc( phoMAX_PARAM_PER_FUNC * ( phoMAX_KEYWD_LENGTH+1 ) * sizeof(char));
  if( pkeywds == NULL ) 
  {
    zvmessage("*** phoBidiRef: memory allocation failed***","");
    free(pkeywds);
    zmabend("pho_routines abend");
  }

  status = phoGetKeys( pho_obj, pkeywds, &KeywordCount );

  switch (status)
  {
   case phoFAILURE:
	zvmessage(" ","");
	zvmessage("*** ERROR in phoBidiRef : phoGetKeys failed ***","");
	zvmessage(" ","");
        phoFuncVal[0] = 1.0;
    	free(pkeywds);
	return phoFAILURE;

  case phoFUNC_NOT_SET:
	zvmessage(" ","");
	zvmessage("*** ERROR in phoBidiRef : phoFUNC_NOT_SET ***","");
	zvmessage(" ","");
        free(pkeywds);
	return phoFUNC_NOT_SET;
  }

  for (i=0; i<KeywordCount; i++) 
  {
    strncpy( keywords[i], pkeywds+i*(phoMAX_KEYWD_LENGTH+1),
		phoMAX_KEYWD_LENGTH+1 );
  }
	/* get the parameter values for these keywords : */

  for( i=0; i < KeywordCount; i++)
  {
	status = phoGetVal( pho_obj, keywords[i], &phoParms[i]); 
  	switch (status)
 	{
 	  case phoFUNC_NOT_SET:
	    zvmessage(" ","");
	    zvmessage("*** ERROR in phoBidiRef :phoFUNC_NOT_SET : ***","");
	    zvmessage(FuncName, "");
	    zvmessage(" ","");
    	    free(pkeywds);
	    return phoFUNC_NOT_SET;

 	  case phoKEYWD_NOT_SET:
	    zvmessage(" ","");
	    zvmessage("*** ERROR in phoBidiRef :phoKEYWORD_NOT_SET : ***","");
	    zvmessage(FuncName, "");
	    zvmessage(" ","");
    	    free(pkeywds);
	    return phoKEYWD_NOT_SET;

   	  case phoINVALID_KEYWD:
	    zvmessage(" ","");
	    zvmessage("*** ERROR in phoBidiRef :pho_INVALID_KEYWORD: ***","");
	    zvmessage(keywords[i], "");
	    zvmessage(" ","");
    	    free(pkeywds);
	    return phoINVALID_KEYWD;
	}	

  }

				

/* The illMode Parameter indicates how illumination is entered in the routine.*/
/* phoCorrect accepts four different representations of the meassured and     */
/* independently target illumination conditions and : 			      */
/*	a) the cosine of angles of incidence or emission, and phase;	      */
/*	b) the cosine of angles of incidence or emission in relation to the   */
/*         local ellipsoid normal and to the local surface normal,	      */
/*         and cosine of phase; 					      */
/* 	c) the directions of incidence (directed to the planet),	      */
/*	   of emission (directed back from the planet) and		      */
/*	   of ellisoid normal (directed back from the planet		      */
/*	d) the directions of incidence (directed to the planet),	      */
/*	   of emission (directed back from the planet),			      */
/*	   of ellisoid normal (directed back from the planet and	      */
/*	   of surface normal (directed back from the planet)		      */

   
  switch (illum->type.sunshadow)
  {

  case illNoShadow:

     switch (illum->type.viewshadow)
     {

     case illNoShadow:

     	switch (illum->mode)
    	{


    	case illEllCos:

	   if (illum->cos.inc < 0 || illum->cos.inc-ANG_EPS >= 1.0 || illum->cos.em < 0 || illum->cos.em-ANG_EPS >= 1.0 || illum->cos.phas < -1.0 || illum->cos.phas > 1.0 ) 
  	   {
	  	zvmessage(" ","");
	 	 zvmessage("*** ERROR in phoBidiRef : ARGUMENT_OUT_OF_RANGE ***","");
	  	zvmessage(" ","");
  	        phoFuncVal[0] = 1.0;
    		free(pkeywds);
	  	return phoARGUMENT_OUT_OF_RANGE;
 	   }



	   arguments[0] = illum->cos.inc;
	   arguments[1] = illum->cos.em;
	   arguments[2] = illum->cos.phas;

	   if ( fabs(illum->cos.inc) < ANG_EPS || fabs(illum->cos.em) < ANG_EPS ) 
		caz = 1.0;
           if ( fabs(1.0 - arguments[0] * arguments[0]) <= 0.0 || fabs(1.0 - 
arguments[1] * arguments[1]) <= 0.0 )
		caz = 1.0;
	   else  
		caz = ( arguments[2] - arguments[1] * arguments[0] ) / sqrt( fabs(1. - arguments[0] * arguments[0]) * fabs(1. - arguments[1] * arguments[1]) );
	   if (caz+ANG_EPS < -1.0 || caz-ANG_EPS > 1.0 ) 
  	   {
	  	zvmessage(" ","");
	  	zvmessage("*** ERROR in phoBidiRef : ARGUMENT_OUT_OF_RANGE ***","");
	 	zvmessage(" ","");
	        phoFuncVal[0] = 1.0;
    		free(pkeywds);
	  	return phoARGUMENT_OUT_OF_RANGE;
  	   }


    	break;						   /* case illEllCos  */

    	case illDTMCos:

       	/* no check for incidence and emission angle greater 90 degree,    */
	/* because a mountain behind terminator / limb 			      */
	/* can be illuminated / visible					      */

	   if ( illum->cos.inc-ANG_EPS >= 1.0 || illum->cos.em-ANG_EPS >= 1.0 || illum->cos.phas < -1.0 || illum->cos.phas > 1.0 )
  	   {
	  	zvmessage(" ","");
	 	zvmessage("*** ERROR in phoBidiRef : ARGUMENT_OUT_OF_RANGE ***","");
	  	zvmessage(" ","");
	        phoFuncVal[0] = 1.0;
    		free(pkeywds);
	  	return phoARGUMENT_OUT_OF_RANGE;
 	   }

if ( illum->cos.inc_surf < 0 || illum->cos.inc_surf-ANG_EPS >= 1.0 || illum->cos.em_surf < 0 || illum->cos.em_surf-ANG_EPS >= 1.0 ) 
  	   {
	  	zvmessage(" ","");
	 	zvmessage("*** ERROR in phoBidiRef : ARGUMENT_OUT_OF_RANGE ***","");
	  	zvmessage(" ","");
	        phoFuncVal[0] = 1.0;
    		free(pkeywds);
	  	return phoARGUMENT_OUT_OF_RANGE;
 	   }


	   arguments[0] = illum->cos.inc;
	   arguments[1] = illum->cos.em;
	   arguments[2] = illum->cos.phas;
	   arguments[3] = illum->cos.inc_surf;
	   arguments[4] = illum->cos.em_surf;

/*
	   if ( fabs(illum->cos.inc) < ANG_EPS || fabs(illum->cos.em) < ANG_EPS ) 
		caz = 1.0;
           if ( fabs(1.0 - arguments[0] * arguments[0]) == 0.0 || fabs(1.0 - 
arguments[1] * arguments[1]) == 0.0 )
		caz = 1.0;
	   else  
		caz = ( arguments[2] - arguments[1] * arguments[0] ) / sqrt( fabs(1. - arguments[0] * arguments[0]) * fabs(1. - arguments[1] * arguments[1]) );
	   if (caz+ANG_EPS < -1.0 || caz-ANG_EPS > 1.0 ) 
  	   {
	  	zvmessage(" ","");
	 	zvmessage("*** ERROR in phoBidiRef : ARGUMENT_OUT_OF_RANGE ***","");
	  	zvmessage(" ","");
	        phoFuncVal[0] = 1.0;
	    	free(pkeywds);
	  	return phoARGUMENT_OUT_OF_RANGE;
  	   }
*/

	   if ( fabs(illum->cos.inc_surf) < ANG_EPS || fabs(illum->cos.em_surf) < ANG_EPS ) 
		caz = 1.0;
           if ( fabs(1.0 - arguments[3] * arguments[3]) == 0.0 || fabs(1.0 - 
arguments[4] * arguments[4]) == 0.0 )
		caz = 1.0;
	   else  
		caz = ( arguments[2] - arguments[4] * arguments[3] ) / sqrt( fabs(1. - arguments[3] * arguments[3]) * fabs(1. - arguments[4] * arguments[4]) );
	   if (caz+ANG_EPS < -1.0 || caz-ANG_EPS > 1.0 ) 
  	   {
	  	zvmessage(" ","");
	 	zvmessage("*** ERROR in phoBidiRef : ARGUMENT_OUT_OF_RANGE ***","");
	  	zvmessage(" ","");
	        phoFuncVal[0] = 1.0;
    		free(pkeywds);
	  	return phoARGUMENT_OUT_OF_RANGE;
  	   }

    	break;						    /* case illDTMCos */

    	case illEllDir:

	   arguments[0] = -DIRCOS(illum->direction.ellips,illum->direction.inc);
	   arguments[1] = DIRCOS(illum->direction.ellips,illum->direction.em);
	   arguments[2] = -DIRCOS(illum->direction.em,illum->direction.inc);

	   if (arguments[0] < 0 || arguments[0]-ANG_EPS >= 1.0 || arguments[1] < 0 || arguments[1]-ANG_EPS >= 1.0 || arguments[2] < -1.0 || arguments[2] > 1.0 ) 
  	   {
	  	zvmessage(" ","");
	  	zvmessage("*** ERROR in phoBidiRef : ARGUMENT_OUT_OF_RANGE ***","");
	  	zvmessage(" ","");
	        phoFuncVal[0] = 1.0;
    		free(pkeywds);
	  	return phoARGUMENT_OUT_OF_RANGE;
 	   }

	   if ( fabs(arguments[0]) < ANG_EPS || fabs(arguments[1]) < ANG_EPS ) 
		caz = 1.0;
           if ( fabs(1.0 - arguments[0] * arguments[0]) == 0.0 || fabs(1.0 - 
arguments[1] * arguments[1]) == 0.0 )
		caz = 1.0;
	   else  
		caz = ( arguments[2] - arguments[1] * arguments[0] ) / sqrt( fabs(1. - arguments[0] * arguments[0]) * fabs(1. - arguments[1] * arguments[1]) );
	   if (caz+ANG_EPS < -1.0 || caz-ANG_EPS > 1.0 ) 
  	   {
	  	zvmessage(" ","");
	  	zvmessage("*** ERROR in phoBidiRef : ARGUMENT_OUT_OF_RANGE ***","");
	 	zvmessage(" ","");
	        phoFuncVal[0] = 1.0;
    		free(pkeywds);
	  	return phoARGUMENT_OUT_OF_RANGE;
  	   }

    	break;						    /* case illEllDir */

    	case illDTMDir:

       	/* no check for incidence and emission angle greater 90 degree,       */
	/* because a mountain behind the terminator / limb 		      */
	/* can be illuminated / visible					      */

	   arguments[0] = -DIRCOS(illum->direction.ellips,illum->direction.inc);
	   arguments[1] = DIRCOS(illum->direction.ellips,illum->direction.em);
	   arguments[2] = -DIRCOS(illum->direction.em,illum->direction.inc);
	   arguments[3] = -DIRCOS(illum->direction.surf,illum->direction.inc);
	   arguments[4] = DIRCOS(illum->direction.surf,illum->direction.em);


	   if ( arguments[2] < -1.0 || arguments[2] > 1.0 || arguments[3] < 0.0 || arguments[3]-ANG_EPS >= 1.0 || arguments[4] < 0.0 || arguments[4]-ANG_EPS >= 1.0 ) 
  	   {
	  	zvmessage(" ","");
	  	zvmessage("*** ERROR in phoBidiRef : ARGUMENT_OUT_OF_RANGE ***","");
	  	zvmessage(" ","");
	        phoFuncVal[0] = 1.0;
    		free(pkeywds);
	  	return phoARGUMENT_OUT_OF_RANGE;
 	   }

	   if ( fabs(arguments[3]) < ANG_EPS || fabs(arguments[2]) < ANG_EPS ) 
		caz = 1.0;
           if ( fabs(1.0 - arguments[3] * arguments[3]) == 0.0 || fabs(1.0 - 
arguments[4] * arguments[4]) == 0.0 )
		caz = 1.0;
	   else  
		caz = ( arguments[2] - arguments[4] * arguments[3] ) / sqrt( (1. - arguments[3] * arguments[3]) * (1. - arguments[4] * arguments[4]) );
	   if (caz+ANG_EPS < -1.0 || caz-ANG_EPS > 1.0 ) 
  	   {
	  	zvmessage(" ","");
	  	zvmessage("*** ERROR in phoBidiRef : ARGUMENT_OUT_OF_RANGE ***","");
	  	zvmessage(" ","");
	        phoFuncVal[0] = 1.0;
    		free(pkeywds);
	  	return phoARGUMENT_OUT_OF_RANGE;
  	   }

    	break;						    /* case illDTMDir */

    	default:
	   zvmessage(" ","");
	   zvmessage("*** ERROR in phoBidiRef: phoINVALID_ILL_MODE: ***","");
	   zvmessage(" ","");
    	   free(pkeywds);
	   return phoINVALID_ILL_MODE;

    	}					      /* end case illum->mode */
     break;					      /* case no viewshadow */

     case illShadow:

	phoFuncVal[0] = 1.0;
     	status = phoFAILURE;  

  }					   /* end case illum->type.viewshadow */
  break;						 /* case no sunshadow */

  case illShadow:

     phoFuncVal[0] = 1.0;
     status = phoFAILURE;  
     free(pkeywds);
     return status;  

  }					    /* end case illum->type.sunshadow */

  status = phoGetMode( pho_obj, phoMode);
    if(status!=phoSUCCESS) 
		    {
			zvmessage(" ","");
	    		zvmessage("***phoBidiRef error***","");
	    		zvmessage("*** phoGetMode failed ***","");
    			free(pkeywds);
	    		zmabend("pho_routines abend");
		    }




/*	Determination of the bidirectional function value 		      */
/*	for given Function, Function parameters and arguments		      */
/*	in the case that there are no sunshadow and no viewshadow	      */
/*	The respective functions are stored as privat	 		      */
/*	subroutines within phoFunc:					      */

  (*func)( arguments, phoParms, phoFuncVal, phoMode);

  free(pkeywds);
  return status;
}

/******************************************************************************/











/******************************************************************************

			BURATTI1 Funktion

 ******************************************************************************/

void BURATTI1 ( 
	double arguments[],
	double params[],
	double *result,
	char phoMode[phoMax_MODE_NAME_LENGTH+1])

/***********************************************************************
 * Dim(arguments)= 3	arguments[0] = ci = cos(i)
 *			arguments[1] = ce = cos(e)
 *			arguments[2] = cg = cos(phase angle)
 * Dim(params)=3	params[0..2] = photometric function parameters
 *			  params[0] = A	= ALBEDO
 *			  params[1] = B	= B_VEVERKA
 *			  params[2] = F	= E_BURATTI
 ***********************************************************************/

{
  double phase, lom, xx, yy, phi, burf, bur2, bur3;

  if(!EQUAL(phoMode,phoVALUE)) \
    zmabend("For this function, only phoMode=value is implemented ");

  phase = acos(arguments[2]);

  xx = arguments[0] + arguments[1];
  if(xx < ANG_EPS) xx = ANG_EPS;

  lom = arguments[0]/xx;

  if (phase<= 1.e-10) {
    bur2 = 0.5 * params[0];
    phi = 1.0;
  }
  else {
	/* BURATTI1 approximation : */
    phi = 1.0 + params[1] * phase;

    xx = phase/2.0;
    yy = 1.0/tan(xx/2.0);

	/* Lommel-Seeliger soil phase function (not normalised) : */

    bur2 = 0.5 * params[0] * (1.0 - sin(xx) * tan(xx) * log(yy));
  }

  xx = 1.0 - params[0];
  yy = xx * 2.0/3.0;
  bur3 = (sin(phase) + (M_PI - phase) * arguments[2])/M_PI;

  burf = (yy + params[0]*params[2])*phi - yy * bur3;

  burf /= bur2;

/*   *result = A * ci/(ci+ce) * burf + (1.0 - A) * ci;  */

  *result = params[0] * lom * burf + xx * arguments[0]; 
}

/******************************************************************************

			BURATTI2 Funktion

 ******************************************************************************/

void BURATTI2 (
	double arguments[],
	double params[],
	double *result,
	char phoMode[phoMax_MODE_NAME_LENGTH+1])

/***********************************************************************
 * Dim(arguments)= 3	arguments[0] = ci = cos(i)
 *			arguments[1] = ce = cos(e)
 *			arguments[2] = cg = cos(phase angle)
 * Dim(params)=4	params[0..3] = photometric function parameters
 *			  params[0] = A	= ALBEDO
 *			  params[1] = B = B_VEVERKA
 *			  params[2] = C	= C_VEVERKA = 1.-A_VEVERKA
 *			  params[3] = F = E_BURATTI
 ***********************************************************************/

{
  double phase, lom, xx, yy, phi, burf, bur2, bur3;

  if(!EQUAL(phoMode,phoVALUE)) \
    zmabend("For this function, only phoMode=value is implemented ");

  phase = acos(arguments[2]);

  xx = arguments[0] + arguments[1];
  if(xx < ANG_EPS) xx = ANG_EPS;

  lom = arguments[0]/xx;

  if (phase<= 1.e-10) {
    bur2 = 0.5 * params[0];
    phi = 1.0;
  }
  else {
	/* BURATTI2 approximation : */
    phi = 1.0 + phase * (params[1] + phase * params[2]);

    xx = phase/2.0;
    yy = 1.0/tan(xx/2.0);

	/* Lommel-Seeliger soil phase function (not normalised) : */

    bur2 = 0.5 * params[0] * (1.0 - sin(xx) * tan(xx) * log(yy));
  }

  xx = 1.0 - params[0];
  yy = xx * 2.0/3.0;
  bur3 = (sin(phase) + (M_PI - phase) * arguments[2])/M_PI;

  burf = (yy + params[0]*params[3])*phi - yy * bur3;

  burf /= bur2;

/*   *result = A * ci/(ci+ce) * burf + (1.0 - A) * ci;  */

  *result = params[0] * lom * burf + xx * arguments[0]; 
}

/******************************************************************************

			BURATTI3 Funktion

 ******************************************************************************/

void BURATTI3 ( 
	double arguments[],
	double params[],
	double *result,
	char phoMode[phoMax_MODE_NAME_LENGTH+1])

/***********************************************************************
 * Dim(arguments)= 3	arguments[0] = ci = cos(i)
 *			arguments[1] = ce = cos(e)
 *			arguments[2] = cg = cos(phase angle)
 * Dim(params)=5	params[0..4] = photometric function parameters
 *			  params[0] = A	= ALBEDO
 *			  params[1] = B	= B_VEVERKA
 *			  params[2] = C	= C_VEVERKA = 1.-A_VEVERKA
 *			  params[3] = D	= D_VEVERKA
 *			  params[4] = F	= E_BURATTI
 ***********************************************************************/

{
  double phase, lom, xx, yy, phi, burf, bur2, bur3;

  if(!EQUAL(phoMode,phoVALUE)) \
    zmabend("For this function, only phoMode=value is implemented ");

  phase = acos(arguments[2]);

  xx = arguments[0] + arguments[1];
  if(xx < ANG_EPS) xx = ANG_EPS;

  lom = arguments[0]/xx;
  
  if (phase<= 1.e-10) {
    bur2 = 0.5 * params[0];
    phi = 1.0;
  }
  else {
    xx  = exp(-params[3] * phase) - 1.0;
	/* BURATTI3 approximation : */
    phi = 1.0 + params[1] * phase + params[2] * xx;

    xx = phase/2.0;
    yy = 1.0/tan(xx/2.0);

	/* Lommel-Seeliger soil phase function (not normalised) : */

    bur2 = 0.5 * params[0] * (1.0 - sin(xx) * tan(xx) * log(yy));
  }

  xx = 1.0 - params[0];
  yy = xx * 2.0/3.0;
  bur3 = (sin(phase) + (M_PI - phase) * arguments[2])/M_PI;

  burf = (yy + params[0]*params[4])*phi - yy * bur3;

  burf /= bur2;

/*   *result = A * ci/(ci+ce) * burf + (1.0 - A) * ci;  */

  *result = params[0] * lom * burf + xx * arguments[0]; 
}

/******************************************************************************

			MOSHER Function

 ******************************************************************************/

void MOSHER ( 
	double arguments[],
	double params[],
	double *result,
	char phoMode[phoMax_MODE_NAME_LENGTH+1])

/***********************************************************************
 * Dim(arguments)= 3    arguments[0] = ci = cos(i)
 *			arguments[1] = ce = cos(e)
 *			arguments[2] = cg = cos(phase angle)
 * Dim(params)= 6	params[0..5] = photometric function parameters
 *			  params[0] = A	= A_VEVERKA
 *			  params[1] = B	= B_VEVERKA
 *			  params[2] = C	= C_VEVERKA
 *			  params[3] = D	= D_VEVRKA
 *			  params[4] = E	= MO_EXP1
 *			  params[5] = F	= MO_EXP2
 ************************************************************************/

{
  double phase, xx, xk, squ, cice;
 
  if(!EQUAL(phoMode,phoVALUE)) \
    zmabend("For this function, only phoMode=value is implemented ");

  phase = acos(arguments[2]);

  xx  = exp(-params[3] * phase);

	/* Squyres-Veverka soil phase function : */

  squ = params[0] + params[1] * phase + params[2] * xx;

  cice = arguments[0] * arguments[1];
  xx = MAX(arguments[1], ANG_EPS);
  xk = params[4] + params[5]*phase; /* modification of Minnaert's exponent */

/*   *result = (A + B*phase + C*exp(-D*phase)) * pow(ci*ce, E+F*phase) / ce ;*/

  *result =  squ * ( pow(cice, xk) )/xx;
}

/******************************************************************************

			Mosher Function (C bridge)
			(the actual function is in pho_routines.f)

 ******************************************************************************/
/*
void MOSHER ( arguments, params, result,phoMode)
double arguments[];
double params[];
double *result;
char phoMode[phoMax_MODE_NAME_LENGTH+1])

{

  if(!EQUAL(phoMode,phoVALUE)) \
    zmabend("For this function, only phoMode=value is implemented ");

FTN_NAME2(xmosher, XMOSHER) ( arguments, params, result);

}
*/

/******************************************************************************

		for the most Hapke functions and the Lumme-Bowel function

 ******************************************************************************/

	/*  Chandrasekhar's H-function :*/
#define HFNCTN(a,b)      ((1.0 + 2.0*a)/(1.0 + 2.0*a*sqrt(1.0-b)))

	/* Cook's modification of the cosinus : */
#define COOK(a,b)        sqrt(1.0 - a * a * (1.0 - b * b))


/******************************************************************************

			hp_rough Subroutine

	This subroutine is called by the HAPKE functions to consider 
	the macroscopic roughness

  *cosi   is a pointer for cos(i)
  *cose   is a pointer for cos(e)
   cosg   is the cos(phase angle)
   tbar   is THETA in rad
 ******************************************************************************/

double hp_rough ( cosi, cose, cosg, tbar)
double *cosi, *cose, cosg, tbar;
{
  double sini, sine, csphi, psi, s2psi, f, y;
  double ci, ce, c0i, c0e, ei1, ei2, ee1, ee2;
  double tant, beta, cof, sfnct;

  if(tbar < 0.0) tbar *= -1.0;

  if(tbar > 1.7e-3) {


/* tbar > 0.1 degree, so rough-surface changes cosi and cose   */

    sini = sqrt(1.0 - *cosi * *cosi);
    sine = sqrt(1.0 - *cose * *cose);

    if(sini != 0.0 && sine != 0.0)
      csphi = (cosg - *cosi * *cose)/(sini * sine);
    else
      csphi = 1.0;

    if(csphi < -1.0) csphi = -1.0;
    if(csphi >  1.0) csphi =  1.0;

    s2psi = 0.5 * (1.0 - csphi);   /*  = sin(0.5*phi)*sin(0.5*phi);  */
    psi = acos(csphi);             /*  0 <= psi <= Pi                */


/* fraction of the visibility shadow hidden in the illumination shadow or */
/* fraction of the illumination shadow hidden in the visibility shadow  : */

    if(psi < 3.10)
      f = exp(-2.0 * tan(0.5 * psi));
    else
      f = 0.0;

    if(tbar > 1.4) tbar = 1.4;

    tant = tan(tbar);

    beta = sqrt(1.0 + M_PI * tant * tant);

    y = sini * tant;
    if(y > 1.0e-2) {
      ci = *cosi/y;
      ei1 = exp(-2.0 * ci/M_PI);
      ei2 = exp(-ci * ci/M_PI);
    }
    else  ei1 = ei2 = 0.0;

    y = sine * tant;
    if(y > 1.0e-2) {
      ce = *cose/y;
      ee1 = exp(-2.0 * ce/M_PI);
      ee2 = exp(-ce * ce/M_PI);
    }
    else  ee1 = ee2 = 0.0;

    ci = *cosi;
    ce = *cose;

    c0i = ci + sini * tant * ei2/(2.0 - ei1);
    c0i /= beta;
    c0e = ce + sine * tant * ee2/(2.0 - ee1);
    c0e /= beta;

    if(*cosi >= *cose) {                       /*   case    i <= e  */

      y = 2.0 - ee1 - (psi/M_PI) * ei1;

      *cosi += sini * tant * (csphi * ee2 + s2psi * ei2)/y;
      *cosi /= beta;

      *cose += sine * tant * (ee2 - s2psi * ei2)/y;
      *cose /= beta;

      y = ci/c0i;
    }
    else {                                     /*   case    i > e   */

      y = 2.0 - ei1 - (psi/M_PI) * ee1;

      *cosi += sini * tant * (ei2 - s2psi * ee2)/y;
      *cosi /= beta;

      *cose += sine * tant * (csphi * ei2 + s2psi * ee2)/y;
      *cose /= beta;

      y = ce/c0e;
    }

    cof = beta * (1.0 - f + f * y/beta);

    sfnct  = (*cose/c0e) * (ci/c0i)/cof;

  }
  else sfnct = 1.0;

  return sfnct;
}

/******************************************************************************

			LUMME_BOWEL_HG1 Funktion

 ******************************************************************************/

void LUMME_BOWEL_HG1 (    
	double arguments[],
	double params[],
	double *result,
	char phoMode[phoMax_MODE_NAME_LENGTH+1])

/***************************************************************
 * Dim(arguments)= 3    arguments[0] = ci = cos(i)
 *			arguments[1] = ce = cos(e)
 *			arguments[2] = cg = cos(phase angle)
 * Dim(params)= 5	photometric function parameters
 *			  params[0] = w		= W_SOIL
 *			  params[1] = h		= H_SHOE
 *			  params[2] = den_soil	= DEN_SOIL
 *			  params[3] = sigma	= THETA
 *			  params[4] = rho	= HG1_SOIL
 ***************************************************************/

{
  int i, j, itmax;
  double xx, yy, pg, lom, pp, hg, sh, rgh, r1, r2;


  if(!EQUAL(phoMode,phoVALUE)) \
    zmabend("For this function, only phoMode=value is implemented ");

	/* Henyey-Greenstein soil phase function : */ 

  xx = params[4] * params[4];
  pg = (1 - xx)/pow(1.0 + xx + 2.0 * params[4] * arguments[2], 1.5);
 
  xx = arguments[0] + arguments[1];
  if(xx < ANG_EPS) xx = ANG_EPS;

  lom = 0.5 * params[0] * arguments[0]/xx;

  r1  = arguments[0] * arguments[0];
  r1 += arguments[1] * arguments[1];
  r1 -= 2.0 * arguments[0] * arguments[1] * arguments[2];
  yy = pp = sqrt(r1);

  if(pp < 0.00001) pp = 0.00001;
  xx = -log(1.0 - params[2]) * xx / (2.4 * pp);

/*  if(xx > 7.0) itmax= 15; else itmax= 6;    empirisch ! */
  if (xx > 7.0) itmax = 8;
  else          itmax = 3;

  for(i=0, sh=pp=hg=1.0; i<itmax; i++) {
    pp *= xx;
    hg *= xx + i;
    sh += pp/hg;
  }
  sh *= 0.5;

  yy /= arguments[0] * arguments[1];

  rgh = (params[3]/(1.0 + params[1] * yy) + 1.0 - params[3]);

  r1  = lom * pg * sh * rgh;
	/* multiple scattering term : */
  r2  = HFNCTN(arguments[0], params[0]) * HFNCTN(arguments[1], params[0]) - 1.0;
  r2 *= 0.25 * params[0];

  *result =  r1 + r2;
}

/******************************************************************************

			HAPKE_81_LE2 Function

 ******************************************************************************/

void HAPKE_81_LE2 ( 
	double arguments[],
	double params[],
	double *result,
	char phoMode[phoMax_MODE_NAME_LENGTH+1])

/***************************************************************
 * Dim(arguments)= 3    arguments[0] = ci = cos(i)
 *			arguments[1] = ce = cos(e)
 *			arguments[2] = cg = cos(phase angle)
 * Dim(params)= 4	photometric function parameters
 *			  params[0] = w	= W_SOIL
 *			  params[1] = h	= H_SHOE
 *			  params[2] = b	= LE1_SOIL
 *			  params[3] = c	= LE2_SOIL
 ****************************************************************/

{
  double xx, lom, bg, tg, pg, hh;
 
  if(!EQUAL(phoMode,phoVALUE)) \
    zmabend("For this function, only phoMode=value is implemented ");

  xx = arguments[0] + arguments[1];
  if(xx < ANG_EPS)
    *result = 0.0;
  else {
    lom = 0.25 * params[0] * arguments[0]/xx;

    if(arguments[2] <= 0.0)                    /*   |phase| >= 0.5*M_PI) */
      bg = 0.0;
    else if(arguments[2] > 0.99995) {          /*  |g| < 0.01        */


/*
 *   phase << 1  ==>  |phase/2| = |sin(phase/2)| = sqrt(0.5*(1 - cg))
 */
      bg  = exp(-0.5*params[0]*params[0]);
      bg *= (1.0 - 3.0*sqrt(0.5*(1.0 - arguments[2]))/params[1]);
    }
    else
    {
      bg = exp(-0.5*params[0]*params[0]);
      tg = sqrt(1.0 - arguments[2]*arguments[2])/arguments[2];
      xx = exp(-params[1]/tg);
      bg *= (1.0 - 0.5 * tg * (3.0 - xx) * (1.0 - xx)/params[1]);
    }


/* Legendre Polynom : 1 + b * cos(a) + c * (3*cos(a)*cos(a) - 1)/2)    */

    hh = params[3] * (1.5 * arguments[2] * arguments[2] - 0.5);

    pg = 1.0 + params[2] * arguments[2] + hh;

    hh = HFNCTN(arguments[0],params[0]) * HFNCTN(arguments[1],params[0]);

    *result = lom * ((1.0 + bg) * pg + hh - 1.0);
  }
}

/******************************************************************************

			HAPKE_81_COOK Funktion

 ******************************************************************************/

void HAPKE_81_COOK (    
	double arguments[],
	double params[],
	double *result,
	char phoMode[phoMax_MODE_NAME_LENGTH+1])

/****************************************************************
 * Dim(arguments)= 3    arguments[0] = ci = cos(i)
 *			arguments[1] = ce = cos(e)
 *			arguments[2] = cg = cos(phase angle)
 * Dim(params)= 5	photometric function parameters
 *			  params[0] = w		= W_SOIL
 *			  params[1] = h		= H_SHOE
 *			  params[2] = b		= LE1_SOIL
 *			  params[3] = c		= LE2_SOIL
 *			  params[4] = cook	= COOK
 ****************************************************************/

{
  double xx, lom, bg, tg, pg, hh, ci, ce, w;
 
  if(!EQUAL(phoMode,phoVALUE)) \
    zmabend("For this function, only phoMode=value is implemented ");

  ci = COOK(params[4], arguments[0]);
  ce = COOK(params[4], arguments[1]);

  w = MIN(params[0], 1.0); 
  xx = ci + ce;
  if(xx < ANG_EPS)
    *result = 0.0;
  else {
    lom = 0.25 * w * ci/xx;

    if(arguments[2] <= 0.0)			/*  |phase| >= 0.5*M_PI)  */
      bg = 0.0;
    else {
      bg  = exp(-0.5*w*w);
      tg = sqrt(1.0 - arguments[2]*arguments[2])/arguments[2];
      tg /= params[1];
      if(tg < 0.025)
        bg *= 1.0 - 1.5 * tg;			/*  exp(-h/tan|phase|) << 1  */
      else {
        xx = exp(-1.0/tg);
        bg *= 1.0 - 0.5 * tg * (3.0 - xx) * (1.0 - xx);
      }
    }


/* Legendre Polynom : 1 + b * cos(a) + c * (3*cos(a)*cos(a) - 1)/2)    */

    hh = params[3] * (1.5 * arguments[2] * arguments[2] - 0.5);

    pg = 1.0 + params[2] * arguments[2] + hh;

    hh = HFNCTN(ci,w) * HFNCTN(ce,w);

    *result = lom * ((1.0 + bg) * pg + hh - 1.0);
  }
}

/******************************************************************************

			HAPKE_86_HG1 Funktion

 ******************************************************************************/

void HAPKE_86_HG1 (    
	double arguments[],
	double params[],
	double *result,
	char phoMode[phoMax_MODE_NAME_LENGTH+1])

/***************************************************************
 * Dim(arguments)= 3    arguments[0] = ci = cos(i)
 *			arguments[1] = ce = cos(e)
 *			arguments[2] = cg = cos(phase angle)
 * Dim(params)= 5	photometric function parameters
 *			  params[0] = w		= W_SOIL
 *			  params[1] = h		= H_SHOE
 *			  params[2] = B0	= B_SHOE
 *			  params[3] = THETA	= THETA
 *			  params[4] = hg1	= HG1_SOIL
 ***************************************************************/

{
  double cosi, cose, cosg, sfnct, bg, pg, cof;
  double y, hh;

  if(!EQUAL(phoMode,phoVALUE)) \
    zmabend("For this function, only phoMode=value is implemented ");

  cosi = arguments[0];
  cose = arguments[1];
  cosg = arguments[2];

  sfnct = hp_rough (&cosi, &cose, arguments[2], params[3]);

  if(params[0] > 0.99998) params[0] = 0.99998;	/* params[0] = w  */

/* determine B(g) function. */
  if(params[1] == 0.0)				/* params[1] = h  */
    bg = 0.0;
  else
  {


/* General is tan(x/2) = sqrt((1-cos(x))/(1+cos(x)) for 0 <= x < Pi.
 * for small angles the tan(x/2) can be developed by
 *     tan(x/2) = x/2 = sin(x/2) = sqrt((1 - cos(x))/2)
 */

    if(cosg < -0.999999) cosg = -0.999999;
    y = sqrt((1-cosg)/(1+cosg));
    y /= params[1];
    bg = params[2]/(1.0 + y);
  }


/* determine P(g); average single particle scattering function
   N O T I C E:  cos(Pi - a) = -cos(a) */

  y = 1.0 + params[4] * (params[4] + 2.0*cosg);
  y = -1.5 * log(y);

  pg = (1.0 - params[4] * params[4]) * exp(y);


  cof = 0.25 * params[0] * (cosi/(cosi + cose));

  hh = HFNCTN(cosi, params[0]) * HFNCTN(cose, params[0]);

  *result = cof * ((1.0+bg) * pg + hh - 1.0) * sfnct;
}

/******************************************************************************

			HAPKE_86_HG2 Funktion

 ******************************************************************************/

void HAPKE_86_HG2 (   
	double arguments[],
	double params[],
	double *result,
	char phoMode[phoMax_MODE_NAME_LENGTH+1])

/*****************************************************************
 * Dim(arguments)= 3    arguments[0] = ci = cos(i)
 *			arguments[1] = ce = cos(e)
 *			arguments[2] = cg = cos(phase angle)
 * Dim(params)= 7	photometric function parameters
 *			  params[0] = w		= W_SOIL
 *			  params[1] = h		= H_SHOE
 *			  params[2] = B0	= B_SHOE
 *			  params[3] = THETA	= THETA
 *			  params[4] = hg1	= HG1_SOIL
 *			  params[5] = hg2	= HG1_SOIL
 *			  params[6] = hg_asy	= HG_ASY_SOIL
 *****************************************************************/

{
  double cosi, cose, cosg, sfnct, bg, pg, cof;
  double y, hg1, hg2, hh;

  if(!EQUAL(phoMode,phoVALUE)) \
    zmabend("For this function, only phoMode=value is implemented ");

  cosi = arguments[0];
  cose = arguments[1];
  cosg = arguments[2];

  sfnct = hp_rough (&cosi, &cose, arguments[2], params[3]);

  if(params[0] > 0.99998) params[0] = 0.99998;	/* params[0] = w  */

/* determine B(g) function. */
  if(params[1] == 0.0)				/* params[1] = h  */
    bg = 0.0;
  else
  {


/* General is tan(x/2) = sqrt((1-cos(x))/(1+cos(x)) for 0 <= x < Pi.
 * for small angles the tan(x/2) can be developed by
 *     tan(x/2) = x/2 = sin(x/2) = sqrt((1 - cos(x))/2)
 */

    if(cosg < -0.999999) cosg = -0.999999;
    y = sqrt((1-cosg)/(1+cosg));
    y /= params[1];
    bg = params[2]/(1.0 + y);
  }


/* determine P(g); average single particle scattering function
   N O T I C E:  cos(Pi - a) = -cos(a) */

  y = 1.0 + params[4] * (params[4] + 2.0*cosg);
  y = -1.5 * log(y);
  hg1 = (1.0 - params[4] * params[4]) * exp(y);

  y = 1.0 + params[5] * (params[5] + 2.0*cosg);
  y = -1.5 * log(y);
  hg2 = (1.0 - params[5] * params[5]) * exp(y);

  pg = params[6] * hg1 + (1.0 - params[6]) * hg2;


  cof = 0.25 * params[0] * (cosi/(cosi + cose));

  hh = HFNCTN(cosi, params[0]) * HFNCTN(cose, params[0]);

  *result = cof * ((1.0+bg) * pg + hh - 1.0) * sfnct;
}

/******************************************************************************

			HAPKE_HG1_DOM Funktion

 ******************************************************************************/

void HAPKE_HG1_DOM (   
	double arguments[],
	double params[],
	double *result,
	char phoMode[phoMax_MODE_NAME_LENGTH+1])

/*****************************************************************
 * Dim(arguments)= 3    arguments[0] = ci = cos(i)
 *			arguments[1] = ce = cos(e)
 *			arguments[2] = cg = cos(phase angle)
 * Dim(params)= 7	photometric function parameters
 *			  params[0] = w		= W_SOIL
 *			  params[1] = h_shoe	= H_SHOE
 *			  params[2] = B0_shoe	= B_SHOE
 *			  params[3] = THETA	= THETA
 *			  params[4] = hg1	= HG1_SOIL
 *			  params[5] = h_cboe	= H_CBOE
 *			  params[6] = B0_cboe	= B_CBOE
 ******************************************************************/

{
  double cosi, cose, cosg, sfnct, bsh, bcb, pg, cof;
  double y, hh;

  if(!EQUAL(phoMode,phoVALUE)) \
    zmabend("For this function, only phoMode=value is implemented ");

  cosi = arguments[0];
  cose = arguments[1];
  cosg = arguments[2];

  sfnct = hp_rough (&cosi, &cose, arguments[2], params[3]);

  if(params[0] > 0.99998) params[0] = 0.99998;	/* params[0] = w  */

/* determine Bsh(g) shadow function. */
  if(params[1] == 0.0)				/* params[1] = h  */
    bsh = 0.0;
  else
  {


/* General is tan(x/2) = sqrt((1-cos(x))/(1+cos(x)) for 0 <= x < Pi.
 * for small angles the tan(x/2) can be developed by
 *     tan(x/2) = x/2 = sin(x/2) = sqrt((1 - cos(x))/2)
 */

    if(cosg < -0.999999) cosg = -0.999999;
    y = sqrt((1-cosg)/(1+cosg));
    y /= params[1];
    bsh = params[2]/(1.0 + y);
  }


/* determine Bcb(g) coherent function. */
  if(params[5] == 0.0)				/* params[1] = h  */
    bcb = 0.0;
  else
  {
    if(cosg < -0.999999) cosg = -0.999999;
    y = sqrt((1-cosg)/(1+cosg));
    y /= params[5];
    bcb = params[6]/(1.0 + y);
  }


/* determine P(g); average single particle scattering function
   N O T I C E:  cos(Pi - a) = -cos(a) */

  y = 1.0 + params[4] * (params[4] + 2.0*cosg);
  y = -1.5 * log(y);

  pg = (1.0 - params[4] * params[4]) * exp(y);


  cof = 0.25 * params[0] * (cosi/(cosi + cose));

  hh = HFNCTN(cosi, params[0]) * HFNCTN(cose, params[0]);

  *result = cof * ((1.0+bsh) * pg + (1.0 + bcb) * (hh - 1.0)) * sfnct;
}

/******************************************************************************

			REGNER_HAPKE_HG1 Funktion

 ******************************************************************************/

void REGNER_HAPKE_HG1 (   
	double arguments[],
	double params[],
	double *result)
{
  zvmessage("REGNER_HAPKE_HG1 is not build in yet","");
  *result = 1.0;
}

/******************************************************************************

			insert here more photometric functions

 ******************************************************************************/

/******************************************************************************

			Lommel-Seeliger Law

 ******************************************************************************/

void LOM_SEEL ( 
	double arguments[],
	double params[],
	double *result,
	char phoMode[phoMax_MODE_NAME_LENGTH+1])

/***********************************************************
 * Dim(arguments)=3  	arguments[0] = ci = cos(i)
 *		        arguments[1] = ce = cos(e)
 *			arguments[2] = cg = cos(g)
 * Dim(params)=2
 ***********************************************************/

{
  double sum, pg;

  if(!EQUAL(phoMode,phoVALUE)) \
    zmabend("For this function, only phoMode=value is implemented ");

  sum = arguments[0] + arguments[1];
  if(sum < ANG_EPS) sum = ANG_EPS;

/*  *result = ci / ( ci + ce ) * pg;  */
/* pg = 4*pi/5*{ [sin(g)+(pi-g)*cg]/pi + [(1-cg)**2]/10 } */

/*   pg = .8 * M_PI* ( ( sqr(1. - arguments[2] * arguments[2] ) +( M_PI - acos(arguments[2] ) ) * / M_PI + (1. - arguments[2] ) * (1. - arguments[2] ) / 10. );
*/
   pg =1.0;

  *result = pg * arguments[0] / sum;  


}

/******************************************************************************

			Squyres-Veverka soil phase function

 ******************************************************************************/

void SQUE_VEV ( 
	double arguments[],
	double params[],
	double *result,
	char phoMode[phoMax_MODE_NAME_LENGTH+1])

/***********************************************************************
 * Dim(arguments)=1  	arguments[2] = cg = cos(phase angle)
 *  Dim(params)=4       params[0..3]= photometric function parameters
 *			  params[0] = A_VEVERKA
 *			  params[1] = B_VEVERKA
 *			  params[2] = C_VEVERKA
 *			  params[3] = D_VEVERKA
 ***********************************************************************/

{
  double phase, cc;

  if(!EQUAL(phoMode,phoVALUE)) \
    zmabend("For this function, only phoMode=value is implemented ");

  phase = acos(arguments[2]);


/*  av_param = params[0];    bv_param = params[1];
    cv_param = params[2];    dv_param = params[3];

   *result = A_VEVERKA + B_VEVERKA * g + C_VEVERKA * cc;
*/

  cc = exp(- 1.0 * params[3] * phase);

  *result = params[0] + params[1] * phase + params[2] * cc;
}


/******************************************************************************/

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create pho_bidirefs.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include "pho_private.h"	/* definitions of the pho_object and ...    */

#define HFNCT1(a,b)       ((1.0 + 2.0*a)/(1.0 + 2.0*a*b))
#define EPS_HX_DIE       0.00001
#define EPS_HX_DA        0.00001
#define EPS_THETA	 1.7e-3  /* THETA > 0.1 degree  		*/
#define MAX_THETA	 1.4	 /* THETA < 80 degree   		*/
#define MAX_AZIMUTH	 3.10	 /* AZIMUTH < 177.6 degree 		*/


/******************************************************************************
	*****************************************************************
	*								*
	*	Subroutines needed for phoFunction			*
	*								*
	*****************************************************************


 N O T I C E, that the subroutines doesn't contain any check if the angles are
 greater than 90 degrees (if the cos(angle) > 0.0). You have to do it before.
 Only a possible division by zero is prevented.

 N O T I C E the angle unit is rad.

 The arguments are equal for all functions and are not changed.

 Parameter
   x    Input of the observation geometrie
 	N O T I C E the angle unit is rad.
   a    Input of the photometric parameters
   res  result array for the function value and the derivations.
	The dimension has to be: Dim(res) = 4 + Dim(a)
	res[0]   = function value
	res[1]   = d res[0]/ d cos(i)
	res[2]   = d res[0]/ d cos(e)
	res[3]   = d res[0]/ d phas
  	res[4+i] = d res[0]/ d a[ i ]   ( i = 0, 1, ..)

 
*******************************************************************************/

/******************************************************************************

			LAMBERT Function

 ******************************************************************************/

void LAMBERT (
	double *x,
	double *a,
	double *res,
	char phoMode[phoMax_MODE_NAME_LENGTH+1])

/**********************************************************
 * 	Dim(arguments)=1 	arguments[0]= ci = cos(i)
 *	Dim(param)=1		params[0]= ALBEDO 
 **********************************************************/

{


  res[0] = a[0] * x[0];		/*  res[0] = albedo * ci           	*/
  if( EQUAL(phoVALUE,phoMode) ) return;

  if( !EQUAL(phoPAR_DEV,phoMode) ) 
  {
    res[1] = a[0];		/*  res[1] = d res[0]/ d cos(i)   	*/
    res[2] = 0.0;		/*  independent				*/
  }

  if( EQUAL(phoDIR_DEV,phoMode) ||  EQUAL(phoALL,phoMode) ) 
  {
    res[3] = 0.0;		/*  independent				*/
  }

 if( EQUAL(phoPAR_DEV,phoMode) ||  EQUAL(phoALL,phoMode) ) 
  {
    res[4] = x[0];		/*  res[4] = d res[0]/ d albedo 	*/
    return;
  }

  return;
}

/******************************************************************************

			Minnaert Funktion

 ******************************************************************************/


void MINNAERT ( 
	double *x,
	double *a,
	double *res,
	char phoMode[phoMax_MODE_NAME_LENGTH+1])

/****************************************************
 * Dim(arguments)=2  	x[0] = ci = cos(i)
 *			x[1] = ce = cos(e)
 * Dim(params)=2	a[0] = B0 = Albedo
 * 			a[1] = k  = Exponent
 ****************************************************/

{
  double ce, cice, power;
 
  ce = MAX(x[1], ANG_EPS);
  cice = x[0] * ce;
  power = pow(cice, a[1]) / ce;


  res[0]  = a[0] * power; 	/* res = B0*pow(ci*ce,k)/ce; 		*/
  if( EQUAL(phoVALUE,phoMode) ) return;
 
  if( !EQUAL(phoPAR_DEV,phoMode) ) 
  {
    res[1]  = a[1] * res[0]/MAX(x[0], ANG_EPS); 
    res[2]  = (a[1] - 1.0) * res[0]/ ce;
  }

  if( EQUAL(phoDIR_DEV,phoMode) ||  EQUAL(phoALL,phoMode) ) 
  {
    res[3] = 0.0;		/*  independent				*/
  }

 if( EQUAL(phoPAR_DEV,phoMode) ||  EQUAL(phoALL,phoMode) ) 
  {
    res[4] = power; 		/* res[4] = d res[0]/ d B0		*/
    res[5] = res[0] * log(cice);   /* res[5] = d res[0]/ d k		*/
    return;
  }

  return;
}

/******************************************************************************

			IRVINE Function

 ******************************************************************************/

void IRVINE ( 
	double *x,
	double *a,
	double *res,
	char phoMode[phoMax_MODE_NAME_LENGTH+1])

/***********************************************************************
 * Dim(arguments)= 2    x[0]    = ci    = cos(i)
 *			x[1]    = ce    = cos(e)
 * Dim(params)= 3	a[0..2] = photometric function parameters
 * 			a[0]    = k	= EXPONENT
 * 			a[1]    = B	= IRV_EXP1
 * 			a[2]    = C	= IRV_EXP2
*************************************************************************/

{
  double ce, cice, bz, bn, exn, exz, power;
 
  ce = MAX(x[1], ANG_EPS);
  cice = x[0] * ce;
  exn = exp(-x[1]*a[2]);
  bz = 1.0 - exn;
  bn = MAX(bz, ANG_EPS);
  exz = exp(-x[0]*a[1]);
  bz = 1.0 - exz;
  power = pow(cice, a[0]);

/*   *res = pow( ci*ce, k )/ce * (1-exp(-ci*B))/(1-exp(-ce*C));  */

  res[0] =  power/ce * bz/bn;

  if( EQUAL(phoVALUE,phoMode) ) return;

  if( !EQUAL(phoPAR_DEV,phoMode) ) 
  {
    res[1] = power / ce / bn * ( 1.0 - (1.0 - a[1]) * exz);
    res[2] = res[0] * ( (a[0]-2.0)/x[1] - a[2] * exn / bn );  
  }

  if( EQUAL(phoDIR_DEV,phoMode) ||  EQUAL(phoALL,phoMode) ) 
  {
    res[3] = 0.0;		/*  independent				*/
  }

 if( EQUAL(phoPAR_DEV,phoMode) ||  EQUAL(phoALL,phoMode) ) 
  {
    res[4] = res[0] * log(cice);	/*  res[4] = d res[0]/ d k 	*/
    res[5] = power/ce * a[1] * exz / bn;
    res[6] = - res[0] * a[2] * exn / bn;
    return;
  }

  return;
}

/******************************************************************************

			Veverka Funktion

 ******************************************************************************/

void VEVERKA ( 
	double *x,
	double *a,
	double *res,
	char phoMode[phoMax_MODE_NAME_LENGTH+1])

/***********************************************************************
 * Dim(arguments)= 3    x[0] = ci = cos(i)
 *			x[1] = ce = cos(e)
 *			x[2] = cg = cos(phase angle)
 * Dim(params)= 4	a[0..3] = photometric function parameters
 *			  a[0] = A = A-VEVERKA
 *			  a[1] = B = B_VEVERKA
 *			  a[2] = C = C_VEVERKA
 * 			  a[3] = D = D_VEVERKA
 ************************************************************************/

{
   double phase, sum, s2, xe, xf, lom;

  phase = RETURN_DEGREES(acos(x[2]));
  xe = exp(-a[3] * phase);
  xf = a[0] + a[1] * phase + a[2] * xe;

  sum = x[0] + x[1];
  if(sum < ANG_EPS) sum = ANG_EPS;
  s2 = sum * sum;
  lom = x[0]/sum;

  res[0] = lom * xf;
  if( EQUAL(phoVALUE,phoMode) ) return;

  if( !EQUAL(phoPAR_DEV,phoMode) ) 
  {
  res[1] = xf*x[1]/s2;	/*  res[1] = d res[0]/ d cos(i)   	*/
  res[2] = -res[0]/sum;	/*  res[2] = d res[0]/ d cos(e)   	*/
  }


  if( EQUAL(phoDIR_DEV,phoMode) ||  EQUAL(phoALL,phoMode) ) 
  {
    res[3] = lom * ( a[1] - a[2] * a[3] * xe);	/*  res[3] = d res[0]/ d phas */
  }

 if( EQUAL(phoPAR_DEV,phoMode) ||  EQUAL(phoALL,phoMode) ) 
  {
    res[4] = x[0]/sum;			        /*  res[4] = d res[0]/ d A */
    res[5] = res[4] * phase;		        /*  res[5] = d res[0]/ d B */
    res[6] = res[4] * xe;		        /*  res[6] = d res[0]/ d C */
    res[7] = -res[6] * a[2] * phase;	        /*  res[7] = d res[0]/ d D */
    return;
  }

  return;
}

/******************************************************************************

			HAPKE_86_LE2 Funktion

 ******************************************************************************/

void HAPKE_86_LE2 (   
	double *x,
	double *a,
	double *res,
	char phoMode[phoMax_MODE_NAME_LENGTH+1])

/*****************************************************************
 * Dim(arguments)= 3    arguments[0] = ci = cos(i)
 *			arguments[1] = ce = cos(e)
 *			arguments[2] = cg = cos(phase angle)
 * Dim(params)= 6	photometric function parameters
 *			  params[0] = w		= W_SOIL
 *			  params[1] = h		= H_SHOE
 *			  params[2] = B0	= B-SHOE
 *			  params[3] = THETA	= THETA(in grad)
 *			  params[4] = b		= LE1_SOIL
 *			  params[5] = c		= LE2_SOIL
 *****************************************************************/

{
int flag;
double dpg;





  if( EQUAL(phoVALUE,phoMode) ) /* res[0] */
  {
    flag = 0;

   /*	cos(phase angle) = x[2].
	Legendre Polynom : 1 + b * cos(a) + c * (3*cos(a)*cos(a) - 1)/2)  */

    res[0] = 1.0 + a[4] * x[2] + a[5] * (1.5 * x[2] * x[2] - 0.5);

    pho_HAPKE86(x, a, res, flag);

    return;
  }






  if( EQUAL(phoGRAD,phoMode) ) 
  {
  /* res[0] and res[1] = d res[0] / d cosi,
                res[2] = d res[0] / d cose  */
    flag = 2;

    /* Legendre Polynom : 1 + b * cos(a) + c * (3*cos(a)*cos(a) - 1)/2)    */
    res[0] = 1.0 + a[4] * x[2] + a[5] * (1.5 * x[2] * x[2] - 0.5);

    pho_HAPKE86(x, a, res, flag);

    return;
  }






  if( EQUAL(phoDIR_DEV,phoMode) ) 
  {
  /* res[0] and res[1] = d res[0] / d cosi,
		res[2] = d res[0] / d cose,
		res[3] = d res[0] / d Tetha  not yet implemented */

    zmabend(" ****phoDIR_DEV not yet implemented**** ");

    flag = 4;

    /* Legendre Polynom : 1 + b * cos(a) + c * (3*cos(a)*cos(a) - 1)/2)    */
    res[0] = 1.0 + a[4] * x[2] + a[5] * (1.5 * x[2] * x[2] - 0.5);

    pho_HAPKE86(x, a, res, flag);


    return;
  }


  if( !EQUAL(phoPAR_DEV,phoMode) ) 
  {
  /* res[0] and the parameter derivations
	res[4] = d res[0] / d w,
	res[5] = d res[0] / d h,
	res[6] = d res[0] / d Bo,
	res[7] = d res[0] / d Theta  not yet implemented ,
	res[8] = d res[0] / d b,
	res[9] = d res[0] / d c  */

    flag = 1;

    /* Legendre Polynom : 1 + b * cos(a) + c * (3*cos(a)*cos(a) - 1)/2)    */

    dpg = 1.5 * x[2] * x[2] - 0.5;
    res[0] = 1.0 + a[4] * x[2] + a[5] * dpg;


    pho_HAPKE86(x, a, res, flag);


    /*	Derivation dHapke/dc = dHapke/da5 = res[9] */

    res[9] = res[8] * dpg;


    /*	Derivation dHapke/db = dHapke/da4 = res[8] */

    res[8] *= x[2];

  return;


  }


 if( EQUAL(phoALL,phoMode) ) 
  {
  /* res[0] and res[1] = d res[0] / d cosi,
		res[2] = d res[0] / d cose,
		res[3] = d res[0] / d Tetha and the parameter derivations 
		res[4] = d res[0] / d w,
		res[5] = d res[0] / d h,
		res[6] = d res[0] / d Bo,
		res[7] = d res[0] / d Theta,  not yet implemented
		res[8] = d res[0] / d b,
		res[9] = d res[0] / d c  */

    flag = 3;


    /* Legendre Polynom : 1 + b * cos(a) + c * (3*cos(a)*cos(a) - 1)/2)    */

    dpg = 1.5 * x[2] * x[2] - 0.5;
    res[0] = 1.0 + a[4] * x[2] + a[5] * dpg;


    pho_HAPKE86(x, a, res, flag);


    /*	Derivation dHapke/dc = dHapke/da5 = res[8] */

    res[9] = res[8] * dpg;


    /*	Derivation dHapke/db = dHapke/da4 = res[7] */

    res[8] *= x[2];

		
    return;
  }

  return;
}





/******************************************************************************

		common HAPKE 86

  The INPUT parameter flag controls the result calculation:
  flag = 0  res[0] only
  flag = 1  res[0] and the parameter derivations d res[0]/ds[i]
  flag = 2  res[0], res[1] and res[2]
  flag = 3  res[0], res[1], res[2] and the parameter derivations
  flag = 4  res[0], res[1], res[2], res[3] 

  res[0] has to contain the value of the single scattering function. 

 ******************************************************************************/

void pho_HAPKE86 (
	double *x,
	double *a, 
	double *res,
	int flag)
{
  double xx, tg, sqw, pg, Theta;
  double cosi, cose, cosg, sfnct, di[3], de[3], dg[3], da[3], bg, dbgdh, dbgdb;
  double hi, dih, dhida, he, deh, dheda, cof, dac, dic, dec, rbr, dir, der;

  cosi = x[0];
  cose = x[1];
  cosg = x[2];
  pg = res[0];
  Theta = RETURN_RADIANS(a[3]);

  if(a[0] > 1.0) a[0] = 1.0;		/* a[0] = w  */
  sqw  = sqrt(1.0 - a[0]);

  if(flag & 3) sfnct = pho_dg_rough(&cosi, &cose, cosg, Theta, di, de, dg, da, flag);
  else         sfnct = pho_hp_rough(&cosi, &cose, cosg, Theta);

/*	Determine the backscattering function */
  if(a[1] == 0.0)  bg = dbgdh = dbgdb = 0.0;
  else {
/*	General is tan(x/2) = sqrt[(1-cos(x))/(1+cos(x))] for 0 <= x < Pi. */
    xx = MAX(cosg, -0.99999999);
    tg = sqrt((1.0-xx)/(1.0+xx)); 
    if(flag & 1) {
      xx = a[1] + tg;
      dbgdh = a[2] * tg / (xx * xx);
      dbgdb = 1.0/(1.0 + tg/a[1]);
      bg = a[2] * dbgdb;
    }
    else {
      tg /= a[1];
      bg = a[2]/(1.0 + tg);
    }
  }

  xx = cosi + cose;
  if(xx < ANG_EPS) xx = ANG_EPS;

/*	COEF_FAC = 1.0/(4 * M_PI) */

  tg = COEF_FAC / xx;
  dac = cosi * tg;
  cof = a[0] * dac;

  if(flag & 3) {
    tg *= a[0] / xx;
    dic = cose * tg;
    dec = -cosi * tg;

    hi = pho_d_h_fct (cosi, sqw, &dih, &dhida);
    he = pho_d_h_fct (cose, sqw, &deh, &dheda);

    rbr = (1.0 + bg) * pg + hi * he - 1.0;
    dir = (dic * rbr + cof * dih * he) * sfnct;
    der = (dec * rbr + cof * hi * deh) * sfnct;
  }  
  else
    rbr = (1.0 + bg) * pg + HFNCT1(cosi, sqw) * HFNCT1(cose, sqw) - 1.0;

/*	The function value */
  res[0] = cof * rbr * sfnct * M_PI;

/*	Derivation dHapke/dcosi and dHapke/dcose */
  if(flag & 2) {
    res[1] = (dir * di[1] + der * di[2] + cof * rbr * di[0]) * M_PI;
    res[2] = (dir * de[1] + der * de[2] + cof * rbr * de[0]) * M_PI;
  }

  if(flag & 1) {
/*	Derivation dHapke/dw = dHapke/da0 = res[4] */
    res[4] = (dac * rbr + cof * (dhida * he + hi * dheda)) * sfnct * M_PI;

/*	Derivation dHapke/dh = dHapke/da1 = res[5] */
    xx = cof * sfnct * pg;
    res[5] = xx * dbgdh * M_PI;

/*	Derivation dHapke/dBo = dHapke/da2 = res[6] */
    res[6] = xx * dbgdb * M_PI;

/*	Derivation dHapke/dtheta = dHapke/da3 = res[7] */
    res[7] = (dir * da[1] + der * da[2] + cof * rbr * da[0]) * M_PI;

/*	Single scattering parameter derivation factor =  = res[8]. */
    res[8] = cof * (1.0 + bg) * sfnct * M_PI;
  }

  return;
}



/******************************************************************************

			pho_hp_rough Subroutine

	This subroutine is called by the HAPKE functions to consider 
	the macroscopic roughness

  *cosi   is a pointer for cos(i)
  *cose   is a pointer for cos(e)
   cosg   is the cos(phase angle)
   tbar   is THETA in rad
 ******************************************************************************/

double pho_hp_rough ( 
	double *cosi,
	double *cose, 
	double cosg, 
	double tbar)
{
  double sini, sine, csphi, psi, s2psi, f, y;
  double ci, ce, c0i, c0e, ei1, ei2, ee1, ee2;
  double tant, beta, cof, sfnct;

  if(*cosi > 1.0) *cosi = 1.0;
  if(*cose > 1.0) *cose = 1.0;

  if(tbar < 0.0) tbar *= -1.0;
  if(tbar > EPS_THETA) {
/*   tbar > 0.1 degree, so rough-surface changes cosi and cose   */

    sini = sqrt(1.0 - *cosi * *cosi);
    sine = sqrt(1.0 - *cose * *cose);

    if(sini != 0.0 && sine != 0.0)
      csphi = (cosg - *cosi * *cose)/(sini * sine);
    else
      csphi = 1.0;

    if(csphi < -1.0) csphi = -1.0;
    if(csphi >  1.0) csphi =  1.0;

    s2psi = 0.5 * (1.0 - csphi);   /*  = sin(0.5*phi)*sin(0.5*phi);  */
    psi = acos(csphi);             /*  0 <= psi <= Pi                */

    if(psi < MAX_AZIMUTH)  f = exp(-2.0 * tan(0.5 * psi));
    else            	    f = 0.0;

    if(tbar > MAX_THETA) tbar = MAX_THETA;

    tant = tan(tbar);
    beta = sqrt(1.0 + M_PI * tant * tant);

    y = sini * tant;
    if(y > 1.0e-2) {
      ci = *cosi/y;
      ei1 = exp(-2.0 * ci/M_PI);
      ei2 = exp(-ci * ci/M_PI);
    }
    else  ei1 = ei2 = 0.0;

    y = sine * tant;
    if(y > 1.0e-2) {
      ce = *cose/y;
      ee1 = exp(-2.0 * ce/M_PI);
      ee2 = exp(-ce * ce/M_PI);
    }
    else  ee1 = ee2 = 0.0;

    ci = *cosi;
    ce = *cose;
    c0i = ci + sini * tant * ei2/(2.0 - ei1);
    c0i /= beta;
    c0e = ce + sine * tant * ee2/(2.0 - ee1);
    c0e /= beta;

    if(*cosi >= *cose) {                       /*   case    i <= e  */
      y = 2.0 - ee1 - (psi/M_PI) * ei1;
      *cosi += sini * tant * (csphi * ee2 + s2psi * ei2)/y;
      *cosi /= beta;
      *cose += sine * tant * (ee2 - s2psi * ei2)/y;
      *cose /= beta;
      y = ci/c0i;
    }
    else {                                     /*   case    i > e   */
      y = 2.0 - ei1 - (psi/M_PI) * ee1;
      *cosi += sini * tant * (ei2 - s2psi * ee2)/y;
      *cosi /= beta;
      *cose += sine * tant * (csphi * ei2 + s2psi * ee2)/y;
      *cose /= beta;
      y = ce/c0e;
    }

    cof = beta * (1.0 - f + f * y/beta);
    sfnct  = (*cose/c0e) * (ci/c0i)/cof;

  }
  else sfnct = 1.0;

  return(sfnct);
}


/******************************************************************************

		derivations of the Roughness Function

  *ci   is a pointer for cos(i)
  *ce   is a pointer for cos(e)
   cg   is the cos(phase angle)
   tbar is THETA in rad
  di[0] expects the derivation d sfnct / d cos(i)
  di[1] expects the derivation d cos(i`) / d cos(i)
  di[2] expects the derivation d cos(e`) / d cos(i)
  de[0] expects the derivation d sfnct / d cos(e)
  de[1] expects the derivation d cos(i`) / d cos(e)
  de[2] expects the derivation d cos(e`) / d cos(e)
  da[0] expects the derivation d sfnct / d tbar
  da[1] expects the derivation d cos(i`) / d tbar
  da[2] expects the derivation d cos(e`) / d tbar
  dg[0] expects the derivation d sfnct /d phase		not yet implemented
  dg[1] expects the derivation d cos(i`) / d phase	not yet implemented
  dg[2] expects the derivation d cos(e`) / d phase	not yet implemented
  flag = 1  di and de are calculated
  flag = 2  da is calculated
  flag = 3  di, de and da are calculated
  flag = 4  dg not yet implemented
 ******************************************************************************/

double pho_dg_rough (
	double *ci,
	double *ce, 
	double cg, 
	double tbar, 
	double *di, 
	double *de, 
	double *dg,
	double *da,
	int flag)
{
  double sfnct, h1ci, h1ce, h2ci, h2ce, h3ci, h3ce, hx;

  h1ci = h2ci = h3ci = *ci;
  h1ce = h2ce = h3ce = *ce;
  sfnct = pho_hp_rough (ci, ce, cg, tbar);

  if(flag & 2) {
    hx = EPS_HX_DIE;
    if(h3ci < hx) hx *= -1;
    h1ci -= hx;
    di[0] = (sfnct - pho_hp_rough(&h1ci, &h1ce, cg, tbar))/hx;
    di[1] = (*ci - h1ci)/hx;
    di[2] = (*ce - h1ce)/hx;

    hx = EPS_HX_DIE;
    if(h3ce < hx) hx *= -1;
    h2ce -= hx;
    de[0] = (sfnct - pho_hp_rough (&h2ci, &h2ce, cg, tbar))/hx;
    de[1] = (*ci - h2ci)/hx;
    de[2] = (*ce - h2ce)/hx;
  }

  if(flag & 1) {
    hx = EPS_HX_DA;
    tbar += hx;
    da[0] = (pho_hp_rough (&h3ci, &h3ce, cg, tbar) - sfnct)/hx;
    da[1] = (h3ci - *ci)/hx;
    da[2] = (h3ce - *ce)/hx;
  }

  return(sfnct);
}

/******************************************************************************

    	pho_d_h_fct	derivation of the h function

  a       cos of the considered angle
  sqw     sqrt(1 - w)
  *da     expects the derivation d h_fct/da
  *dw     expects the derivation d w_fct/dw
 ******************************************************************************/

double pho_d_h_fct (
	double a, 
	double sqw, 
	double *da, 
	double *dw)
{
  double xx, tg, hh;

  xx = 1.0 + 2.0 * a * sqw;
  tg = sqw * xx;
  hh = (1 + 2.0 * a) / xx;
  dw[0] = a * hh / tg;
  da[0] = 2.0 * (1.0 - sqw)/(xx * xx);

  return(hh);
}

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create pho_routinesf.f
$ DECK/DOLLARS="$ VOKAGLEVE"
c******************************************************************************
c
c			Mosher Function 
c		(called by the C bridge in pho_routines.c)
c	
c ******************************************************************************
	SUBROUTINE XMOSHER( arg, con, ref)

	INCLUDE 'pho'

	double precision arg(3), con(6), ref

        CI = COS(con(1)*rad_deg)
        CE = COS(con(2)*rad_deg)
	xg = con(3)
        REF = (CON(1)+CON(2)*XG+CON(3)*EXP(-CON(4)*XG)) *
     &   (CI*CE)**(CON(5)+CON(6)*XG)/CE

	return
	end

c ******************************************************************************
  
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create pho_private.h
$ DECK/DOLLARS="$ VOKAGLEVE"
	/*
	 *   PHO_PRIVATE.H
	 *
	 * Photometric Data Object Include File
	 * 
	 * Purpose:
	 *
	 * This is an include file that is used by all routines of the 
	 * 'phoX...' set of photometric function correction software. 
	 * It includes the definition of the data object proper.
	 * The pho_init.h includes the definition of the codes needed 
	 * to process it .
	 * 
	 * These are private include files which are *not* to be included 
	 * in programs or subroutines which themselves call the 'phoX...' 
	 * functions.
	 * 
	 * To add a new function:
	 *  1. Add the function definition to PHO_ROUTINES.COM 
	 *    corresponding to the call in phoFunctions (in PHO_ROUTINES.COM).
	 *  2. Increase parameter phoFUNCTION_COUNT by 1.
	 *  3. Find all arrays with dimension phoFUNCTION_COUNT and
	 *    add an element to each array.
	 *  4. Add the function to the list of '#define's in PHO.H.
	 *  5. Check if parameter phoMAX_PARAM_COUNT increases, and if
	 *    so repeat steps 2 (may need to be increased by more than 1)
	 *    and 3 for this parameter.
	 *  6. Check if parameter phoMAX_PARAM_PER_FUNC (in pho.h and
	 *    pho.fin) increases,
	 *    and if so repeat steps 2 (may need to be increased by more
	 *    than 1) and 3 for this parameter.
	 *
	 * History:
	 *   05jan94 -lwk- initial version
	 */

#include <math.h>
#include "xvmaininc.h"		/* Standard VICAR Include File		    */
#include "pho.h"

		/*
		 * DEFINES of overall structure properties:
		 * Note:  phoMAX_FUNC_NAME_LENGTH and phoMAX_PARAM_PER_FUNC
		 * and phoMAX_KEYWD_LENGTH are defined in pho.h
		 */
#define	phoFUNCTION_COUNT	18	/* Number of phot.functions */
#define phoMAX_PARAM_COUNT	29	/* Number of distinct parameters */


/*************************************************************************/
  /* PHO data object: */

typedef struct  {
	char func_name[phoMAX_FUNC_NAME_LENGTH+1];
	double func_params[phoMAX_PARAM_PER_FUNC];
	char flag_set[phoMAX_PARAM_PER_FUNC];
	char flag_chg[phoMAX_PARAM_PER_FUNC];
	char flag_func_set[1];
	char flag_func_chg[1];
	char flag_mode[phoMax_MODE_NAME_LENGTH+1];
	}
PHO_STRUCT;


/*************************************************************************/
  /*
   * Second, the declarations of subroutine calls to the above-
   * listed supported photometric functions:
   */
void LAMBERT(double *x, double *a, double *res, 
             char phoMode[phoMax_MODE_NAME_LENGTH+1]);
void MINNAERT(double *x, double *a, double *res, 
             char phoMode[phoMax_MODE_NAME_LENGTH+1]);
void VEVERKA(double *x, double *a, double *res, 
             char phoMode[phoMax_MODE_NAME_LENGTH+1]);
void IRVINE(double *x, double *a, double *res, 
             char phoMode[phoMax_MODE_NAME_LENGTH+1]);
/*void LOM_SEEL();*/
void SQUE_VEV();
void BURATTI1();
void BURATTI2();
void BURATTI3();
void MOSHER();
void LUMME_BOWEL_HG1();
void HAPKE_81_LE2();
void HAPKE_81_COOK();
void HAPKE_86_HG1();
void HAPKE_86_HG2();
void HAPKE_86_LE2(double *x, double *a, double *res, 
             char phoMode[phoMax_MODE_NAME_LENGTH+1]);
void HAPKE_HG1_DOM();
void REGNER_HAPKE_HG1();

  /*
   * helping functions : 
   */
void pho_HAPKE86 (double *x, double *a, double *res, int flag);
double pho_hp_rough (double *cosi, double *cose, double cosg, double tbar);
double pho_dg_rough (double *ci, double *ce, double cg, double tbar, 
		     double *di, double *de, double *dg, double *da, int flag);
double pho_d_h_fct (double a, double sqw, double *da, double *dw);




 typedef void (*FTyp)();

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create pho_init.h
$ DECK/DOLLARS="$ VOKAGLEVE"
	/*
	 * PHO_INIT.H
	 *
	 * Photometric  Functions Include File
	 *
	 * Purpose:
	 *
	 * This is an include file that is used only by the routines
	 * which have a direct access to the pho_object.
	 * It includes the definitionof coodes needed to get a direct
	 * access to the pho_object.
	 * To add a new function see the description in "pho_private.h
	 * to.
	 * The following comments describe the changings in this file
	 * to add a new function :

/*************************************************************************/
  /*
   * HERE FOLLOW THREE LISTS OF VALID PHOTOMETRIC FUNCTIONS, WHICH
   * MUST BE MAINTAINED IN PARALLEL.  FURTHERMORE, THE DEFINITIONS
   * OF THE FUNCTION NAMES AS '#DEFINE'S ARE FOUND IN PHO.H AND MUST
   * ALSO BE MAINTAINED IN PARALLEL.
   *   
   * First, all the valid, currently supported photometric functions:
   */

char phoFUNC_NAMES[phoFUNCTION_COUNT][phoMAX_FUNC_NAME_LENGTH+1] = {
	phoLAMBERT,
	phoMINNAERT,
	phoIRVINE,
	phoSQUE_VEV,
	phoVEVERKA,
	phoBURATTI1,
	phoBURATTI2,
	phoBURATTI3,
	phoMOSHER,
	phoLUMME_BOWEL_HG1,
	phoHAPKE_81_LE2,
	phoHAPKE_81_COOK,
	phoHAPKE_86_HG1,
	phoHAPKE_86_HG2,
	phoHAPKE_86_LE2,
	phoHAPKE_HG1_DOM,
	phoREGNER_HAPKE_HG1,
	phoATMO_CORR_REGNER
};
/*************************************************************************/
  /*
   * Third, the names of subroutine calls to the above-listed 
   * supported photometric functions:
   */


FTyp	phoFUNC_CALL_NAME[phoFUNCTION_COUNT-1] = {
	LAMBERT,
	MINNAERT,
	IRVINE,
	SQUE_VEV,
	VEVERKA,
	BURATTI1,
	BURATTI2,
	BURATTI3,
	MOSHER,
	LUMME_BOWEL_HG1,
	HAPKE_81_LE2,
	HAPKE_81_COOK,
	HAPKE_86_HG1,
	HAPKE_86_HG2,
	HAPKE_86_LE2,
	HAPKE_HG1_DOM,
	REGNER_HAPKE_HG1
};


/*************************************************************************/
  /* Array specifying number of parameters for each function: */

int phoPARAM_COUNT[phoFUNCTION_COUNT] = { 1, 2, 3, 4, 4, 3, 4, 5, 6, 5, 4, 5, 5, 
					7, 6, 7, 8, 8 };


/*************************************************************************/
  /* 
   * Array of valid parameter codes for the functions:
   * in this array, each function has a separate line, in which the
   * entries refer to the phoPARAM_KEYWORDS array (below), e.g., a 
   * value of 2 denotes the 3rd entry in that array.
   */

int phoFUNC_PARAM_CODES[phoFUNCTION_COUNT][phoMAX_PARAM_PER_FUNC] = {
  0, 0, 0, 0, 0, 0, 0, 0,		/* func.0 = Lambert  		*/
  0, 1, 0, 0, 0, 0, 0, 0,		/* func.1 = Minnaert 		*/
  1,27,28, 0, 0, 0, 0, 0,		/* func.2 = Irvine		*/
  2, 3, 4, 5, 0, 0, 0, 0,		/* func.3 = Sque_Vev		*/ 
  2, 3, 4, 5, 0, 0, 0, 0,		/* func.4 = Veverka  		*/
  0, 3, 8, 0, 0, 0, 0, 0,		/* func.5 = Buratti1		*/
  0, 3, 4, 8, 0, 0, 0, 0,		/* func.6 = Buratti2		*/
  0, 3, 4, 5, 8, 0, 0, 0,		/* func.7 = Buratti3		*/
  2, 3, 4, 5, 6, 7, 0, 0,		/* func.8 = Mosher   		*/
  12,18,11,22,13,0, 0, 0,		/* func.9 = Lumme-Bowel-HG1 	*/
  12,18,16,17,0, 0, 0, 0,		/* func.10= Hapke-81-Le2 	*/
  12,18,16,17,23,0, 0, 0,		/* func.11= Hapke-81-Cook	*/
  12,18,19,22,13,0, 0, 0,		/* func.12= Hapke-86-HG1	*/
  12,18,19,22,13,14,15,0,		/* func.13= Hapke-86-HG2	*/
  12,18,19,22,16,17,0, 0,		/* func.14= Hapke-86-Le2	*/
  12,18,19,22,13,20,21,0,		/* func.15= Hapke-HG1-Dom	*/
  12,18,19,22,13,25,24,26,		/* func.16= Regner-Hapke-HG1	*/
  12,18,19,22,13,25,24,26             /* func.17= Atmospheric_Correct_Regner */
};


/*************************************************************************/
  /*
   * all the valid photometric parameter keywords:
   * For clarity, the sequence number is repeated in a comment on
   * each line, since this index is used by the phoFUNC_PARAM_CODES
   * array (see above)
   */

char phoPARAM_KEYWORDS[phoMAX_PARAM_COUNT][phoMAX_KEYWD_LENGTH+1] = {
	phoALBEDO,	/* keyword #0 */
	phoEXPONENT,	/* keyword #1 */
	phoA_VEVERKA,	/* keyword #2 */
	phoB_VEVERKA,	/* keyword #3 */
	phoC_VEVERKA,	/* keyword #4 */
	phoD_VEVERKA,	/* keyword #5 */
	phoMO_EXP1,	/* keyword #6 */
	phoMO_EXP2,	/* keyword #7 */
	phoE_BURATTI,	/* keyword #8 */
	phoMO_EXP1,	/* keyword #9 */
	phoMO_EXP2,	/* keyword #10 */
	phoDEN_SOIL,	/* keyword #11 */
	phoW_SOIL,	/* keyword #12 */
	phoHG1_SOIL,	/* keyword #13 */
	phoHG2_SOIL,	/* keyword #14 */
	phoHG_ASY_SOIL,	/* keyword #15 */
	phoLE1_SOIL,	/* keyword #16 */
	phoLE2_SOIL,	/* keyword #17 */
	phoH_SHOE,	/* keyword #18 */
	phoB_SHOE,	/* keyword #19 */
	phoH_CBOE,	/* keyword #20 */
	phoB_CBOE,	/* keyword #21 */
	phoTHETA,	/* keyword #22 */
	phoCOOK,	/* keyword #23 */
	phoTAU_ATM,	/* keyword #24 */
	phoW_ATM,	/* keyword #25 */
	phoHG1_ATM,	/* keyword #26 */
	phoIRV_EXP1,	/* keyword #27 */
	phoIRV_EXP2	/* keyword #28 */
};

$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create pho_routines.imake
#define SUBROUTINE  	pho_routines

#define MODULE_LIST  	pho_routines.c pho_bidirefs.c pho_routinesf.f

#define INCLUDE_LIST  	pho_private.h pho_init.h

#define FTNINC_LIST pho

#define USES_ANSI_C
#define USES_FORTRAN

#define FTN_STRING

#define P2_SUBLIB

/********************************************
DEBUGGER for development 

#define LOCAL_LIRARY
#define LIB_LOCAL
#define DEBUG
#define LIB_P2SUB_DEBUG

*******************************************/

#define LIB_P2SUB
#define LIB_RTL
#define LIB_TAE



$ Return
$!#############################################################################
$Other_File:
$ create pho_free.hlp
================================================================================
VICAR SUBROUTINE			phoFree
================================================================================

Purpose:				To release memory which was previously 
					allocated for a photometric function 
					data object.

================================================================================

Function:				Deallocates memory for the pointer to 
					the photometric structure using free().
					The structure was previously allocated 
					by phoInit.

================================================================================

Requirements and Dependencies:

Libraries and subroutines
required to run routine:		none

Necessary include files
for calling routine 
or program:				pho.h (for C routines )
					pho.fin (for FORTRAN routines )

Main programs from which 
subroutine will be called:		general application software

Other requirements: 			need to run routine phoInit first

================================================================================

Interfaces: Input / Output Parameters:

Calling Sequence:		

	calling from C			include "pho.h"
					PHO PHO_DATA;
					int status;
					...
					status = phoFree( PHO_DATA );

	calling from FORTRAN		INCLUDE 'pho'
					INTEGER PHO_DATA, STATUS
					...
					call pho_free( PHO_DATA, status ) 


Parameters:
	
	Name		Type		In/Out		Description
	
	PHO_DATA	PHO_STRUCT	Input		Address of photometric 
							function data object

	status	int			Output		Error status:
							phoSUCCESS - success

================================================================================

Other Information:

	Software Platforms:		VICAR (VMS/UNIX)

	Hardware Platforms:		No particular hardware required
					tested on 

	Programming Language:		ANSI C, 
					FORTRAN bridge

	Specification by:		Friedel Oschuetz, DLR

	Initial version:		Lucas Kamp, JPL

	Cognizant Programmer:		Friedel Oschuetz

	Date of Specification:		Jan. '94

	Time Estimated for 
	Routine Development:		1/4 day 

	Estimate of Delivery Date:

	History:			Original

================================================================================
$!-----------------------------------------------------------------------------
$ create pho_get_func.hlp
================================================================================
VICAR SUBROUTINE			phoGetFunc
================================================================================

Purpose:		This routine retrieves the photometric function 
			name from the PHO (photometric data object).

================================================================================

Function:		Checks whether the function has been set in the 
			photometric data object and whether this is a valid 
			function name. It retrieves the name of this
			function from the photometric data object.

================================================================================

Requirements and Dependencies:

Libraries and subroutines
required to run routine:	

Necessary include files
for calling routine 
or program:			pho.h (for C routines )
				pho.fin (for FORTRAN routines )

Other requirements: 		need to run routines phoInit, phoSetFunc 
				first

Main programs from which
subroutine will be called:	general application software and higher-level
				subroutines; 
				phoImgLabWrt, phoParFilWrt, phoFunc, 
				phoCorrect;
				hwphoco, hwphomas; 
				PHOTEST, PHOTFIT2; 
				HWGEOM*, HWORTH*, HWPHOCOE,....

================================================================================

Interfaces: Input / Output Parameters:

Calling Sequence:		

	calling from C:	include "pho.h"
				PHO PHO_DATA;
				char PHO_FUNC[phoMAX_FUNC_NAME_LENGTH+1];
				int status;
				...
				status = phoGetFunc( PHO_DATA, PHO_FUNC); 

	calling from FORTRAN:
				INCLUDE 'pho'
				CHARACTER*(PHO_MAX_FUNC_NAME_LENGTH) PHO_FUNC
				INTEGER PHO_DATA, status
				...
				call pho_get_func( PHO_DATA, PHO_FUNC, status )

Necessary include files
from calling routine 
or program:		pho.h (in C routines) 
			pho.fin ( in FORTRAN routines)

Parameters:
	
	Name		Type		In/Out	Description

	
	PHO_DATA	PHO_STRUCT	Input	Address of photometric 
						data object

	PHO_FUNC	char		Output	The current photometric 
		[phoMAX_FUNC_NAME_LENGTH+1]	function name

	status		int		Output	Error status:
							phoSUCCESS - success
							phoFUNC_NOT_SET 
							- failed: 
								photometric 
								function not 
								set in the 
								pho_object
							phoInvalid_Function 
							- failed :
								invalid 
								photometric 
								function name 
								has been 
								passed

================================================================================
Background and References:
================================================================================

Other Information:

Software Platforms:		VICAR (VMS/UNIX)

Software Platforms:		No particular hardware required;
				tested on ....

Programming Language:		ANSI-C, 
				ANSI-FORTRAN bridge

Specification by:		Friedel Oschuetz, DLR

Initial version:		Lucas Kamp, JPL

Cognizant Programmer:		Friedel Oschuetz

Date of Specification:		Jan. '94

Time Estimated for 
Routine Development:		1/2 day 

Estimate of Delivery Date:

History:			L. Kamp, Jan. '94 : Original

================================================================================

$!-----------------------------------------------------------------------------
$ create pho_get_keys.hlp
================================================================================
VICAR SUBROUTINE			phoGetKeys
================================================================================

Purpose:		Returns the number and a list of all parameter 
			keywords that are valid for the current 
			photometric function 

================================================================================

Function:		First, determines the current photometric 
			function from to photometric data object;  
			second, retrieves the number of parameters for
			this function;  
			third retrieves the list of parameter keyword names.
			It is the responsibility of the calling 
			program to allocate sufficient memory to hold 
			the array that will be returned.  
			Only in C: if the program is called with no 
			allocation for the keyword array, the routine 
			will return the number of keywords (num_kwd) only.  
			The user can then allocate memory and make a 
			second call to the routine. 
			Alternatively, the user can allocate the 
			arrays to their maximum length, which in C is 
			phoMAX_PARAM_PER_FUNC* (phoMAX_KEYWD_LENGTH+1) 
			(these constants are defined in pho.h and pho.fin).

================================================================================

Requirements and Dependencies:

Libraries and subroutines
required to run routine:	

Necessary include files
for calling routine 
or program:			pho.h (for C routines )
				pho.fin (for FORTRAN routines )

Other requirements: 		need to run routine phoInit, phoSetFunc first

Main programs from which
subroutine will be called:	general application software and 
				higher-level subroutines

================================================================================

Interfaces: Input / Output Parameters:

Calling Sequence:		

	calling from C:		include "pho.h"
				PHO PHO_DATA;
				char *keywords;
				int num_kwd, status;
				...
				status = phoGetKeys( PHO_DATA, 0,&num_kwd );
				keywords = (char *)malloc( num_kwd *
				( phoMAX_KEYWD_LENGTH+1 ) * sizeof(char));
				status = phoGetKeys( PHO_DATA, keywords,
				&num_kwd );

	calling from FORTRAN:	INCLUDE 'pho'
				CHARACTER*(PHO_MAX_KEYWD_LENGTH) 
			*	keywords(PHO_MAX_PARAM_PER_FUNC)
				INTEGER PHO_DATA, num_kwd, status
				...
				call pho_get_keys( PHO_DATA, keywords, 
			*	num_kwd, status )

Necessary include files
from calling routine 
or program:			pho.h (for C routines)
				pho.fin (for FORTRAN routines )

Parameters:
	
	Name		Type		In/Out	Description

	PHO_DATA	PHO_STRUCT	Input	Address of the photometric 
						data object

	keywords		char	Output	array of keywords
		[num_kwd]*[phoMAX_KEYWD_LENGTH+1]

	num_kwd	 	int	 	Input	number of keywords 
					/Output

	status		int		Output	Error status:
						phoSUCCESS - success : 
							list of keywords 
							returned
						phoFUNC_NOT_SET failed : 
							photometric function 
							not set in the 
							pho_object

================================================================================
Background and References:
================================================================================

Other Information:

	Software Platforms:		VICAR (VMS/UNIX)

	Hardware Platforms:		No particular hardware required;
					tested on ....

	Programming Language:		ANSI-C 
					ANSI-FORTRAN bridge

	Specification by:		Friedel Oschuetz, DLR

	Initial Version :		Lucas Kamp, JPL

	Cognizant Programmer:		Friedel Oschuetz

	Date of Specification:		Jan. '94

	Time Estimated for 
	Routine Development: 		1 day

	Estimate of Delivery Date:

	History:			Lucas Kamp, Jan. '94 : Original
					Friedel Oschuetz, March '94 : 
						possibility to return only the 
						number of keywords (without 
						responsibility of memory 
						allocation for the keyword list) 
================================================================================

$!-----------------------------------------------------------------------------
$ create pho_get_parms.hlp
================================================================================
VICAR SUBROUTINE		phoGetParms
================================================================================

Purpose:		This routine reads input parameters that pertain 
			to photometric functions from the PDF and set these 
			in the pho_object.

================================================================================

Function:		Searches for the PDF keyword PHO_FUNC; 
			checks if the function has been set in the PDF and 
			reads the keyword value; 
			sets this function name in the pho_object, calling 
			phoSetFunc.
			Next, retrieves a list of photometric parameter 
			keywords pertaining to this function, 
			calling phoGetKeys;  
			searches for and reads the parameter values associated 
			with these keywords from the PDF;
			checks if the values have been set in the PDF;
			sets the parameter values in the photometric data 
			object, calling phoSetVal.

================================================================================

Requirements and Dependencies:

Libraries and subroutines
required to run routine:		VICAR RTL, TAE RTL;
					phoSetFunc, phoGetKeys, phoSetVal

Necessary include files
for calling routine 
or program:				pho.h (for C routines )
					pho.fin (for FORTRAN routines )

Other requirements: 			need to run routine phoInit first.

Main programs from which
subroutine will be called:		general application software and 
					higher-level subroutines;

================================================================================

Interfaces: Input / Output Parameters:

Calling Sequence:		

	calling from C:		include "pho.h"
				PHO PHO_DATA;
				int status;
				...
				status = phoGetParms( PHO_DATA );
 
	calling from FORTRAN:	INCLUDE 'pho'
				INTEGER PHO_DATA, status
				...
				call PHO_GET_PARMS(PHO_DATA, status)

Necessary include files
from calling routine 
or program:			pho.h ( for C routines )
				pho.fin ( for FORTRAN routines )

Parameters:
	
Name		Type		In/Out	Description

PHO_DATAPHO_STRUCT		Input	Address of the photometric data object

status	int			Output	Error status:
					phoSUCCESS - success
						photometric function and 
						photometric parameters are set; 
						PHO was empty before
					phoFUNC_CHANGED - success: 
						photometric function set in the 
						PHO; function had been 
						set previously
					phoKEYWD_CHANGED - success :
						photometric parameter set in 
						the PHO; one of the parameters 
						had been set previously
					phoFUNC_NOT_SPECIFIED - failed :
						no photometric function 
							specified in the PDF
						phoINVALID_FUNCTION - failed :
							an invalid photometric 
							function has been passed
						phoKEYWD_NOT_SPECIFIED - failed:
							a required photometric 
							parameter has not 
							been set in the PDF

================================================================================
Background and References:
================================================================================

Other Information:

	Software Platforms:			VICAR (VMS/UNIX)
	
	oftware Platforms:			No particular hardware required;
						tested on ....

	rogramming Language:			ANSI-C, 
						ANSI-FORTRAN bridge

	Specification by:			Friedel Oschuetz, DLR

	Initial Version:			Friedel Oschuetz

	Cognizant Programmer:			Friedel Oschuetz

	Date of Specification:			Jan. '94

	Time Estimated for Development: 	2 days

	Estimate of Delivery Date:

	History:				F. Oschuetz, Jan. '94 : 
							Original
						F. Oschuetz, March '94 : 
							error actions when
							- function name has 
							  been changed,
							- a parameter value has 
							  been changed,
							- a parameter keyword 
							  has not been set in 
							  the PDF

================================================================================

$!-----------------------------------------------------------------------------
$ create pho_get_val.hlp
================================================================================
VICAR SUBROUTINE			phoGetVal
================================================================================

Purpose:		This routine retrieves a value from the PHO 
			(photometric data object)  for a given parameter 
			keyword.

================================================================================

Function:		determines the current photometric function;
			verifies that the parameter keyword is a valid one; 
			retrieves the value from the PHO

================================================================================

Requirements and Dependencies:

Libraries and subroutines
required to run routine:	

Necessary include files
for calling routine 
or program:			pho.h (for C routines )
				pho.fin (for FORTRAN routines )

Other requirements: 		need to run routines phoInit, 
				phoSetFunc, phoSetVal first

Main programs from which
subroutine will be called:	general application software and 
				higher-level subroutines; 
				phoParFilWrt, phoFunc, hwphoco; 
				PHOTEST, PHOTFIT2, 
				HWGEOM*, HWORTH*, HWPHOCOE, ....

================================================================================

Interfaces: Input / Output Parameters:

Calling Sequence:		

	calling from C:		include "pho.h"
				PHO PHO_DATA;
				char keyword[phoMAX_KEYWD_LENGTH+1];
				double value;
				int status;
				...
				status =phoGetVal( PHO_DATA, keyword, &value); 

	calling from FORTRAN:	INCLUDE 'pho'
				CHARACTER*(PHO_MAX_KEYWD_LENGTH) keyword
				INTEGER PHO_DATA, status
				DOUBLE PRECISION value
				...
				call pho_get_val( PHO_DATA, keyword, 
			*	value, status)

Necessary include files
from calling routine 
or program:			pho.h ( for C routines)
				pho.fin ( for FORTRAN routines )




Parameters:
	
	Name		Type		In/Out	Description

	
	PHO_DATA	PHO_STRUCT	Input	Address of photometric data 
						data object

	keyword		char		Output	the photometric parameter 
		[phoMAX_KEYWD_LENGTH+1]		keyword

	value		double	 	Output	value for the keyword

	status	int			Output	Error status:
						phoSUCCESS - success
							phoFUNC_NOT_SET 
							   - failed :
								photometric 
								function has 
								not been set in 
								the pho_object
							phoINVALID_KEYWD 
							   - failed :
								invalid 
								parameter 
								keyword has 
								been passed
							phoKEYWD_NOT_SET 
							   - failed :
								the parameter 
								value has not 
								been set in 
								the pho_object

================================================================================
Background and References:
================================================================================

Other Information:

	Software Platforms:		VICAR (VMS/UNIX)

	Software Platforms:		No particular hardware required;
					tested on ....

	rogramming Language:		ANSI-C, 
					ANSI-FORTRAN bridge

	Specification by:		Friedel Oschuetz, DLR

	Initial Version :		Lucas Kamp, JPL

	Cognizant Programmer:		Friedel Oschuetz

	Date of Specification:		Jan. '94

	Time Estimated for 
	Routine Development: 		1/2 day

	Estimate of Delivery Date:

	History:			L. Kamp, Jan. '94 : Original

================================================================================

$!-----------------------------------------------------------------------------
$ create pho_init.hlp
================================================================================
VICAR SUBROUTINE			phoInit
================================================================================

Purpose:		initialises the PHO (photometric data object)

================================================================================

Function:		Allocates memory for the structure of the 
			photometric data object using malloc().
			Initialises the flags of the data object to zero and 
			returns a pointer to that structure. 

================================================================================

Requirements and Dependencies:

	Libraries and subroutines
	required to run routine:	none

	ecessary include files
	for calling routine
	or program:			pho.h (for C routines)
					pho.fin ( for FORTRAN routines)

	Main programs from which 
	subroutine will be called:	general application software; 

	Other requirements: 		need to run routine phoFree to free 
					the memory

================================================================================

Interfaces:  Input / Output Parameters:

Calling Sequence:

	calling from C:		include "pho.h"
				PHO PHO_DATA;
				int status;
				...
				status = phoInit ( &PHO_DATA );

	calling from FORTRAN:	INCLUDE 'pho'
				INTEGER PHO_DATA, STATUS
				...
				CALL PHO_INIT( PHO_DATA, STATUS)


Parameters:

	Name		Type		In/Out	Description

	PHO_DATA	PHO_STRUCT 	Output	Pointer to a photometric
						data object

	status		int		Output	Error status:
						phoSUCCESS - success
						phoFAILURE - memory  
							allocation failed

================================================================================
Background and References:
================================================================================

Other Information:

	Software Platform:		VICAR (VMS/UNIX)

	Hardware Platforms:		No particular hardware required;
					tested on ....

	Programming Language:		ANSI-C
					ANSI-FORTRAN bridge

	Specification by:		Friedel Oschuetz, DLR

	Initial version:		Lucas Kamp, JPL

	Cognizant Programmer:		Friedel Oschuetz

	Date of Specification:		Jan. '94

	Time Estimated for 
	Routine Development: 		1/4 day

	Estimate of Delivery Date:

	History:			L. Kamp, Jan. '94 :
						Original
					F. Oschuetz, March '94: 
						initialisation for error 
						actions for changing of 
						function name and function 
						parameters

================================================================================

$!-----------------------------------------------------------------------------
$ create pho_set_func.hlp
================================================================================
VICAR SUBROUTINE			phoSetFunc
================================================================================

Purpose:		This routine sets the photometric function name 
			in the PHO (photometric data object).

================================================================================

Function:		It checks whether the function name is valid. 
			It sets the name in the photometric data 
			object and sets the "function set" and "function 
			changed" flags. 

================================================================================

Requirements and Dependencies:

	Libraries and subroutines
	required to run routine:	

	Other requirements: 		need to run routines phoInit first

	Necessary include files
	for calling routine 
	or program:			pho.h (for C routines )
					pho.fin (for FORTRAN routines )

	Main programs from which
	subroutine will be called:	general application software and 
					higher-level subroutines; 
					phoGetParms, phoParFilRead;
					hwphomas; 
					PHOTEST, PHOTFIT2; 
					HWGEOM*, HWORTH*, HWPHOCOE,....

	Other requirements: 		need to run routine phoInit first

================================================================================

Interfaces: Input / Output Parameters:

Calling Sequence:		

	calling from C:	include "pho.h"
				PHO PHO_DATA;
				char PHO_FUNC[phoMAX_FUNC_NAME_LENGTH+1];
				int status;
				...
				status = phoSetFunc( PHO_DATA, PHO_FUNC); 

	calling from FORTRAN:
	for scalar values:	INCLUDE 'pho'
				CHARACTER*(PHO_MAX_FUNC_NAME_LENGTH) PHO_FUNC
				INTEGER PHO_DATA, status
				...
				call pho_set_func( PHO_DATA, PHO_FUNC, status )

Necessary include files
from calling routine 
or program:			pho.h (in C routines) 
				pho.fin ( in FORTRAN routines)

Parameters:
	
	Name		Type		In/Out	Description

	
	PHO_DATA	PHO_STRUCT	Input	Address of photometric data 
						object

	PHO_FUNC	char		Input	the photometric function name
			[phoMAX_FUNC_NAME_LENGTH+1]

	status	int			Input	Error status:
					/Output	phoSUCCESS - success: 
							photometric function 
							set in the pho_object 
							for the first time
						phoFUNC_CHANGED - success: 
							photometric function 
							has been set in the 
							pho_object, 
							but it was already set 
							before
						phoINVALID_FUNCTION - 
							invalid photometric 
							function has been passed

================================================================================
Background and References:
================================================================================

Other Information:

	Software Platforms:		VICAR (VMS/UNIX)

	Software Platforms:		No particular hardware required;
					tested on ....

	Programming Language:		ANSI-C, 
					ANSI-FORTRAN-bridge

	Specification by:		Friedel Oschuetz, DLR

	Initial version:		Lucas Kamp, JPL

	Cognizant Programmer:		Friedel Oschuetz

	ate of Specification:		Jan. '94

	Time Estimated for 
	Routine Development:		1/2 day 

	Estimate of Delivery Date:

	History:			L. Kamp, Jan. '94: 
						Original
					F. Oschuetz, March '94: 
						error action in case that
						the function name has been 
						changed

================================================================================



$!-----------------------------------------------------------------------------
$ create pho_set_val.hlp
================================================================================
VICAR SUBROUTINE			phoSetVal
================================================================================

Purpose:		This routine places a photometric function parameter 
			value in the PHO (photometric data object) for a given 
			parameter keyword.

================================================================================

Function:		Verifies that the function name has been set;
			checks that the parameter keyword is a valid one;
			places the value in the PHO;
			checks if the value has been set previously;
			sets the "value set" and the "value changed" flags. 

================================================================================

Requirements and Dependencies:

	Libraries and subroutines
	required to run routine:	

	Necessary include files
	or calling routine 
	or program:			pho.h (for C routines )
					pho.fin (for FORTRAN routines )

	Other requirements: 		need to run routines phoInit and 
					phoSetFunc first

	Main programs from which
	subroutine will be called:	general application software and 
					higher-level subroutines;
					phoGetParms, phoParFilGet; 
					hwdiran, hwphomas; 
					PHOTEST, PHOTFIT2, 
					HWGEOM*, HWORTH*, HWPHOCOE, ...

================================================================================

Interfaces: Input / Output Parameters:

Calling Sequence:		

	calling from C:		include "pho.h"
				PHO PHO_DATA;
				char keyword[phoMAX_KEYWD_LENGTH+1];
				double value;
				int status;
				...
				status = phoSetVal( PHO_DATA, keyword, value); 

	calling from FORTRAN:	INCLUDE 'pho'
				CHARACTER*(PHO_MAX_KEYWD_LENGTH) keyword
				INTEGER PHO_DATA, status
				DOUBLE PRECISION value
				...
				call pho_set_val( PHO_DATA, keyword, 
			*	value, status) 

Necessary include files
from calling routine 
or program:			pho.h ( for C routines )
				pho.fin ( for FORTRAN routines )

Parameters:
	
	Name		Type		In/Out	Description
	
	PHO_DATA	PHO_STRUCT	Input/	Address of photometric data 
						Output	object

	keyword		char		Input	the keyword of the parameter
			[phoMAXKEYWD_LENGTH+1]

	value		double	 	Input	value for the keyword 

	status		int		Output	Error status:
						phoSUCCESS - success :
							the value has been set 
							for the first time
						phoKEYWD_CHANGED - success :
							but the value for the 
							keyword had been set
							previously and now has
							been changed
						phoFUNC_NOT_SET - failed :
							the function has not 
							been set in the 
							pho_object
						phoINVALID_KEYWD - failed :
							invalid keyword has 
							been passed

================================================================================
Background and References:
================================================================================

Other Information:

	Software Platforms:		VICAR (VMS/UNIX)

	Hardware Platforms:		No particular hardware required;
					tested on ....

	rogramming Language:		ANSI-C, 
					ANSI-FORTRAN bridge

	Specification by:		Friedel Oschuetz, DLR

	Initial version:		Lucas Kamp, JPL

	Cognizant Programmer:		Friedel Oschuetz

	Date of Specification:		Jan. '94

	Time Estimated for 
	Routine Development: 		1/2 day

	Estimate of Delivery Date:	

	History:			L. Kamp, Jan. '94 :
						Original
					F. Oschuetz, March '94 : 
						error action when parameter 
						values have been modified 

================================================================================



$!-----------------------------------------------------------------------------
$ create pho_correct.hlp
===========================================================================

VICAR SUBROUTINE		phoCorrect

===========================================================================

Purpose:		Returns the photometric correction factor of 
			a given photometric data object (containing a
			photometric function) to change radiance 
			values from measured viewing and illumination 
			conditions to new artificial target viewing 
			and illumination conditions.

===========================================================================

Function:		The routine calls twice the routine phoBidiRef 
			for the measured and the target illumination 
			condition and divides them.  In the case that a 
			failure occurs or the target point is not 
			illuminated or is not visible for the measured 
			or the target illumination conditions 
			the function returns the value 1.0 .

			The filled photometric data object is passed to this
			routine.  The input of the illumination conditions 
			is different for the Fortran- and the C-routine:
			FORTRAN :
				Two arrays and some "switch"-parameters are 
				passed to the routine.
			C :
				Two PHO_ILLUM unions (containing all 
				illumination conditions )will be filled before 
				directly.The union is defined in pho.h .
 
			phoCorrect accepts four different representations 
			of the measured illumination conditions and target
			ones independently : 
				a) cosine of the angles of incidence, 
				   emission, and phase;
				b) cosine of the angles of incidence, 
				   and emission in relation to the local 
				   ellipsoid normal and to the local 
				   surface narmal, and cosine of phase;
				c) directions of incidence(directed to 
				   the planet), emission(directed away 
				   from the planet) and of ellipsoid 
				   normal(directed away from the 
				   planet);
				d) directions of incidence(directed to 
				   the planet), emission(directed away 
				   from the planet) and of ellipsoid 
				   normal(directed away from the planet) 
				   and of surface normal (directed back 
				   from the planet) 

			To do a kind of atmospheric correction together 
			with the photometric correction :
			You have to select for the Parameter 
			PHO_FUNC the keyword ATMO_CORR_REGNER. 
			Then the routine computes the bidirectional 
			reflectance of the Regner-1990 approximation for the 
			measured illumination condition and the 
			bidirectional reflectance of the 
			Hapke-1968-1Therm-Henyey-Greenstein 
			approximation for the target illumination 
			condition and divide these.

			For the supported functions of the  
			bidirectional reflectance (additional to the 
			ATMO_CORR_REGNER) see the help of the
			pho-routine phoBidiRef.

===========================================================================

Requirements and Dependencies:

Libraries and subroutines
required to run routine:	VICAR RTL, phoInit, phoSetFunc, 
				phoGetFunc, phoGetKeys, phoSetVal, 
				phoGetVal, phoBidiRef 

Necessary include files
for calling routine 
or program:			pho.h (for C routines )
				PHO.FIN (for FORTRAN routines )

Other requirements: 		need to run routine phoInit, phoSetFunc, 
				phoSetVal (or other higher-level 
				subroutine calling phoSetFunc and 
				phoSetVal) first and to fill both PHO_ILLUM 
				unions for (in C-programs) or both illuArrays 
				(in FORTRAN programs) the first.

Main programs from which
subroutine will be called:	general application software and 
				higher-level subroutines;
				hwphoco.

===========================================================================

Interfaces: Input / Output Parameters:

Calling Sequence:		

calling from C :	include "pho.h"
			PHO PHO_DATA;
			PHO_ILLUM Millum, Tillum;
			int status;
			double phoCoVal;
			...
			status = phoCorrect( PHO_DATA, &Millum, &Tillum,
			&phoCorVal );

calling from FORTRAN :	INCLUDE 'pho'
			INTEGER MillMode, MSunShadow, MViewShadow, 
			DOUBLE PRECISION MilluArray(*) 
			INTEGER TillMode, TSunShadow, TViewShadow,
			DOUBLE PRECISION TilluArray(*) 
			DOUBLE PRECISION phoCorVal
			INTEGER PHO_DATA status
				...
			call pho_correct( PHO_DATA, 
		+	MillMode, MSunShadow, MViewShadow,MilluArray, 
		+	TillMode, TSunShadow, TViewShadow,TilluArray, 
		+	phoCorVal, status )

Necessary include files
from calling routine 
or program:		pho.h (for C routines )
			PHO.FIN ( for FORTRAN routines )


Parameters:
	
Name		Type	In/Out	Description
	

PHO_DATA	PHO	Input	Address of the photometric data object

MillMode	 int	Input	The illMode parameter indicates how 
TillMode	 int	Input	illumination is entered in the routine for the 
				measured(MillMode) and for the target(TillMode).
				phoCorrect accepts four different 
				representations of the measured and 
				independently target illumination conditions : 
					illEllCos --> case a);
					illDTMCos --> case b);
					illEllDir --> case c);
					illDTMDir --> case d)


MSunShadow	 int	Input	The parameter indicates if the point is in the 
				"Sun Shadow"(for measured illumination )

TSunShadow	 int	Input	The parameter indicates if the point is 
				in the "Sun Shadow"(for target illumination )

MViewShadow  int	Input	The parameter indicates if the point is in the 
				"View Shadow"(for measured emission direction )

TViewShadow  int	Input	The parameter indicates if the point is in the 
				"View Shadow"(for target emission direction )

MilluArray   double	Input	These arrays contain the measured and 
TilluArray   double	Input	target illumination and viewing geometry
				respectively.  The contence belongs to 
				the *illMode :

				illEllCos --> case a :

				DOUBLE PRECISION MilluArray(3) 
				DOUBLE PRECISION TilluArray(3)
				MilluArray(1)=cos(incidenceAngle) 
				MilluArray(2)=cos(emissionAngle) 
				MilluArray(3)=cos(phaseAngle) 
				analogous for TilluArray

				illDTMCos --> case b :

				DOUBLE PRECISION MilluArray(5) 
				DOUBLE PRECISION MilluArray(5)
				MilluArray(1)=cos(incidenceAngle2Ellips) 
				MilluArray(2)=cos(emissionAngle2Ellips) 
				MilluArray(3)=cos(phaseAngle) 
				MilluArray(4)=cos(incidenceAngle2Surf) 
				MilluArray(5)=cos(emissionAngle2Surface)
				analogous for TilluArray

				illEllDir --> case c :

				DOUBLE PRECISION MilluArray(9) 
				DOUBLE PRECISION MilluArray(9)
				MilluArray(1)=incidenceDirection(1) 
				MilluArray(2)=incidenceDirection(2) 
				MilluArray(3)=incidenceDirection(3) 
				MilluArray(4)=emissionDirection(1) 
				MilluArray(5)=emissionDirection(2) 
				MilluArray(6)=emissionDirection(2) 
				MilluArray(7)=EllipsNormalDirection(1) 
				MilluArray(8)=EllipsNormalDirection(2) 
				MilluArray(9)=EllipsNormalDirection(3) 
				analogous for TilluArray

				illDTMDir --> case d :

				DOUBLE PRECISION MilluArray(12) 
				DOUBLE PRECISION MilluArray(12)
				MilluArray(1)=incidenceDirection(1) 
				MilluArray(2)=incidenceDirection(2) 
				MilluArray(3)=incidenceDirection(3) 
				MilluArray(4)=emissionDirection(1) 
				MilluArray(5)=emissionDirection(2) 
				MilluArray(6)=emissionDirection(2) 
				MilluArray(7)=EllipsNormalDirection(1) 
				MilluArray(8)=EllipsNormalDirection(2) 
				MilluArray(9)=EllipsNormalDirection(3) 
				MilluArray(10)=SurfNormalDirection(1) 
				MilluArray(11)=SurfNormalDirection(2) 
				MilluArray(12)=SurfNormalDirection(3) 
				analogous for TilluArray


Millum	 PHO_ILLUM	Input	These unions contain the measured and 
Tillum	 PHO_ILLUM	Input	target illumination and viewing geometry
				respectively :
						
				For all representations of the illumination 
				geometry one of the makro illShadow or 
				illNoShadow is to be filled in the union:

				Millum.type.sunshadow=...
				Millum.type.viewshadow=...
				analogous for Tillum

				In relation to the case of the 
				representation of the illumination geometry :

				illEllCos --> case a) .
				...
				double incidAngle2Ellips;
				double emAngle2Ellips;
				double phaseAngle;,
				...
				Millum.mode=illEllCos;
				Millum.cos.inc=cos(incidAngle2Ellips);
				Millum.cos.em=cos(emAngle2Ellips);
				Millum.cos.phas=cos(phaseAngle);
				analogous for Tillum

				illDTMCos --> case b) :
				...
				double inciAngle2Ellips;
				double emAngle2Ellips;
				double inciAngle2Surf;
				double emAngle2Surf;
				double phaseAngle;
				...
				Millum.mode=illDTMCos;
				Millum.cos.inc=cos(inciAngle2Ellips);
				Millum.cos.em=cos(emAngle2Ellips);
				Millum.cos.phas=cos(phaseAngle);
				Millum.cos.inc_surf=cos(inciAngle2Surf);
				Millum.cos.em_surf=cos(emAngle2Surf);
				analogous for Tillum

				illEllDir --> case c) .
				....
				double incidDir(3);
				double emDir(3);
				double ellDir(3);
				....
				Millum.mode=illEllDir;
				memcpy(&(Millum.direction.inc), 
				incidDir, 3*sizeof(double));
				memcpy(&(Millum.direction.em), 
				emDir, 3*sizeof(double));
				memcpy(&(Millum.direction.ellips), 
				ellDir, 3*sizeof(double));
				analogous for Tillum

				illDTMDir --> case d) .
				...
				double incidDir(3);
				double emDir(3);
				double ellDir(3);
				double surfDir(3);
				...
				Millum.mode=illDTMDir;
				memcpy(&(Millum.direction.inc), 
				incidDir, 3*sizeof(double));
				memcpy(&(Millum.direction.em), 
				emDir, 3*sizeof(double));
				memcpy(&(Millum.direction.ellips), 
				ellDir, 3*sizeof(double));
				memcpy(&(Millum.direction.surf), 
				surfDir, 3*sizeof(double));
				analogous for Tillum

phoCorVal	double	Output	Photometric correction value 

status		int	Output	Error status:
				phoSUCCESS - success
				phoFAILURE - failed
				phoFUNC_NOT_SET - failed : 
					photometric function not set in the PHO
				phoKEYWD_NOT_SET - failed :
					one of the required parameter values has
					not been set in the PHO
				phoINVALID_KEYWD - failed :
					one of the parameter keywords is invalid
				phoINVALID_ILL_MODE - failed :
					invalid illMode
				phoARGUMENT_OUT_OF_RANGE-failed:
					one of the arguments is out of the valid
					range

===========================================================================

Background and References:

===========================================================================

Other Information:

Software Platforms:		VICAR 13 / TAE 5.2

Hardware Platforms:		No particular hardware required;
				tested on (SUN_OS,SUN_SOLARIS,VAX,AXP)

Programming Language:		ANSI-C, 
				ANSI-FORTRAN bridge

Specification by:		Friedel Oschuetz, DLR

Date of specification:		Jul '94

Cognizant Programmer:		Friedel Oschuetz
				Institute of Planetary Exploration
				DLR
				12484 Berlin (FRG)

History:			Jan '94, L. Kamp, JPL, initial version
				Jul '94, F. Oschuetz, preliminary version

===========================================================================
$!-----------------------------------------------------------------------------
$ create pho_func.hlp
===========================================================================

VICAR SUBROUTINE		phoFunc

===========================================================================

Purpose:		Returns the value of a photometric function 
			for a given photometric data object 
			(containing a photometric function) and given 
			illumination conditions.

===========================================================================

Function:		The routine calls twice the routine 
			phoBidiRef to compute the ratio of the 
			bidirectional reflectance for a fixed emission 
			angle, but varying incidence and phase angles, 
			to its value at phase-angle=0 as defined by Hapke(1993).

			The filled photometric data object is passed to this
			routine.  The input of the illumination conditions 
			is different for the Fortran- and the C-routine:
			FORTRAN :
				Two arrays and some "switch"-parameters are 
				passed to the routine.
			C :
				Two PHO_ILLUM unions (containing all 
				illumination conditions )will be filled before 
				directly.The union is defined in pho.h .
 
			phoCorrect accepts four different representations 
			of the measured illumination conditions and target
			ones independently : 
				a) cosine of the angles of incidence, 
				   emission, and phase;
				b) cosine of the angles of incidence, 
				   and emission in relation to the local 
				   ellipsoid normal and to the local 
				   surface narmal, and cosine of phase;
				c) directions of incidence(directed to 
				   the planet), emission(directed away 
				   from the planet) and of ellipsoid 
				   normal(directed away from the planet);
				d) directions of incidence(directed to 
				   the planet), emission(directed away 
				   from the planet) and of ellipsoid 
				   normal(directed away from the planet) 
				   and of surface normal (directed back 
				   from the planet) 

			For the supported functions of the  
			bidirectional reflectance see the help of the
			pho-routine phoBidiRef.

===========================================================================

Requirements and Dependencies:

Libraries and subroutines
required to run routine:	VICAR RTL, phoInit, phoSetFunc, 
				phoGetFunc, phoGetKeys, phoSetVal, 
				phoGetVal, phoBidiRef 

Necessary include files
for calling routine 
or program:			pho.h (for C routines )
				PHO.FIN (for FORTRAN routines )

Other requirements: 		need to run routine phoInit, phoSetFunc, 
				phoSetVal (or other higher-level 
				subroutine calling phoSetFunc and 
				phoSetVal) first

Main programs from which
subroutine will be called:	general application software and 
				higher-level subroutines.

===========================================================================

Interfaces: Input / Output Parameters:

Calling Sequence:		

calling from C :		calling from C :		iinclude "pho.h"
				PHO PHO_DATA;
				PHO_ILLUM illum;
				int status;
				double phoFuncVal;
				...
				status = phoBidiRef( PHO_DATA, &illum,
				&phoFuncVal );


calling from FORTRAN :	INCLUDE 'pho'
			INTEGER illMode, SunShadow, ViewShadow, 	
			DOUBLE PRECISION illuArray(*) 
			DOUBLE PRECISION phoFuncVal
			INTEGER PHO_DATA status
			...
			call pho_bidi_ref( PHO_DATA, 
		+	illMode, SunShadow, ViewShadow,illuArray, 
		+	phoFuncVal, status )

Necessary include files
from calling routine 
or program:		pho.h (for C routines )
			PHO.FIN ( for FORTRAN routines )


Parameters:
	
Name		Type	In/Out	Description
	

PHO_DATA	PHO	Input	Address of the photometric data object

illMode	 	int	Input	The illMode parameter indicates how 
				illumination is entered in the routine. 
				phoCorrect accepts four different 
				representations of the illumination conditions : 
					illEllCos --> case a);
					illDTMCos --> case b);
					illEllDir --> case c);
					illDTMDir --> case d)


SunShadow	 int	Input	The parameter indicates if the point is 
				in the "Sun Shadow"

ViewShadow  	int	Input	The parameter indicates if the point is 
				in the "View Shadow"


illuArray   double	Input	These arrays contain the illumination 
				geometry respectively.  The contence 
				belongs to the *illMode :

				illEllCos --> case a :
				...
				DOUBLE PRECISION illuArray(3) 
				DOUBLE PRECISION illuArray(3)
				...
				illuArray(1)=cos(incidenceAngle) 
				illuArray(2)=cos(emissionAngle) 
				illuArray(3)=cos(phaseAngle) 

				illDTMCos --> case b :
				...
				DOUBLE PRECISION illuArray(5) 
				DOUBLE PRECISION illuArray(5)
				...
				illuArray(1)=cos(incidenceAngle2Ellips) 
				illuArray(2)=cos(emissionAngle2Ellips) 
				illuArray(3)=cos(phaseAngle) 
				illuArray(4)=cos(incidenceAngle2Surf) 
				illuArray(5)=cos(emissionAngle2Surface) 

				illEllDir --> case c :
				...
				DOUBLE PRECISION illuArray(9) 
				DOUBLE PRECISION illuArray(9)
				...
				illuArray(1)=incidenceDirection(1) 
				illuArray(2)=incidenceDirection(2) 
				illuArray(3)=incidenceDirection(3) 
				illuArray(4)=emissionDirection(1) 
				illuArray(5)=emissionDirection(2) 
				illuArray(6)=emissionDirection(2) 
				illuArray(7)=EllipsNormalDirection(1) 
				illuArray(8)=EllipsNormalDirection(2) 
				illuArray(9)=EllipsNormalDirection(3) 

				illDTMDir --> case d :
				...
				DOUBLE PRECISION illuArray(12) 
				DOUBLE PRECISION illuArray(12)
				...
				illuArray(1)=incidenceDirection(1) 
				illuArray(2)=incidenceDirection(2) 
				illuArray(3)=incidenceDirection(3) 
				illuArray(4)=emissionDirection(1) 
				illuArray(5)=emissionDirection(2) 
				illuArray(6)=emissionDirection(2) 
				illuArray(7)=EllipsNormalDirection(1) 
				illuArray(8)=EllipsNormalDirection(2) 
				illuArray(9)=EllipsNormalDirection(3) 
				illuArray(10)=SurfNormalDirection(1)
				illuArray(11)=SurfNormalDirection(2) 
				illuArray(12)=SurfNormalDirection(3) 


illum	 PHO_ILLUM	Input	These unions contain the illumination geometry :
						
				For all representations of the illumination 
				geometry one of the makro illShadow or 
				illNoShadow is to be filled in the union:

				illum.type.sunshadow=...
				illum.type.viewshadow=...

				In relation to the case of the representation of
				the illumination geometry :

				illEllCos --> case a) .
				...
				double incidAngle2Ellips;
				double emAngle2Ellips;
				double phaseAngle;,
				...
				illum.mode=illEllCos;
				illum.cos.inc=cos(incidAngle2Ellips);
				illum.cos.em=cos(emAngle2Ellips);
				illum.cos.phas=cos(phaseAngle);

				illDTMCos --> case b) :
				...
				double inciAngle2Ellips;
				double emAngle2Ellips;
				double inciAngle2Surf;
				double emAngle2Surf;
				double phaseAngle;
				...
				illum.mode=illDTMCos;
				illum.cos.inc=cos(inciAngle2Ellips);
				illum.cos.em=cos(emAngle2Ellips);
				illum.cos.phas=cos(phaseAngle);
				illum.cos.inc_surf=cos(inciAngle2Surf);
				illum.cos.em_surf=cos(emAngle2Surf);

				illEllDir --> case c) .
				....
				double incidDir(3);
				double emDir(3);
				double ellDir(3);
				....
				illum.mode=illEllDir;
				memcpy(&(illum.direction.inc), incidDir, 
				  3*sizeof(double));
				memcpy(&(illum.direction.em), emDir, 
				  3*sizeof(double));
				memcpy(&(illum.direction.ellips), ellDir, 
				  3*sizeof(double));

				illDTMDir --> case d) .
				...
				double incidDir(3);
				double emDir(3);
				double ellDir(3);
				double surfDir(3);
				...
				illum.mode=illDTMDir;
				memcpy(&(illum.direction.inc), incidDir, 
				  3*sizeof(double));
				memcpy(&(illum.direction.em), emDir, 
				  3*sizeof(double));
				memcpy(&(illum.direction.ellips), ellDir, 
				  3*sizeof(double));
				memcpy(&(illum.direction.surf), surfDir, 
				  3*sizeof(double));

phoFuncVal	double	Output	Value of the photometric function

status	int	Output		Error status:
				phoSUCCESS - success
				phoFAILURE - failed
				phoFUNC_NOT_SET - failed : 
					photometric function not set in the PHO
				phoKEYWD_NOT_SET - failed :
					one of the required parameter values
					has not been set in the PHO
				phoINVALID_KEYWD - failed :
					one of the parameterkeywords is invalid
				phoINVALID_ILL_MODE - failed :
					invalid illMode
				phoARGUMENT_OUT_OF_RANGE-failed:
					one of the arguments is out of the valid
					range

===========================================================================

Background and References:
			Bruce Hapke, Theory of Reflectance Spectroscopy, 
			(Topics in Remote Sensing;3), 
			Cambridge University Press 1993, p. 272

===========================================================================

Other Information:

Software Platforms:		VICAR 13 / TAE 5.2 
				(SUN_OS,SUN_SOLARIS,VAX,AXP)

Hardware Platforms:		No particular hardware required;
				tested on SUN_OS,SUN_SOLARIS,VAX,AXP

Programming Language:		ANSI-C, 
				ANSI-FORTRAN bridge

Specification by:		Friedel Oschuetz, DLR

Date of specification:		July '94

Cognizant programmer:		Friedel Oschuetz, DLR-NE-PE
				Institute of Planetary Exploration
				DLR
				12484 Berlin (FRG)



History:			Jan '94, L. Kamp, initial version
				Jul '94, F. Oschuetz, preliminary version


===========================================================================
$!-----------------------------------------------------------------------------
$ create pho_bidi_ref.hlp
===========================================================================

VICAR SUBROUTINE		phoBidiRef

===========================================================================

Purpose:			Returns the value of a bidirectional 
				reflectance for a given photometric data object 
				(containing a photometric function) and 
				illumination geometry.

===========================================================================

Function:			First, the routine calls phoGetFunc, 
				phoGetKeys and phoGetVal to retrieve the 
				current bidirectional reflectance and their  
				associated parameter values.  
				Next, the routine computes the value of this 
				bidirectional reflectance for given illumination
				conditions.  
				The respective functions are stored as internal 
				subroutines within phoFunc.

				The filled photometric data object is passed to
				this routine.  The input of the illumination 
				conditions is different for the Fortran- and the
				C-routine:
				FORTRAN :
					Two arrays and some "switch"-parameters 
					are passed to the routine.
				C :
					Two PHO_ILLUM unions (containing all 
					illumination conditions )will be filled 
					before directly.The union is defined in 
					pho.h .
 
				phoCorrect accepts four different 
				representations of the measured illumination 
				conditions and target ones independently : 
					a) cosine of the angles of incidence, 
					   emission, and phase;
					b) cosine of the angles of incidence, 
					   and emission in relation to the local 
					   ellipsoid normal and to the local 
					   surface narmal, and cosine of phase;
					c) directions of incidence(directed to 
					   the planet), emission(directed away 
					   from the planet) and of ellipsoid 
					   normal(directed away from the 
					   planet);
					d) directions of incidence(directed to 
					   the planet), emission(directed away 
					   from the planet) and of ellipsoid 
					   normal(directed away from the planet) 
					   and of surface normal (directed back 
					   from the planet) 

				Supported functions of the bidirectional 
				reflectance are :
				1.	Lambert
				2.	Minnaert
				3.	Veverka
				4.	Buratti1
				5.	Buratti2
				6.	Buratti3
				7.	Mosher
				8.	Irvine
				9.	Lommel-Seeliger
				10.	Lumme-Bowell-1981
				11.	Hapke-1981
				12.	Hapke-Cook-1981
				13.	Hapke-1Therm-Henyey-Greenstein-1986
				14.	Hapke-2Therm-Henyey-Greenstein-1986
				15.	Hapke-Legendre-Polynom-1986
				16.   Hapke-Dominique-Coherent-Backscatter-1992
				17.	Regner-1990

===========================================================================


Requirements and Dependencies:

Libraries and subroutines
required to run routine:	VICAR RTL, phoInit, phoSetFunc, 
				phoGetFunc, phoGetKeys, phoSetVal, 
				phoGetVal 

Necessary include files
for calling routine 
or program:			pho.h (for C routines )
				PHO.FIN (for FORTRAN routines )

Other requirements: 		need to run routine phoInit, phoSetFunc, 
				phoSetVal (or other higher-level 
				subroutine calling phoSetFunc and 
				phoSetVal) first

Main programs from which
subroutine will be called:	general application software and 
				higher-level subroutines;
				phoCorrect, phoFunc; 
				hwphoco;
				PHOTFIT, PHOTTEST;
				HWGEOM*, HWORTH*, HWPHOCOE.

===========================================================================

Interfaces: Input / Output Parameters:

Calling Sequence:		

calling from C :	include "pho.h"
			PHO PHO_DATA;
			PHO_ILLUM illum;
			int status;
			double phoRefVal;
			...
			status = phoBidiRef( PHO_DATA, &illum, &phoRefVal );


calling from FORTRAN :	INCLUDE 'pho'
			INTEGER illMode, SunShadow, ViewShadow, 	
			DOUBLE PRECISION illuArray(*) 
			DOUBLE PRECISION phoRefVal
			INTEGER PHO_DATA status
			...
			call pho_bidi_ref( PHO_DATA, 
		+	illMode, SunShadow, ViewShadow,illuArray, 
		+	phoRefVal, status )

Necessary include files
from calling routine 
or program:		pho.h (for C routines )
			pho.fin ( for FORTRAN routines )



Parameters:
	
Name		Type	In/Out	Description
	

PHO_DATA	PHO	Input	Address of the photometric 
				data object

illMode		int	Input	The illMode parameter indicates how 
				illumination is entered in the routine. 
				phoCorrect accepts four different 
				representations of the illumination 
				conditions : 
					illEllCos --> case a);
					illDTMCos --> case b);
					illEllDir --> case c);
					illDTMDir --> case d)


SunShadow	int	Input	The parameter indicates if the point is 
				in the "Sun Shadow"

ViewShadow  	int	Input	The parameter indicates if the point is 
				in the "View Shadow"


illuArray   double	Input	These arrays contain the illumination 
				geometry respectively.  The contence 
				belongs to the *illMode :

				illEllCos --> case a :

				DOUBLE PRECISION illuArray(3) 
				DOUBLE PRECISION illuArray(3)
				illuArray(1)=cos(incidenceAngle) 
				illuArray(2)=cos(emissionAngle) 
				illuArray(3)=cos(phaseAngle) 

				illDTMCos --> case b :

				DOUBLE PRECISION illuArray(5) 
				DOUBLE PRECISION illuArray(5)
				illuArray(1)=cos(incidenceAngle2Ellips) 
				illuArray(2)=cos(emissionAngle2Ellips) 
				illuArray(3)=cos(phaseAngle) 
				illuArray(4)=cos(incidenceAngle2Surf) 
				illuArray(5)=cos(emissionAngle2Surface) 

				illEllDir --> case c :

				DOUBLE PRECISION illuArray(9) 
				DOUBLE PRECISION illuArray(9)
				illuArray(1)=incidenceDirection(1) 
				illuArray(2)=incidenceDirection(2) 
				illuArray(3)=incidenceDirection(3) 
				illuArray(4)=emissionDirection(1) 
				illuArray(5)=emissionDirection(2) 
				illuArray(6)=emissionDirection(2) 
				illuArray(7)=EllipsNormalDirection(1) 
				illuArray(8)=EllipsNormalDirection(2) 
				illuArray(9)=EllipsNormalDirection(3) 

				illDTMDir --> case d :

				DOUBLE PRECISION illuArray(12) 
				DOUBLE PRECISION illuArray(12)
				illuArray(1)=incidenceDirection(1) 
				illuArray(2)=incidenceDirection(2) 
				illuArray(3)=incidenceDirection(3) 
				illuArray(4)=emissionDirection(1) 
				illuArray(5)=emissionDirection(2) 
				illuArray(6)=emissionDirection(2) 
				illuArray(7)=EllipsNormalDirection(1) 
				illuArray(8)=EllipsNormalDirection(2) 
				illuArray(9)=EllipsNormalDirection(3) 
				illuArray(10)=SurfNormalDirection(1) 
				illuArray(11)=SurfNormalDirection(2) 
				illuArray(12)=SurfNormalDirection(3) 


illum	 PHO_ILLUM	Input	These unions contain the illumination 
				geometry :
						
				For all representations of the 
				illumination geometry one of the makro 
				illShadow or illNoShadow is to be filled 
				in the union:

				illum.type.sunshadow=...
				illum.type.viewshadow=...

				In relation to the case of the 
				representation of the illumination 
				geometry :

				illEllCos --> case a) .
				...
				double incidAngle2Ellips;
				double emAngle2Ellips;
				double phaseAngle;,
				...
				illum.mode=illEllCos;
				illum.cos.inc=cos(incidAngle2Ellips);
				illum.cos.em=cos(emAngle2Ellips);
				illum.cos.phas=cos(phaseAngle);

				illDTMCos --> case b) :
				...
				double inciAngle2Ellips;
				double emAngle2Ellips;
				double inciAngle2Surf;
				double emAngle2Surf;
				double phaseAngle;
				...
				illum.mode=illDTMCos;
				illum.cos.inc=cos(inciAngle2Ellips);
				illum.cos.emw=cos(emAngle2Ellips);
				illum.cos.phas=cos(phaseAngle);
				illum.cos.inc_surf=cos(inciAngle2Surf);
				illum.cos.em_surf=cos(emAngle2Surf);

				illEllDir --> case c) .
				....
				double incidDir(3);
				double emDir(3);
				double ellDir(3);
				....
				illum.mode=illEllDir;
				memcpy(&(illum.direction.inc), incidDir, 
				  3*sizeof(double));
				memcpy(&(illum.direction.em), emDir, 
				  3*sizeof(double));
				memcpy(&(illum.direction.ellips), ellDir, 
				  3*sizeof(double));

				illDTMDir --> case d) .
				...
				double incidDir(3);
				double emDir(3);
				double ellDir(3);
				double surfDir(3);
				...
				illum.mode=illDTMDir;
				memcpy(&(illum.direction.inc), incidDir, 
				  3*sizeof(double));
				memcpy(&(illum.direction.em), emDir, 
				  3*sizeof(double));
				memcpy(&(illum.direction.ellips), ellDir, 
				  3*sizeof(double));
				memcpy(&(illum.direction.surf),  surfDir, 
				 3*sizeof(double));

phoRefVal	double	Output	Value of the bidirectional reflectance


status		int	Output	Error status:
			phoSUCCESS - success
			phoFAILURE - failed
			phoFUNC_NOT_SET - failed : 
				photometric function not set in the PHO
			phoKEYWD_NOT_SET - failed :
				one of the required parameter values has 
				not been set in the PHO
			phoINVALID_KEYWD - failed :
				one of the parameter keywords is invalid
			phoINVALID_ILL_MODE - failed :
				invalid illMode
			phoARGUMENT_OUT_OF_RANGE-failed:
				one of the arguments is out of the valid range

===========================================================================

Background and References:

	2.	M. Minnaert, The reciprocity principle in Lunar photometry,
			Astrophysical Journal, Vol. 93, No. 2, p. 403-410, 1941
	3.	Joseph Veverka, J. Goguen, S. Young, J. Elliont, Scattering of 
			light from particulate surfaces. 
		I. A laboratory assessment of multiple-scattering effects.
			Icarus, Vol. 34, p. 406-414
	4.-6.	Bonnie J. Buratti, Voyager Disk Resolved Photometry of the 
			Saturnian Satellites,
			Icarus, Vol. 59, p. 392-405, 1984
		Bonnie J. Buratti, Joseph Veverka, Voyager Photometry of 
			Europa,
			Icarus, Vol. 55, p.93-110, 1983
		Steven W. Squyres, Joseph Veverka, Voyager Photometry of 
			Surface Features on Ganymede and Callisto, 
			Icarus, Vol. 46, p. 137-155,1981
	7.-8.	old VICAR Photometry programs
	9.	Bruce Hapke, Theory of Reflectance Spectroscopy, 
			(Topics in Remote Sensing;3), 
			Cambridge University Press 1993, p. 199
	10.	Kari Lume, Edward Bowell, Radiative Transfer in Surfaces of 
			Atmosphereless Bodies.
		1. Theory  
			The Astronomical Journal, Vol. 86, No. 11, 
			p. 1694-1704, 1981
		2. Interpretation of Phase Curves, 
			The  Astronomical Journal, Vol. 86, No. 11, 
			p. 1705-1721, 1981
		3. Interpretation of Lunar Photometry, 
			The  Astronomical Journal, Vol. 87, No. 7, 
			p. 1076-1082, 1982
	11.	Bruce Hapke, Bidirectional Reflectance Spectroscopy.
		1. Theory
			Journal of Geophysical Research Vol. 86, No.. B4, 
			p. 3039-3054, 1981
	12.	Old VICAR Photometry programs
	13.-15.Bruce Hapke, Bidirectional Reflectance Spectroscopy.
		3. Correction for Macroscopic Roughness
			Icarus, Vol. 59, p.41-59, 1984
		4. The Extinction Coefficient and the Opposition Effect
			Icarus, Vol. 67, p. 264-280, 1986
	16.	Deborah Dominique, A Simple Method for Comparing Shadow-Hiding 
	 		and Coherent Backscatter Models of the Opposition Effect
			The 24th Annual Meeting of the Division for Planetary 
			Sciences, 
			12-16 October 1992, Munich, Germany, 
			Bulletin of the American Astronomical Society, 
			Annual Report of the AAS, DPS Abstracts, 
			DDA/HAD Abstracts, Vol. 24, No. 3, 1992, p.958
		Bruce Hapke, Coherent Backscatter and the Radar Characteristics 
			of Outer Planet Satellites,
			Icarus, Vol. 88, 407-417, 1990
		Bruce Hapke, Bidirectional Reflectance Spectroscopy.
		3. Correction for Macroscopic Roughness
			Icarus, Vol. 59, p.41-59, 1984
	17.	Peter Regner, Photometric Analysis for the Determination of 
			Physical and Structural Properties of the Martian 
			Surface in the Oxia Palus Region, 
			Thesis University Munich, DLR-FB 90-29, 1990

===========================================================================

Other Information:

Software Platforms:		VICAR 13 / TAE 5.2 
				(SUN_OS,SUN_SOLARIS,VAX,AXP)

Hardware Platforms:		No particular hardware required;
				tested on SUN_OS,SUN_SOLARIS,VAX,AXP

Programming Language:		ANSI-C, 
				ANSI-FORTRAN bridge

Specification by:		Friedel Oschuetz, DLR

Date of specification:		July '94

Cognizant programmer:		Friedel Oschuetz, DLR-NE-PE
				Institute of Planetary Exploration
				DLR
				12484 Berlin (FRG)



History:			Jan '94, L. Kamp, initial version
				Jul '94, F. Oschuetz, preliminary version

===========================================================================
$ Return
$!#############################################################################
$Test_File:
$ create tzpho_routines.c
#include <math.h>
#include "vicmain_c"
#include "pho.h"

/* Program PHO_TEST  */

void main44()
{
  int cnt, def, i, ival, ival1, num, illMode, status;
  float temp;
  double dval, dval1, IncAng, EmAng, PhasAng, *phoFuncVal;
  char subcmd[9], cval[133], cval1[133], msg[133],
  keylist[phoMAX_PARAM_PER_FUNC][phoMAX_KEYWD_LENGTH+1];
  char *pkeylist;
  PHO pho_obj;
  PHO_ILLUM Millum, Tillum;

  zvmessage(" program PHO_TEST", "");
  zvmessage( " ", "");

  zveaction("","");


  phoFuncVal = (double *)malloc((4 + phoMAX_PARAM_PER_FUNC) * sizeof(double));
  if( phoFuncVal == NULL ) 
  {
    zvmessage("*** main44: memory allocation failed***","");
    free(phoFuncVal);
    zmabend("main44 abend");
  }

  status = phoInit( &pho_obj);

/* get the photometric function and there input parameters from the PDF     */
/* and set these in the photometric object :				    */

  status = phoGetParms( pho_obj);
    if(status != phoSUCCESS) zmabend("phoGetParms failed in main44");	

/* get the number of parameters of the current photometric function : */

  status = phoGetKeys( pho_obj, 0, &num); 
    if(status != phoSUCCESS) zmabend("phoGetKeys failed in main44");	
  strcpy( msg, " parameter number = " );
  sprintf( cval1, " %i", num);
  strcat( msg, cval1);
  zvmessage( msg, "");

/* get the list of parameter keywords for the current photometric function : */

  pkeylist = (char *)malloc( phoMAX_PARAM_PER_FUNC * ( phoMAX_KEYWD_LENGTH+1 ) * sizeof(char));
	if (pkeylist == NULL )
	{
	  zvmessage(" ", "");
	  zvmessage("***phottest error  error***","");
	  zvmessage("*** memory allocation failed ***","");
	  free(pkeylist);
	  zmabend("phottest abend");
	}
  pkeylist = (char *)keylist;

  status = phoGetKeys( pho_obj, pkeylist, &num);
  if(status != phoSUCCESS) zmabend("phoGetKeys failed in main44");	

/* get the photometric function name : */

  status = phoGetFunc( pho_obj, cval1);
  if(status != phoSUCCESS) zmabend("phoGetFunc failed in main44");	
  strcpy( msg, " Function =" );
  strcat( msg, cval1);
  zvmessage( msg, "");
  zvmessage( " ", "");

  for (i=0; i<num; i++) {

    status = phoGetVal( pho_obj, keylist[i], &dval1);
      if(status != phoSUCCESS) zmabend("phoGetVal failed in main44");	
    strcpy( msg, keylist[i]);
    strcat( msg, " = ");
    sprintf( cval1, " %10.3e", dval1);
    strcat( msg, cval1);
    zvmessage( msg, "");
  }

/* reads in the function arguments from PDF and fill the illumination union: */

/* fill the illumination union for the meassured illumination conditions :*/

  zvmessage( " ", "");

  zvp("INC_ANG", &temp, &cnt);
  strcpy( msg, "Incidence Angle =  " );
  sprintf( cval1, " %10.3e", temp);
  strcat( msg, cval1);
  zvmessage( msg, "");

  Millum.cos.inc = cos(RETURN_RADIANS((double )temp));

  zvp("EM_ANG", &temp, &cnt);
  strcpy( msg, "Emission Angle =  " );
  sprintf( cval1, " %10.3e", temp);
  strcat( msg, cval1);
  zvmessage( msg, "");

  Millum.cos.em = cos(RETURN_RADIANS((double )temp));

  zvp("PHAS_ANG", &temp, &cnt);
  strcpy( msg, "Phase Angle =  " );
  sprintf( cval1, " %10.3e", temp);
  strcat( msg, cval1);
  zvmessage( msg, "");

  Millum.cos.phas = cos(RETURN_RADIANS((double )temp));

  zvmessage( " ", "");

  Millum.mode = illEllCos;
  Millum.type.sunshadow = illNoShadow;
  Millum.type.viewshadow = illNoShadow;

/* fill the illumination union for the target illumination conditions : */

  Tillum.mode = illEllCos;
  Tillum.type.sunshadow = illNoShadow;
  Tillum.type.viewshadow = illNoShadow;

  Tillum.cos.inc  = 1.0;
  Tillum.cos.em = 1.0;
  Tillum.cos.phas = 1.0;



/* get the Bidirectional Reflectance Value : */

  status = phoBidiRef( pho_obj, &Millum, phoFuncVal );

  strcpy( msg, "Bidirectional Reflectance Value (meassured illumin.) =");
  sprintf( cval1, " %10.3e", *phoFuncVal);
  strcat( msg, cval1);
  zvmessage( msg, "");

  status = phoBidiRef( pho_obj, &Tillum, phoFuncVal );

  strcpy( msg, "Bidirectional Reflectance Value (target illumin.=nadir/nadir) =");
  sprintf( cval1, " %10.3e", *phoFuncVal);
  strcat( msg, cval1);
  zvmessage( msg, "");

/* get the photometric function value : */

  status = phoFunc( pho_obj, &Millum, phoFuncVal );

  strcpy( msg, "Photometric Function Value (meassuered illumin.) =");
  sprintf( cval1, " %10.3e", *phoFuncVal);
  strcat( msg, cval1);
  zvmessage( msg, "");

  status = phoFunc( pho_obj, &Tillum, phoFuncVal );

  strcpy( msg, "Photometric Function Value (target illumin.=nadir/nadir) =");
  sprintf( cval1, " %10.3e", *phoFuncVal);
  strcat( msg, cval1);
  zvmessage( msg, "");

/* get the correction value to nadir viewing and illumination conditions : */

  status = phoCorrect( pho_obj, &Millum, &Tillum, phoFuncVal );

  strcpy( msg, " to-nadir Correction Value =");
  sprintf( cval1, " %10.3e", *phoFuncVal);
  strcat( msg, cval1);
  zvmessage( msg, "");

  zvmessage( " ", "");


  status = phoFree( pho_obj);
  return;
}
$!-----------------------------------------------------------------------------
$ create tzpho_routines.imake
#define PROGRAM tzpho_routines

#define MODULE_LIST tzpho_routines.c 

#define MAIN_LANG_C
#define USES_FORTRAN
#define USES_ANSI_C

#define TEST

/********************************************
LOCAL LIBRARY and DEBUGGER for development 

#define LIB_LOCAL
#define DEBUG
#define LIB_P2SUB_DEBUG

*******************************************/

#define LIB_P2SUB
#define LIB_RTL
#define LIB_TAE

$!-----------------------------------------------------------------------------
$ create tzpho_routines.pdf
process execute=tzpho_routines help=*



    parm PHO_FUNC	string 	count=0:1 	+
			valid=(	LAMBERT,	+
				MINNAERT,	+
				IRVINE,		+
				VEVERKA,	+
				BURATTI1,	+
				BURATTI2,	+
				BURATTI3,	+
				MOSHER,		+
				LUMME_BOWEL_HG1,+
				HAPKE_81_LE2,	+
				HAPKE_81_COOK,	+
				HAPKE_86_HG1,	+
				HAPKE_86_HG2,	+
				HAPKE_86_LE2,	+
				HAPKE_HG1_DOM,	+
				REGNER_HAPKE_HG1,+
				ATMO_CORR_REGNER) +
			default=MINNAERT

    parm ALBEDO 	real count=0:1 +
			valid=(0:1)	default=1.0
    parm EXPONENT 	real count=0:1 +
			valid=(0:1)	default=0.5
    parm A_VEVERKA 	real count=0:1 +
					default=--
    parm B_VEVERKA 	real count=0:1 +
					default=--
    parm C_VEVERKA 	real count=0:1 +
					default=--
    parm D_VEVERKA 	real count=0:1 +
					default=-- 
    parm MO_EXP1 	real count=0:1 +
					default=--
    parm MO_EXP2 	real count=0:1 +
					default=--
    parm E_BURATTI 	real count=0:1 +
					default=--
    parm DEN_SOIL 	real count=0:1 +
					default=--
    parm W_SOIL 	real count=0:1 +
			valid=(0:1)	default=--
    parm HG1_SOIL 	real count=0:1 +
					default=--
    parm HG2_SOIL 	real count=0:1 +
					default=--
    parm HG_ASY_SOIL 	real count=0:1 +
					default=--
    parm LE1_SOIL 	real count=0:1 +
					default=--
    parm LE2_SOIL 	real count=0:1 +
					default=--
    parm H_SHOE 	real count=0:1 +
					default=--
    parm B_SHOE 	real count=0:1 +
					default=--
    parm H_CBOE 	real count=0:1 +
					default=--
    parm B_CBOE 	real count=0:1 +
					default=--
    parm THETA 		real count=0:1 +
					default=--
    parm COOK 		real count=0:1 +
					default=--
    parm TAU_ATM 	real count=0:1 +
					default=--
    parm W_ATM 		real count=0:1 +
			valid=(0:1)	default=--
    parm HG1_ATM 	real count=0:1 +
					default=--
    parm IRV_EXP1 	real count=0:1 +
					default=--
    parm IRV_EXP2 	real count=0:1 +
					default=--
    parm INC_ANG	real count=0:1 +
					default=--
    parm EM_ANG 	real count=0:1 +
					default=--
    parm PHAS_ANG 	real count=0:1 +
					default=--

end-proc
 
.Title
 TZPHO_ROUTINES - test general photometric subroutine package

.HELP
 C test program for the general photometric subroutine package

.LEVEL1

.VARI PHO_FUNC
photometric function

.VARI ALBEDO
albedo

.VARI EXPONENT
Minnaert's konstant

.VARI A_VEVERKA 
Veverka parameter

.VARI B_VEVERKA
Veverka parameter

.VARI C_VEVERKA
Veverka parameter

.VARI D_VEVERKA
Veverka parameter

.VARI MO_EXP2
Mosher's exponent

.VARI MO_EXP1
Mosher's exponent

.VARI E_BURATTI
Buratti's parameter

.VARI DEN_SOIL
Hapke parameter

.VARI W_SOIL
Hapke parameter

.VARI HG1_SOIL
Hapke Parameter

.VARI HG2_SOIL
Hapke parameter

.VARI HG_ASY_SOIL
Hapke parameter

.VARI LE1_SOIL
Hapke parameter

.VARI LE2_SOIL
Hapke parameter

.VARI H_SHOE
Hapke parameter

.VARI B_SHOE
Hapke parameter

.VARI H_CBOE
Hapke-Dominique parameter

.VARI B_CBOE
Hapke-Dominique parameter

.VARI THETA
Hapke parameter

.VARI COOK
Hapke-Cook parameter

.VARI TAU_ATM
Regner parameter

.VARI W_ATM
Regner parameter

.VARI HG1_ATM
Regner parameter

.VARI IRV_EXP1
Irvine parameter

.VARI IRV_EXP2
Irvine parameter

.VARI INC_ANG
incidence angle

.VARI EM_ANG
emission angle

.VARI PHAS_ANG
phase angle

.LEVEL2

.VARI PHO_FUNC
Name of the photometric function

.VARI ALBEDO
Albedo -  valid for the Lambert and Minnaert photometric functions.

.VARI EXPONENT
Exponent - the geometrical constant k of the Minnaert photometric function.

.VARI A_VEVERKA 
Parameter of the Veverka, Squyres-Veverka and Mosher photometric functions.

.VARI B_VEVERKA
Parameter of the Veverka, Mosher, Squyres-Veverka and Buratti 
photometric functions.

.VARI C_VEVERKA
Parameter of the Veverka, Mosher, Squyres-Veverka and Buratti 
photometric functions.

.VARI D_VEVERKA
Parameter of the Veverka, Mosher, Squyres-Veverka and Buratti 
photometric functions.

.VARI E_BURATTI
Buratti's parameter for modification of the Veverka photometric function.

.VARI MO_EXP1
Modification of the coefficient k in the Minnaert part 
of Mosher's photometric function (goes along with MO_EXP2).

.VARI MO_EXP2
Modification of the coefficient k in the Minnaert part 
of Mosher's photometric function (goes along with MO_EXP1).

.VARI DEN_SOIL
Specific volume density of the soil.

.VARI W_SOIL
Single-scattering albedo of the soil particles. It characterizes the 
efficiency of an average particle to scatter and absorb light. 
One of the classical Hapke parameter.

.VARI HG1_SOIL
Parameter of the first term of the Henyey-Greenstein soil particle 
phase function.

.VARI HG2_SOIL
Parameter of the second term of the Henyey-Greenstein soil particle 
phase function.

.VARI HG_ASY_SOIL
Asymmetry parameter (weight of the two terms 
in the Henyey-Greenstein soil phase function).

.VARI LE1_SOIL
Parameter of the first term of the Legendre-Polynomial soil particle 
phase function.

.VARI LE2_SOIL
Parameter of the second term of the Legendre-Polynomial soil particle 
phase function.

.VARI H_SHOE
One of the classical Hapke parameter.
Parameter which characterizes the soil structure in the terms of porosity, 
particle-size distribution, and rate of compaction with depth (angular width 
of opposition surge due to shadowing). 

.VARI B_SHOE
One of the classical Hapke parameter. 
Opposition magnitude coefficient. The total amplitude of the opposition surge 
due to shadowing. It is the ratio of the light scattered from near the 
illuminated surface of the particle to the total amount of light scattered at 
zero phase : 
B_SHOE=S(0)/(W_SOIL*p(0))
with p(0) - soil phase function
S(0) - opposition surge amplitude term which characterizes the contribution of 
light scattered from near the front surface of individual particles at zero 
phase.
.page
For a true, shadow-hiding opposition effect, 0<=B_SHOE<=1.
However, there are several other phenomena that may also cause a surge in 
brightness at small phase angles. These including the following:
1) The coherent backscatter or weak photon localisation due to multiply 
   scattered light.
2) An single-particle opposition effect caused by complex porous agglomerates 
   ( soil phase function )
3) Glory caused by sperical particles ( soil phase function )
4) Internal reflections of transparent particles ( soil phase function )
   These various phenomena may be large enough to increase the opposition surge 
   by more than a factor of 2. This possibility may be taken into account by 
   allowing B_SHOE to be greater than 1.
 
.VARI H_CBOE
Parameter of the coherent backscattering ( angular width of the opposition 
surge due to multiply scattered light).
H_CBOE=lambda/(2*pi*L)
lambda - wavelength
L - the free path of the phonon in the medium

.VARI B_CBOE
Opposition magnitude coefficient of the coherent backscattering 
(height of opposition surge due to multiply scattered light). 

.VARI THETA
Average topographic slope angle of surface roughness at subresolution scale.
One of the classical Hapke parameter. 

.VARI COOK
 Parameter of the Cook's modification of the old Hapke function.

.VARI TAU_ATM
Optical depth of the atmosphere.

.VARI W_ATM
Single scattering albedo of the atmospheric aerosols.

.VARI HG1_ATM
Parameter of the first term of the Henyey-Greenstein atmospheric phase function.

.VARI IRV_EXP1
Parameter of the Irvine photometric function.

.VARI IRV_EXP2
Parameter of the Irvine photometric function.

.VARI INC_ANG
Incidence angle in degree.

.VARI EM_ANG
Emission angle in degree.

.VARI PHAS_ANG
Phase angle in degree.

.END
$!-----------------------------------------------------------------------------
$ create txpho_routines.f
c  Program TXPHO_ROUTINES

	INCLUDE 'VICMAIN_FOR'

	SUBROUTINE MAIN44

        INCLUDE 'pho'

	integer cnt, i, num, MillMode, TillMode, SunShadow, ViewShadow, status
	real*4 temp
c	real*8 dval1, IncAng, EmAng, PhasAng, pval
	real*8 dval1, pval
	real*8 MIllum(3),TIllum(3) !Illum(1,2,3)=[cos(inc),cos(em),cos(phase)] 
	character*132 msg
	character*(pho_max_func_name_length) cval1
c	character*8 cval1
c	character*(pho_max_key_length) keylist(4)
	character*(pho_max_keywd_length) keylist(pho_max_param_func)

	integer pho

	call xvmessage(' program PHO_TEST', ' ')
	call xvmessage( ' ', ' ')

	call xveaction(' ',' ')

	call pho_init( pho, status)
c get the photometric function and there input parameters from the PDF
c and set these in the photometric object :

	call pho_get_Parms( pho, status)

	i = 0

c get the list of parameter keywords for the current photometric function : 

	call pho_get_keys( pho, keylist, num, status)

c get the photometric function name :

	call pho_get_func( pho, cval1, status)

	msg = ' Function = '//cval1
	call xvmessage( msg, ' ')

	call xvmessage( ' ', ' ')

	  write( msg, 1000)  num
1000	  format( 'parameter number = ', i4)
	  call xvmessage( msg, ' ')

	do i=1,num
	  call pho_get_val( pho, keylist(i), dval1, status)
	  write( msg, 1010) keylist(i), dval1
1010	  format( a<pho_max_keywd_length>, ' = ', 1pe10.3)
	  call xvmessage( msg, ' ')
	enddo

c  reads in the function arguments from the PDF :

	call xvp('INC_ANG', temp, cnt)
	MIllum(1) = temp

	call xvp('EM_ANG', temp, cnt)
	MIllum(2) = temp

	call xvp('PHAS_ANG', temp, cnt)
	MIllum(3) = temp

	call xvmessage( ' ', ' ')

	write( msg, 1001) MIllum(1)
1001	format(' Incidence Angle = ', 1pd10.3)
	call xvmessage( msg, ' ')

	write( msg, 1002) MIllum(2)
1002	format(' Emission Angle = ', 1pd10.3)
	call xvmessage( msg, ' ')

	write( msg, 1003) MIllum(3)
1003	format(' Phase Angle = ', 1pd10.3)
	call xvmessage( msg, ' ')

	call xvmessage( ' ', ' ')


c get the bidirectional reflectance value :

	MillMode = illEllCos
	MIllum(1)=cos(MIllum(1)*rad_deg)
	MIllum(2)=cos(MIllum(2)*rad_deg)
	MIllum(3)=cos(MIllum(3)*rad_deg)

	SunShadow = illNoShadow
	ViewShadow = illNoShadow
	call pho_bidi_ref(pho,MillMode,SunShadow,ViewShadow,MIllum,pval,status) 

	write( msg, 1004) pval
1004	format(' Bidirectional Reflectance Value = ', 1pd10.3)
	call xvmessage( msg, ' ')


c get the photometric function value :

	MillMode = illEllCos
	call pho_func(pho, MillMode, SunShadow, ViewShadow, MIllum, 
     f	pval, status)

	write( msg, 1005) pval
1005	format(' Photometric Function value = ', 1pd10.3)
	call xvmessage( msg, ' ')


c get the photometric correction function value to nadir :

	MillMode = illEllCos
	TillMode = illEllCos

	TIllum(1) =1.0
	TIllum(2) =1.0
	TIllum(3) =1.0

	call pho_correct(pho,MillMode,SunShadow,ViewShadow,MIllum,
     f  TillMode,SunShadow,ViewShadow,TIllum,pval,status)

	write( msg, 1006) pval
1006	format(' to-nadir Correction Value = ', 1pd10.3)
	call xvmessage( msg, ' ')

	call xvmessage( ' ', ' ')

	call pho_free( pho, status)
	return
	end
$!-----------------------------------------------------------------------------
$ create txpho_routines.imake
#define PROGRAM txpho_routines

#define MODULE_LIST txpho_routines.f

#define FTNINC_LIST pho

#define MAIN_LANG_FORTRAN
#define USES_FORTRAN
#define USES_ANSI_C

#define FTN_STRING


#define TEST


/********************************************
LOCAL LIBRARY and DEBUGGER for development 

#define LIB_LOCAL
#define DEBUG
#define LIB_P2SUB_DEBUG

*******************************************/

#define LIB_P2SUB
#define LIB_RTL
#define LIB_TAE


$!-----------------------------------------------------------------------------
$ create txpho_routines.pdf
Process help=*

    parm PHO_FUNC	string 	count=0:1 	+
			valid=(	LAMBERT,	+
				MINNAERT,	+
				IRVINE,		+
				SQUE_VEV,	+
				VEVERKA,	+
				BURATTI1,	+
				BURATTI2,	+
				BURATTI3,	+
				MOSHER,		+
				LUMME_BOWEL_HG1,+
				HAPKE_81_LE2,	+
				HAPKE_81_COOK,	+
				HAPKE_86_HG1,	+
				HAPKE_86_HG2,	+
				HAPKE_86_LE2,	+
				HAPKE_HG1_DOM,	+
				REGNER_HAPKE_HG1,+
				ATMO_CORR_REGNER) +
			default=--

    parm ALBEDO 	real count=0:1 +
			valid=(0:1)	default=--
    parm EXPONENT 	real count=0:1 +
			valid=(0:1)	default=--
    parm A_VEVERKA 	real count=0:1 +
					default=--
    parm B_VEVERKA 	real count=0:1 +
					default=--
    parm C_VEVERKA 	real count=0:1 +
					default=--
    parm D_VEVERKA 	real count=0:1 +
					default=-- 
    parm MO_EXP1 	real count=0:1 +
					default=--
    parm MO_EXP2 	real count=0:1 +
					default=--
    parm E_BURATTI 	real count=0:1 +
					default=--
    parm DEN_SOIL 	real count=0:1 +
					default=--
    parm W_SOIL 	real count=0:1 +
			valid=(0:1)	default=--
    parm HG1_SOIL 	real count=0:1 +
					default=--
    parm HG2_SOIL 	real count=0:1 +
					default=--
    parm HG_ASY_SOIL 	real count=0:1 +
					default=--
    parm LE1_SOIL 	real count=0:1 +
					default=--
    parm LE2_SOIL 	real count=0:1 +
					default=--
    parm H_SHOE 	real count=0:1 +
					default=--
    parm B_SHOE 	real count=0:1 +
					default=--
    parm H_CBOE 	real count=0:1 +
					default=--
    parm B_CBOE 	real count=0:1 +
					default=--
    parm THETA 		real count=0:1 +
					default=--
    parm COOK 		real count=0:1 +
					default=--
    parm TAU_ATM 	real count=0:1 +
					default=--
    parm W_ATM 		real count=0:1 +
			valid=(0:1)	default=--
    parm HG1_ATM 	real count=0:1 +
					default=--
    parm IRV_EXP1 	real count=0:1 +
					default=--
    parm IRV_EXP2 	real count=0:1 +
					default=--
    parm INC_ANG	real count=0:1 +
					default=--
    parm EM_ANG 	real count=0:1 +
					default=--
    parm PHAS_ANG 	real count=0:1 +
					default=--
end-proc
 
.Title
 TXPHO_ROUTINES - test general photometric subroutine package

.HELP
 FORTRAN test program for the photometric subroutine package


.LEVEL1

.VARI PHO_FUNC
photometric function

.VARI ALBEDO
albedo

.VARI EXPONENT
Minnaert's konstant

.VARI A_VEVERKA 
Veverka parameter

.VARI B_VEVERKA
Veverka parameter

.VARI C_VEVERKA
Veverka parameter

.VARI D_VEVERKA
Veverka parameter

.VARI MO_EXP2
Mosher's exponent

.VARI MO_EXP1
Mosher's exponent

.VARI E_BURATTI
Buratti's parameter

.VARI DEN_SOIL
Hapke parameter

.VARI W_SOIL
Hapke parameter

.VARI HG1_SOIL
Hapke Parameter

.VARI HG2_SOIL
Hapke parameter

.VARI HG_ASY_SOIL
Hapke parameter

.VARI LE1_SOIL
Hapke parameter

.VARI LE2_SOIL
Hapke parameter

.VARI H_SHOE
Hapke parameter

.VARI B_SHOE
Hapke parameter

.VARI H_CBOE
Hapke-Dominique parameter

.VARI B_CBOE
Hapke-Dominique parameter

.VARI THETA
Hapke parameter

.VARI COOK
Hapke-Cook parameter

.VARI TAU_ATM
Regner parameter

.VARI W_ATM
Regner parameter

.VARI HG1_ATM
Regner parameter

.VARI IRV_EXP1
Irvine parameter

.VARI IRV_EXP2
Irvine parameter

.VARI INC_ANG
incidence angle

.VARI EM_ANG
emission angle

.VARI PHAS_ANG
phase angle

.LEVEL2

.VARI PHO_FUNC
Name of the photometric function

.VARI ALBEDO
Albedo -  valid for the Lambert and Minnaert photometric functions.

.VARI EXPONENT
Exponent - the geometrical constant k of the Minnaert photometric function.

.VARI A_VEVERKA 
Parameter of the Veverka, Squyres-Veverka and Mosher photometric functions.

.VARI B_VEVERKA
Parameter of the Veverka, Mosher, Squyres-Veverka and Buratti 
photometric functions.

.VARI C_VEVERKA
Parameter of the Veverka, Mosher, Squyres-Veverka and Buratti 
photometric functions.

.VARI D_VEVERKA
Parameter of the Veverka, Mosher, Squyres-Veverka and Buratti 
photometric functions.

.VARI E_BURATTI
Buratti's parameter for modification of the Veverka photometric function.

.VARI MO_EXP1
Modification of the coefficient k in the Minnaert part 
of Mosher's photometric function (goes along with MO_EXP2).

.VARI MO_EXP2
Modification of the coefficient k in the Minnaert part 
of Mosher's photometric function (goes along with MO_EXP1).

.VARI DEN_SOIL
Specific volume density of the soil.

.VARI W_SOIL
Single-scattering albedo of the soil particles. It characterizes the 
efficiency of an average particle to scatter and absorb light. 
One of the classical Hapke parameter.

.VARI HG1_SOIL
Parameter of the first term of the Henyey-Greenstein soil particle 
phase function.

.VARI HG2_SOIL
Parameter of the second term of the Henyey-Greenstein soil particle 
phase function.

.VARI HG_ASY_SOIL
Asymmetry parameter (weight of the two terms 
in the Henyey-Greenstein soil phase function).

.VARI LE1_SOIL
Parameter of the first term of the Legendre-Polynomial soil particle 
phase function.

.VARI LE2_SOIL
Parameter of the second term of the Legendre-Polynomial soil particle 
phase function.

.VARI H_SHOE
One of the classical Hapke parameter.
Parameter which characterizes the soil structure in the terms of porosity, 
particle-size distribution, and rate of compaction with depth (angular width 
of opposition surge due to shadowing). 

.VARI B_SHOE
One of the classical Hapke parameter. 
Opposition magnitude coefficient. The total amplitude of the opposition surge 
due to shadowing. It is the ratio of the light scattered from near the 
illuminated surface of the particle to the total amount of light scattered at 
zero phase : 
B_SHOE=S(0)/(W_SOIL*p(0))
with p(0) - soil phase function
S(0) - opposition surge amplitude term which characterizes the contribution of 
light scattered from near the front surface of individual particles at zero 
phase.
.page
For a true, shadow-hiding opposition effect, 0<=B_SHOE<=1.
However, there are several other phenomena that may also cause a surge in 
brightness at small phase angles. These including the following:
1) The coherent backscatter or weak photon localisation due to multiply 
   scattered light.
2) An single-particle opposition effect caused by complex porous agglomerates 
   ( soil phase function )
3) Glory caused by sperical particles ( soil phase function )
4) Internal reflections of transparent particles ( soil phase function )
   These various phenomena may be large enough to increase the opposition surge 
   by more than a factor of 2. This possibility may be taken into account by 
   allowing B_SHOE to be greater than 1.
 
.VARI H_CBOE
Parameter of the coherent backscattering ( angular width of the opposition 
surge due to multiply scattered light).
H_CBOE=lambda/(2*pi*L)
lambda - wavelength
L - the free path of the phonon in the medium

.VARI B_CBOE
Opposition magnitude coefficient of the coherent backscattering 
(height of opposition surge due to multiply scattered light). 

.VARI THETA
Average topographic slope angle of surface roughness at subresolution scale.
One of the classical Hapke parameter. 

.VARI COOK
 Parameter of the Cook's modification of the old Hapke function.

.VARI TAU_ATM
Optical depth of the atmosphere.

.VARI W_ATM
Single scattering albedo of the atmospheric aerosols.

.VARI HG1_ATM
Parameter of the first term of the Henyey-Greenstein atmospheric phase function.

.VARI IRV_EXP1
Parameter of the Irvine photometric function.

.VARI IRV_EXP2
Parameter of the Irvine photometric function.

.VARI INC_ANG
Incidence angle in degree.

.VARI EM_ANG
Emission angle in degree.

.VARI PHAS_ANG
Phase angle in degree.

.END
$!-----------------------------------------------------------------------------
$ create tstpho_routines.pdf
procedure
    PARMSET name= tstphopdf_parms
    	parm PHO_FUNC	string 	count=0:1 	+
			valid=(	LAMBERT,	+
				MINNAERT,	+
				IRVINE,		+
				VEVERKA,	+
				BURATTI1,	+
				BURATTI2,	+
				BURATTI3,	+
				MOSHER,		+
				LUMME_BOWEL_HG1,+
				HAPKE_81_LE2,	+
				HAPKE_81_COOK,	+
				HAPKE_86_HG1,	+
				HAPKE_86_HG2,	+
				HAPKE_86_LE2,	+
				HAPKE_HG1_DOM,	+
				REGNER_HAPKE_HG1,+
				ATMO_CORR_REGNER) +
			default=MINNAERT

    	parm ALBEDO 	real count=0:1 +
			valid=(0:1)	default=1.0
    	parm EXPONENT 	real count=0:1 +
			valid=(0:1)	default=0.5
    	parm A_VEVERKA 	real count=0:1 +
					default=1.0
    	parm B_VEVERKA 	real count=0:1 +
					default=0.005
    	parm C_VEVERKA 	real count=0:1 +
					default=0.5
    	parm D_VEVERKA 	real count=0:1 +
					default=0.05 
    	parm MO_EXP1 	real count=0:1 +
					default=0.05
    	parm MO_EXP2 	real count=0:1 +
					default=.005
    	parm E_BURATTI 	real count=0:1 +
					default=2.0
    	parm DEN_SOIL 	real count=0:1 +
					default=0.005
    	parm W_SOIL 	real count=0:1 +
			valid=(0:1)	default=0.7
    	parm HG1_SOIL 	real count=0:1 +
					default=5.0
    	parm HG2_SOIL 	real count=0:1 +
					default=3.0
    	parm HG_ASY_SOIL 	real count=0:1 +
					default=1.0
    	parm LE1_SOIL 	real count=0:1 +
					default=3.0
   	parm LE2_SOIL 	real count=0:1 +
					default=2.0
    	parm H_SHOE 	real count=0:1 +
					default=0.4
    	parm B_SHOE 	real count=0:1 +
					default=0.22
    	parm H_CBOE 	real count=0:1 +
					default=0.3
   	parm B_CBOE 	real count=0:1 +
					default=0.1
    	parm THETA 	real count=0:1 +
					default=10.0
   	parm COOK 	real count=0:1 +
					default=0.9
    	parm TAU_ATM 	real count=0:1 +
					default=0.4
    	parm W_ATM 		real count=0:1 +
			valid=(0:1)	default=0.9
    	parm HG1_ATM 	real count=0:1 +
					default=3.0
    	parm IRV_EXP1 	real count=0:1 +
					default=0.8
    	parm IRV_EXP2 	real count=0:1 +
					default=0.08
    	parm INC_ANG	real count=0:1 +
					default=10.0
    	parm EM_ANG 	real count=0:1 +
					default=50.0
    	parm PHAS_ANG 	real count=0:1 +
					default=55.0
   end-proc

body

tstphopdf_parms |save=tstphopdf_parms|

tzpho_routines |restore=tstphopdf_parms|	PHO_FUNC=MINNAERT

txpho_routines |restore=tstphopdf_parms|	PHO_FUNC=MOSHER

end-proc
$ Return
$!#############################################################################
