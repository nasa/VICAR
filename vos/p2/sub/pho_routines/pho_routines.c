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

