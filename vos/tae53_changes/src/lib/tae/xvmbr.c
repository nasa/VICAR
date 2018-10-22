/****************************************************************************
 *      Copyright (c) 1993, 1994
 *      Century Computing, Inc.
 *      ALL RIGHTS RESERVED
 *
 *      The software (programs, data bases and/or documentation) on or in
 *      any media can not be reproduced, disclosed, or used except under
 *      the terms of the TAE Plus Software License Agreement.
 *
 ***************************************************************************/



/* FORTRAN to "C" outer bridge */
/*
 *	TAE version 4 XV bridges for manipulation of Dynamic Memory Variables
 *	FORTRAN-callable.
 *
 *	CHANGE LOG:
 *	6/22/89 Initial Cut...rsg/AS
 *	26-feb-90	Added xvm_copy, xvm_find, xvm_findvar...krw
 *	15-mar-90	Made all BRIDGE1 and BRIDGE2 names the same, so
 *			it will all work on the VAXStation..krw
 *
 */

#include	"taeconf.inp"	/* TAE configuration (REQUIRED)		*/
#include	"forstr.inp"	/* fortran-77 string struct		*/
#include "taeintproto.h"

FUNCTION VOID BRIDGE2_NAME(xvm_call) 
(
	TAEINT *vmid,		/* in:id for a vm object. */
	TAEINT *status		/* out:  SUCCESS or error coded */  

 );
FUNCTION VOID BRIDGE2_NAME(xvm_copy) 
(
        TAEINT *targetId,       /* in:id for target vm object. */
        TAEINT *sourceId,       /* in:id for source vm object. */
	TAEINT *status		/* out:  SUCCESS or error coded */  

 );
FUNCTION VOID BRIDGE2_NAME(xvm_get_attribute) 
(
	TAEINT *vmid,	/* in:id for a vm object. */
	FORSTR *name,	/* in: name of the variable to extract attributes */
	TAEINT *type,	/* out: type of the variable. */
	TAEINT *n,	/* out: current number of values for the variable. */
	BOOL   *dflt,	/* out: TRUE if value of variable is the default . */
	TAEINT *access,	/* out: if variable is a file, the access mode.  */
	TAEINT *status	/* out:  SUCCESS or error code */

 );
FUNCTION VOID BRIDGE2_NAME(xvm_get_intg) 
(
	TAEINT *vmid,		/* in: id for a vm object. */
	FORSTR *varnam,		/* in: name of the integer variable to locate */
	TAEINT *dim,		/* in: maximum number of values expected. */
	TAEINT intg[],		/* out: current number of values for variable */
	TAEINT *count,		/* out:  actual number of variables received */
	TAEINT *status		/* out:  SUCCESS or error code */

 );
FUNCTION VOID BRIDGE2_NAME(xvm_get_dble) 
(
	TAEINT *vmid,		/* in:  id for a vm object */
	FORSTR *varnam,		/* in:  name of the real variable to extract */
	TAEINT *dim,		/* in: maximum number of values expected */
	double dble[],		/* out:  double precision values */
	TAEINT *count,		/* out:  actual number of strings received */
	TAEINT *status		/* out: SUCCESS or error code */

 );
FUNCTION VOID BRIDGE2_NAME(xvm_get_real) 
(
	TAEINT *vmid,		/* in:  id for a vm object */
	FORSTR *varnam,		/* in:  name of the real variable to extract */
	TAEINT *dim,		/* in: maximum number of values expected */
	float  real[],		/* out:  single precision real values */
	TAEINT *count,		/* out:  actual number of strings received */
	TAEINT *status		/* out: SUCCESS or error code */

 );
FUNCTION VOID BRIDGE2_NAME(xvm_get_str) 
(
	TAEINT	*vmid,		/* in:	id for a vm object */
	FORSTR	*varnam,	/* in:	name of string variable to extract */
	TAEINT	*dim,		/* in:	maximum number of values expected */
	FORSTR	*string,	/* out:	Array of strings in FOR-77 format */
	TAEINT	*count,		/* out:	actual number of strings received */
	TAEINT	*status	/* out:	SUCCESS or error code */

 );
FUNCTION VOID BRIDGE2_NAME(xvm_find) 
(
        TAEINT *vmid,		/* in:id for vm object. */
	FORSTR *varnam,		/* in: name of the variable to locate */
        TAEINT *var,       	/* out: 'C' pointer to variable structure*/
	TAEINT *status		/* out:  SUCCESS or error coded */  

 );
FUNCTION VOID BRIDGE2_NAME(xvm_findvar) 
(
        TAEINT *vmid,		/* in:id for vm object. */
	FORSTR *varnam,		/* in: name of the variable to locate */
        TAEINT *var,       	/* out: 'C' pointer to variable structure*/
	TAEINT *status		/* out:  SUCCESS or error coded */  

 );
FUNCTION VOID BRIDGE2_NAME(xvm_free) 
(
	TAEINT	*vmid,			/*  in:	  id of a vm object */
	TAEINT	*status		/*  out:  SUCCESS of error code */

 );
FUNCTION VOID BRIDGE2_NAME(xvm_format_var) 
(
	TAEINT *vmid,	/* in:  id of a vm object */
	FORSTR *line,	/* out: string containing variables from Vm object */
	TAEINT *length,	/* in:  length of output line in bytes */
	TAEINT *status		/*  out:  SUCCESS of error code */

 );
FUNCTION VOID BRIDGE2_NAME(xvm_get_host_err) 
(
	TAEINT *vmid,		/* in:  id of a vm object */
	TAEINT *vmcode,		/* out:  host-dependent error code */
	TAEINT *status		/* out:  SUCCESS or error code */

 );
FUNCTION VOID BRIDGE2_NAME(xvm_dyn_tutor) 
(
	TAEINT *vmid,		/* in: id of a vm object */
	FORSTR *pdfnam,		/* in: pdf file spec for tae monitor */
	TAEINT *pdfset,		/* in:  mode */
	TAEINT *status		/* out: SUCCESS or error code */

 );
FUNCTION VOID BRIDGE2_NAME(xvm_init_format) 
(
	TAEINT *vmid,		/* in:  id of a vm object */
	TAEINT *status		/* out: SUCCESS or error code */ 

 );
FUNCTION VOID BRIDGE2_NAME(xvm_new) 
(
	TAEINT *vmid,		/* out: id of a vm object */
	TAEINT *mode,		/* in: how function should handle errors */
	TAEINT *status		/* out: SUCCESS or error code */

 );
FUNCTION VOID BRIDGE2_NAME(xvm_init) 
(
TAEINT *vmid,		/* in: id of a vm object */
TAEINT *lun,		/* in: logical unit number */
TAEINT *status		/* out: SUCCESS or error code */

 );
FUNCTION VOID BRIDGE2_NAME(xvm_read_from_disk) 
(
	TAEINT *vmid,		/* in: id of a vm object */
	FORSTR *fspec,		/* in: host file specification of file */
	TAEINT *status		/* out: SUCCESS or error code */

 );
FUNCTION VOID BRIDGE2_NAME(xvm_read_from_tm) 
(
	TAEINT *vmid,		/* in: id of a vm object */
	TAEINT *status		/* out: SUCCESS or error code */

 );
FUNCTION VOID BRIDGE2_NAME(xvm_set_intg) 
(
	TAEINT *vmid,	/* in: id of a vm object */
	FORSTR *name,	/* in: name of the tcl integer variable to be set */
	TAEINT *count,	/* in: multiplicity of the variable */
	TAEINT intg[],	/* in: an array of integer values for the variables */
	TAEINT *mode,	/* in: variable being added or being modified */
	TAEINT *status	/* out: SUCCESS or error code */

 );
FUNCTION VOID BRIDGE2_NAME(xvm_set_valid_intg) 
(	
	TAEINT *vmid,		/* in: id of a vm object */
	FORSTR *name,		/* in: name of tcl integer variable */
	TAEINT *count,		/* in: multiplicity of the valids */
	TAEINT ilo[],		/* in: array of integers for low values */
	TAEINT ihi[],		/* in: array of integers for high values */
	TAEINT *status		/* out: SUCCESS or error code */

	);
FUNCTION VOID BRIDGE2_NAME(xvm_set_max) 
(	
	TAEINT *vmid,		/* in: id of a vm object */
	FORSTR *name,		/* in: name of the variable */
	TAEINT *maxcnt,		/* in: max vector count of variable */
	TAEINT *status		/* out: SUCCESS or error code */

	);
FUNCTION VOID BRIDGE2_NAME(xvm_set_min) 
(	
	TAEINT *vmid,		/* in: id of a vm object */
	FORSTR *name,		/* in: name of the variable */
	TAEINT *mincnt,		/* in: min vector count of variable */
	TAEINT *status		/* out: SUCCESS or error code */

	);
FUNCTION VOID BRIDGE2_NAME(xvm_set_next_menu) 
(	
	TAEINT *vmid,		/* in: id of a vm object */
	FORSTR *name,		/* in: name of the variable */
	TAEINT *status		/* out: SUCCESS or error code */

	);
FUNCTION VOID BRIDGE2_NAME(xvm_set_parm_page) 
(	
	TAEINT *vmid,		/* in: id of a vm object */
	FORSTR *name,		/* in: FORSTR *name of the variable */
	BOOL   *flag,		/* in: sets page indicator */
	TAEINT *status		/* out: SUCCESS or error code */

	);
FUNCTION VOID BRIDGE2_NAME(xvm_set_dble) 
(	
	TAEINT *vmid,		/* in: id of a vm object */
	FORSTR *name,		/* in: name of the variable */
	TAEINT *count,		/* in: number of real values in the real array */
	double dble[],		/* in: array of double precision values */
	TAEINT *mode,		/* in: variable being added or being modified */	
	TAEINT *status		/* out: SUCCESS or error code */

	);
FUNCTION VOID BRIDGE2_NAME(xvm_set_real) 
(	
	TAEINT *vmid,		/* in: id of a vm object */
	FORSTR *name,		/* in: name of the variable */
	TAEINT *count,		/* in: number of real values in the real array */
	float  real[],		/* in: array of real values for the variable */
	TAEINT *mode,		/* in: variable being added or being modified */	
	TAEINT *status		/* out: SUCCESS or error code */

	);
FUNCTION VOID BRIDGE2_NAME(xvm_set_valid_dble) 
(	
	TAEINT *vmid,		/* in: id of a vm object */
	FORSTR *name,		/* in: name of the tcl integer variable. */
	TAEINT *count,		/* in: multiplicity of the valids */
	double dlo[],		/* in: array of doubles for the low values */
	double dhi[],		/* in: array of doubles for the high values */
	TAEINT *status		/* out: SUCCESS or error code */

	);
FUNCTION VOID BRIDGE2_NAME(xvm_set_valid_real) 
(	
	TAEINT *vmid,		/* in: id of a vm object */
	FORSTR *name,		/* in: name of the tcl integer variable. */
	TAEINT *count,		/* in: multiplicity of the valids */
	float  rlo[],		/* in: array of reals for the low values */
	float  rhi[],		/* in: array of reals for the high values */
	TAEINT *status		/* out: SUCCESS or error code */

	);
FUNCTION VOID BRIDGE2_NAME(xvm_set_string_length) 
(	
	TAEINT *vmid,	/* in: id of a vm object */
	FORSTR *name,	/* in: name of tcl variable or keyword */
	TAEINT *strlen,	/* in: max length of a string for the variable */
	TAEINT *status	/* out: SUCCESS or error code */

	);
FUNCTION VOID BRIDGE2_NAME(xvm_set_string) 
(	
	TAEINT *vmid,		/* in: id of a vm object */
	FORSTR *name,		/* in: name of tcl string variable */
	TAEINT *count,		/* in: multiplicity of the variable */
	FORSTR *string,		/* in: array of strings F77 format */
	TAEINT *mode,		/* in: variable being added or being modified */
	TAEINT *status		/* out: SUCCESS or error code */

	);
FUNCTION VOID BRIDGE2_NAME(xvm_set_valid_string) 
(	
TAEINT *vmid,		/* in: id of a vm object */
FORSTR *name,		/* in: name of the variable */
TAEINT *count,		/* in: multiplicity of the valids */
FORSTR vector[],	/* in: array of strings for the variable */
TAEINT *status		/* out: SUCCESS or error code */

	);
FUNCTION VOID BRIDGE2_NAME(xvm_set_tcl_var) 
(	
	TAEINT *vmid,		/* in: id of a vm object */
	TAEINT *status		/* out: SUCCESS or error code */

	);
FUNCTION VOID BRIDGE2_NAME(xvm_write_to_disk) 
(	
	TAEINT *vmid,		/* in: id of a vm object */
	FORSTR *fspec,		/* host file specification of file to be created */
	TAEINT *status		/* out: SUCCESS or error code */

	);

/*
 *	 xvm_call -- installation dependent initialization
 */

FUNCTION VOID BRIDGE1_NAME(xvm_call) 
(
	TAEINT *vmid,		/* in:id for a vm object. */
	TAEINT *status		/* out:  SUCCESS or error coded */  

 )
	{
		BRIDGE2_NAME (xvm_call) (vmid, status);

		return;
	}
/*
 * xvm_call is for installation-provided application initialization.  
 * xvm_call is called automatically by xv_init
 * The xvm_call provided with the tae delivery performs no function.
 */

/*
 *	 xvm_copy -- copy one vm object to another
 */

FUNCTION VOID BRIDGE1_NAME(xvm_copy) 
(
	TAEINT *targetId,	/* in:id for target vm object. */
	TAEINT *sourceId,	/* in:id for source vm object. */
	TAEINT *status		/* out:  SUCCESS or error coded */  

 )
	{
		BRIDGE2_NAME (xvm_copy) (targetId, sourceId, status);

		return;
	}
#ifdef VMFIND
/*
 *	 xvm_find -- find the specified variable in the vm object
 *		     Abort if not found
 */

FUNCTION VOID BRIDGE1_NAME(xvm_find) 
(
	TAEINT *vmid,		/* in:id for a vm object. */
	TEXT   *varnam,		/* in: name of the variable to locate */
	TAEINT *var,		/* out: 'C' pointer to variable structure */
	TAEINT *status,		/* out:  SUCCESS or error code */
	STRLEN varnaml		/* in: length of varname */
 )
	{
		FORSTR varnamd;

		varnamd.length = GETLEN (varnaml);
		varnamd.pointer = varnam;
		BRIDGE2_NAME(xvm_find) (vmid, &varnamd, var, status);

		return;
	}

/*
 *	 xvm_findvar -- find the specified variable in the vm object
 */

FUNCTION VOID BRIDGE1_NAME(xvm_findvar) 
(
	TAEINT *vmid,		/* in:id for a vm object. */
	TEXT   *varnam,		/* in: name of the variable to locate */
	TAEINT *var,		/* out: 'C' pointer to variable structure */
	TAEINT *status,		/* out:  SUCCESS or error code */
	STRLEN varnaml		/* in: length of varname */
 )
	{
		FORSTR varnamd;

		varnamd.length = GETLEN (varnaml);
		varnamd.pointer = varnam;
		BRIDGE2_NAME(xvm_findvar) (vmid, &varnamd, var, status);

		return;
	}

#endif
/*
 *	xvm_get_attribute -- get variable attributes
 */

FUNCTION VOID BRIDGE1_NAME(xvm_get_attribute)
(
	TAEINT *vmid,	/* in: id for a vm object. */
	TEXT   *name,	/* in: name of the variable for which the attributes 
					are to be extracted. */
	TAEINT *type,	/* out: the type of the variable.  */
	TAEINT *n,	/* out: current number of values for the variable. */
	BOOL   *dflt,	/* out: TRUE if value of the variable is the default */
	TAEINT *access,	/* out: if variable is a file, this is access mode.  */
	TAEINT *status,	/* out:  SUCCESS or error code */
	STRLEN namel

 )
	{
		FORSTR named;

		named.length = GETLEN(namel);
		named.pointer = name;

		BRIDGE2_NAME(xvm_get_attribute) 
				(vmid, &named, type, n, dflt, access, status);

		return;
	}


/*
 *	xvm_get_intg -- get an integer variable
 */
/* xvm_get_intg is used to retrieve individual integer variables from the vm . 
 * object. if the variable is not found and the vm object has been created 
 * by xvm_new with mode = xabort, the process will be aborted.
 */

FUNCTION VOID BRIDGE1_NAME(xvm_get_intg) 
(
	TAEINT *vmid,		/* in: id for a vm object. */
	TEXT   *varnam,		/* in: name of the integer variable to locate */
	TAEINT *dim,		/* in: maximum number of values expected. */
	TAEINT intg[],		/* out: array for current values for the variable */
	TAEINT *count,		/* out:  actual number of variables received */
	TAEINT *status,		/* out:  SUCCESS or error code */
	STRLEN varnaml

 )
	{
		FORSTR varnamd;

		varnamd.length = GETLEN (varnaml);
		varnamd.pointer = varnam;
		BRIDGE2_NAME(xvm_get_intg) (vmid, &varnamd, dim, intg, count, status);
		return;
	}




/*
*	xvm_get_dble -- get  a double precision variable
*/
/* xvm_get_dble is used to retrieve individual real variables from the vm 
 *object.  
 */

FUNCTION VOID BRIDGE1_NAME(xvm_get_dble) 
(	
	TAEINT   *vmid,		/* in:  id for a vm object */
	TEXT     *varnam,	/* in:  name of the real variable to extract */
	TAEINT   *dim,		/* in: maximum number of values expected */
	double	 dble[],	/* out:  array of real values */
	TAEINT   *count,	/* out:  actual number of values received */
	TAEINT   *status,	/* out: SUCCESS or error code */
	STRLEN   varnaml

	)
	{
		FORSTR varnamd;

		varnamd.length = GETLEN (varnaml);
		varnamd.pointer = varnam;

		BRIDGE2_NAME(xvm_get_dble) 
			(vmid, &varnamd, dim, dble, count, status);
		return;
	}


/*
*	xvm_get_real -- get  a real variable
*/
/* xvm_get_real is used to retrieve individual real variables from the vm 
 *object.  
 */

/* 	FUNCTION VOID BRIDGE1_NAME(xvm_get_real)  */
/* 			(vmid, varnam, dim, real, count, status, varnaml)		 */
	
/* 	TAEINT   *vmid;		/\* in:  id for a vm object *\/ */
/* 	TEXT     *varnam;	/\* in:  name of the real variable to extract *\/ */
/* 	TAEINT   *dim;		/\* in: maximum number of values expected *\/ */
/* 	float	 real[];	/\* out:  array of real values *\/ */
/* 	TAEINT   *count;	/\* out:  actual number of values received *\/ */
/* 	TAEINT   *status;	/\* out: SUCCESS or error code *\/ */
/* 	STRLEN   varnaml; */

/* 	{ */
/* 		FORSTR varnamd; */

/* 		varnamd.length = GETLEN (varnaml); */
/* 		varnamd.pointer = varnam; */

/* 		BRIDGE2_NAME(xverel) (vmid, &varnamd, dim, real, count, status); */
/* 		return; */
/* 	} */


/*
*	xvm_get_str -- extract  a string variable
*/


FUNCTION VOID BRIDGE1_NAME(xvm_get_str) 
(	
	TAEINT *vmid,		/* in:	id for a vm object */
	TEXT   *varnam,		/* in:	name of the real variable to extract */
	TAEINT *dim,		/* in:	maximum number of values expected */
	TEXT   *string,		/* out:	Array of strings in FOR-77 format */
	TAEINT *count,		/* out:	actual number of strings received */
	TAEINT *status,		/* out:	SUCCESS or error code */
	STRLEN varnaml,
	STRLEN stringl 

	)
	{
		FORSTR varnamd;
		FORSTR stringd;

		varnamd.length = GETLEN (varnaml);
		varnamd.pointer = varnam;
		stringd.length = GETLEN (stringl);
		stringd.pointer = string;

		BRIDGE2_NAME(xvm_get_str) 
			(vmid, &varnamd, dim, &stringd, count, status);
		return;
	}




/*
 *	xvm_free -- deallocate an xv parameter object
 */
/* xv_free destroys the vm object, releasing all memory allocated to the object. */

FUNCTION VOID BRIDGE1_NAME(xvm_free) 
(	
	TAEINT *vmid,		/*  in:	  id of a vm object */
	TAEINT *status		/*  out:  SUCCESS of error code */

	)
	{
		BRIDGE2_NAME(xvm_free) (vmid, status);

		return;
	}



/*
*	xvm_format_var -- format variables
*/
/* xvm_format_var obtains parameter values and values of global and local 
 * variables in a displayable form, i. e. , as lines of text.  
 */

FUNCTION VOID BRIDGE1_NAME(xvm_format_var) 
(	
	TAEINT *vmid,	/* in:  id of a vm object */
	TEXT   *line,	/* out: string containing variables from Vm object */
	TAEINT *length,	/* in:  length of output line in bytes */
	TAEINT *status,	/* out:  SUCCESS or error code */
	STRLEN linel

	)
	{
		FORSTR lined;

		lined.length = GETLEN(linel);
		lined.pointer = line;

		BRIDGE2_NAME (xvm_format_var) (vmid, &lined, length, status);

		return;

	}



/*
   *	xvm_get_host_err -- get host-dependent error code
   */
/* xvm_get_host_err returns the host-dependent error code resulting from the
 *  last vm operation (e.g., a "file not found" error code). 
 */

FUNCTION VOID BRIDGE1_NAME(xvm_get_host_err) 
(	
	TAEINT *vmid,		/* in:  id of a vm object */
	TAEINT *vmcode,		/* out:  host-dependent error code */
	TAEINT *status		/* out:  SUCCESS or error code */

	)
	{

		BRIDGE2_NAME(xvm_get_host_err) (vmid, vmcode, status);

		return;
	}



/*
   *	xvm_dyn_tutor -- initiate dynamic tutor
   */

/* 	FUNCTION VOID BRIDGE1_NAME(xvm_dyn_tutor)  */
/* 				(vmid, pdfnam, pdfset, status, pdfnaml) */
	
/* 	TAEINT *vmid;		/\* in: id of a vm object *\/ */
/* 	TEXT   *pdfnam;		/\* in: pdf file spec for tae monitor *\/ */
/* 	TAEINT *pdfset;		/\* in:  mode *\/ */
/* 	TAEINT *status;		/\* out: SUCCESS or error code *\/ */
/* 	STRLEN pdfnaml; */

/* 	{ */
/* 		FORSTR pdfnamd; */

/* 		pdfnamd.length = GETLEN(pdfnaml); */
/* 		pdfnamd.pointer = pdfnam; */

/* 		BRIDGE2_NAME(xvidyt) (vmid, &pdfnamd, pdfset, status); */

/* 		return; */

/* 	} */


/*
   *	xvm_init_format -- initialize formatting variables
   */

FUNCTION VOID BRIDGE1_NAME(xvm_init_format) 
(	
	TAEINT *vmid,		/* in:  id of a vm object */
	TAEINT *status		/* out: SUCCESS or error code */ 

	)
	{
		BRIDGE2_NAME(xvm_init_format) (vmid, status);

		return;
	}



/*
   *	xvm_new -- allocate a new xv parameter object
   */

FUNCTION VOID BRIDGE1_NAME(xvm_new) 
(
	TAEINT *vmid,		/* out: id of a vm object */
	TAEINT *mode,		/* in: how function should handle errors */
	TAEINT *status		/* out: SUCCESS or error code */

 )
	{
		BRIDGE2_NAME(xvm_new) (vmid, mode, status);

		return;
	}


/*
*	xvm_init -- initialize and open the current standard output device
*/

FUNCTION VOID BRIDGE1_NAME(xvm_init) 
(
	TAEINT *vmid,		/* in: id of a vm object */
	TAEINT *lun,		/* in: logical unit number */
	TAEINT *status		/* out: SUCCESS or error code */

 )
	{
		BRIDGE2_NAME(xvm_init) (vmid, lun, status);

		return;
	}




/*
*	xvm_read_from_disk -- read from disk
*/


FUNCTION VOID BRIDGE1_NAME(xvm_read_from_disk) 
(	
	TAEINT *vmid,		/* in: id of a vm object */
	TEXT   *fspec,		/* in: host file specification of file */
	TAEINT *status,		/* out: SUCCESS or error code */
	STRLEN fspecl

	)
	{
		FORSTR fspecd;

		fspecd.length = GETLEN(fspecl);
		fspecd.pointer = fspec;
		
		BRIDGE2_NAME(xvm_read_from_disk) (vmid, &fspecd, status);

		return;
	}

/*
*	xvm_read_from_tm -- recieve variables from tm
*/

FUNCTION VOID BRIDGE1_NAME(xvm_read_from_tm) 
(
	TAEINT *vmid,		/* in: id of a vm object */
	TAEINT *status		/* out: SUCCESS or error code */

 )
	{
		BRIDGE2_NAME(xvm_read_from_tm) (vmid, status);

		return;
	}


/*
*	xvm_set_intg -- set values for integer variables
*/

FUNCTION VOID BRIDGE1_NAME(xvm_set_intg) 
(	
	TAEINT *vmid,	/* in: id of a vm object */
	TEXT   *name,	/* in: name of the tcl integer variable to be set */
	TAEINT *count,	/* in: multiplicity of the variable */
	TAEINT intg[],	/* in: an array of integer values for the variables */
	TAEINT *mode,	/* in: variable being added or being modified */
	TAEINT *status,	/* out: SUCCESS or error code */
	STRLEN namel

	)
	{
		FORSTR named;

		named.length = GETLEN(namel);
		named.pointer = name;

		BRIDGE2_NAME(xvm_set_intg) (vmid, &named, count, intg, mode, status);

		return;
	}

/*
*	xvm_set_valid_intg -- set integer valids
*/

FUNCTION VOID BRIDGE1_NAME(xvm_set_valid_intg) 
(	
	TAEINT *vmid,		/* in: id of a vm object */
	TEXT   *name,		/* in: name of tcl integer variable */
	TAEINT *count,		/* in: multiplicity of the valids */
	TAEINT ilo[],		/* in: array of integers for low values */
	TAEINT ihi[],		/* in: array of integers for high values */
	TAEINT *status,		/* out: SUCCESS or error code */
	STRLEN namel		

	)
	{
		FORSTR named;

		named.length = GETLEN(namel);
		named.pointer = name;

		BRIDGE2_NAME(xvm_set_valid_intg) (vmid, &named, count, ilo, ihi, status);

		return;
	}



/*
*	xvm_set_max -- set maximum vector count of a variable
*/

FUNCTION VOID BRIDGE1_NAME(xvm_set_max) 
(	
	TAEINT *vmid,		/* in: id of a vm object */
	TEXT   *name,		/* in: name of the variable */
	TAEINT *maxcnt,		/* in: max vector count of variable */
	TAEINT *status,		/* out: SUCCESS or error code */
	STRLEN namel

	)
	{
		FORSTR named;

		named.length = GETLEN(namel);
		named.pointer = name;

		BRIDGE2_NAME(xvm_set_max) (vmid, &named, maxcnt, status);

		return;
	}



/*
*	xvm_set_min -- set minimum vector count of a variable
*/

FUNCTION VOID BRIDGE1_NAME(xvm_set_min) 
(	
	TAEINT *vmid,		/* in: id of a vm object */
	TEXT   *name,		/* in: name of the variable */
	TAEINT *mincnt,		/* in: min vector count of variable */
	TAEINT *status,		/* out: SUCCESS or error code */
	STRLEN namel

	)
	{
		FORSTR named;

		named.length = GETLEN(namel);
		named.pointer = name;

		BRIDGE2_NAME(xvm_set_min) (vmid, &named, mincnt, status);

		return;
	}



/*
*	xvm_set_next_menu -- set next menu to be executed
*/

FUNCTION VOID BRIDGE1_NAME(xvm_set_next_menu) 
(	
	TAEINT *vmid,		/* in: id of a vm object */
	TEXT   *name,		/* in: name of the variable */
	TAEINT *status,		/* out: SUCCESS or error code */
	STRLEN namel

	)
	{
		FORSTR named;

		named.length = GETLEN(namel);
		named.pointer = name;

		BRIDGE2_NAME(xvm_set_next_menu) (vmid, &named, status);

		return;
	}




/*
*	xvm_set_parm_page -- set parameter (variable) page indicator
*/

FUNCTION VOID BRIDGE1_NAME(xvm_set_parm_page) 
(	
	TAEINT *vmid,		/* in: id of a vm object */
	TEXT   *name,		/* in: name of the variable */
	BOOL   *flag,		/* in: sets page indicator */
	TAEINT *status,		/* out: SUCCESS or error code */
	STRLEN namel

	)
	{
		FORSTR named;

		named.length = GETLEN(namel);
		named.pointer = name;

		BRIDGE2_NAME(xvm_set_parm_page) (vmid, &named, flag, status);

		return;
	}



/*
*	xvm_set_dble -- set values for double precision variables
*/

FUNCTION VOID BRIDGE1_NAME(xvm_set_dble) 
(	
	TAEINT *vmid,		/* in: id of a vm object */
	TEXT   *name,		/* in: name of the variable */
	TAEINT *count,		/* in: number of real values in the real array */
	double dble[],		/* in: array of double precision values for the variable */
	TAEINT *mode,		/* in: variable being added or being modified */	
	TAEINT *status,		/* out: SUCCESS or error code */
	STRLEN namel

	)
	{
		FORSTR named;

		named.length = GETLEN(namel);
		named.pointer = name;

		BRIDGE2_NAME(xvm_set_dble) (vmid, &named, count, dble, mode, status);
		return;
	}

/*
*	xvm_set_real -- set values for real  variables
*/

FUNCTION VOID BRIDGE1_NAME(xvm_set_real) 
(	
	TAEINT *vmid,		/* in: id of a vm object */
	TEXT   *name,		/* in: name of the variable */
	TAEINT *count,		/* in: number of real values in the real array */
	float  real[],		/* in: array of real values for the variable */
	TAEINT *mode,		/* in: variable being added or being modified */	
	TAEINT *status,		/* out: SUCCESS or error code */
	STRLEN namel

	)
	{
		FORSTR named;

		named.length = GETLEN(namel);
		named.pointer = name;

		BRIDGE2_NAME(xvm_set_real) 
			(vmid, &named, count, real, mode, status);

		return;
	}

/*
*	xvm_set_valid_dble -- set double precision valids
*/

FUNCTION VOID BRIDGE1_NAME(xvm_set_valid_dble) 
(	
	TAEINT *vmid,		/* in: id of a vm object */
	TEXT   *name,		/* in: name of the tcl integer variable. */
	TAEINT *count,		/* in: multiplicity of the valids */
	double dlo[],		/* in: array of doubles for the low values */
	double dhi[],		/* in: array of doubles for the high values */
	TAEINT *status,		/* out: SUCCESS or error code */
	STRLEN namel

	)
	{
		FORSTR named;

		named.length = GETLEN(namel);
		named.pointer = name;

		BRIDGE2_NAME(xvm_set_valid_dble) 
			(vmid, &named, count, dlo, dhi, status);

		return;
	}

/*
*	xvm_set_valid_real -- set real valids
*/

FUNCTION VOID BRIDGE1_NAME(xvm_set_valid_real) 
(	
	TAEINT *vmid,		/* in: id of a vm object */
	TEXT   *name,		/* in: name of the tcl integer variable. */
	TAEINT *count,		/* in: multiplicity of the valids */
	float  rlo[],		/* in: array of reals for the low values */
	float  rhi[],		/* in: array of reals for the high values */
	TAEINT *status,		/* out: SUCCESS or error code */
	STRLEN  namel

	)
	{
		FORSTR named;

		named.length = GETLEN(namel);
		named.pointer = name;

		BRIDGE2_NAME(xvm_set_valid_real) 
			(vmid, &named, count, rlo, rhi, status);

		return;
	}

/*
*	xvm_set_string_length -- set maximum string length of a string 
*	or keyword variable
*/

FUNCTION VOID BRIDGE1_NAME(xvm_set_string_length) 
(	
	TAEINT *vmid,		/* in: id of a vm object */
	TEXT   *name,		/* in: name of tcl variable or keyword */
	TAEINT *strlen,		/* in: max length of a string for the variable */
	TAEINT *status,		/* out: SUCCESS or error code */
	STRLEN namel

	)
	{
		FORSTR named;

		named.length = GETLEN(namel);
		named.pointer = name;

		BRIDGE2_NAME(xvm_set_string_length) 
			(vmid, &named, strlen, status);

		return;
	}

/*
*	xvm_set_string -- set values for a string or keyword variable
*/

FUNCTION VOID BRIDGE1_NAME(xvm_set_string) 
(
	TAEINT *vmid,		/* in: id of a vm object */
	TEXT   *name,		/* in: name of tcl string variable */
	TAEINT *count,		/* in: multiplicity of the variable */
	TEXT   *string,		/* in: array of strings F77 fmt */
	TAEINT *mode,		/* in: variable being added or being modified */
	TAEINT *status,		/* out: SUCCESS or error code */
	STRLEN namel,
	STRLEN stringl

 )
	{
		FORSTR named;
		FORSTR stringd;

		stringd.length = GETLEN (stringl);
		stringd.pointer = string;

		named.length = GETLEN(namel);
		named.pointer = name;

		BRIDGE2_NAME (xvm_set_string) 
				(vmid, &named, count, &stringd, mode, status);

		return;
	}


/*
*	xvm_set_valid_string -- set string valids
*/

FUNCTION VOID BRIDGE1_NAME(xvm_set_valid_string) 
(
	TAEINT *svid,		/* in: id of a vm object */
	TEXT   *name,		/* in: name of the variable */
	TAEINT *count,		/* in: multiplicity of the valids */
	TEXT   *string,		/* in: array of strings for the variable */
	TAEINT *status,		/* out: SUCCESS or error code */
	STRLEN namel,
	STRLEN stringl
 )
	{
		FORSTR named;
		FORSTR stringd;

		stringd.length = GETLEN (stringl);
		stringd.pointer = string;
		named.length = GETLEN(namel);
		named.pointer = name;

		BRIDGE2_NAME(xvm_set_valid_string) 
			(svid, &named, count, &stringd, status);

		return;
	}

/*
*	xvm_set_tcl_var -- set TCL variable
*/

FUNCTION VOID BRIDGE1_NAME(xvm_set_tcl_var) 
(	
	TAEINT *vmid,		/* in: id of a vm object */
	TAEINT *status		/* out: SUCCESS or error code */

	)
	{
		BRIDGE2_NAME(xvm_set_tcl_var) (vmid, status);

		return;
	}


 /*
*	xvm_write_to_disk -- write the vm object to a disk file
*/

FUNCTION VOID BRIDGE1_NAME(xvm_write_to_disk) 
(
	TAEINT *vmid,		/* in: id of a vm object */
	TEXT   *fspec,		/* host file specification of file to be created */
	TAEINT *status,		/* out: SUCCESS or error code */
	STRLEN fspecl

 )
	{
		FORSTR fspecd;

		fspecd.length = GETLEN(fspecl);
		fspecd.pointer = fspec;

		BRIDGE2_NAME(xvm_write_to_disk) (vmid, &fspecd, status);

		return;
	}
