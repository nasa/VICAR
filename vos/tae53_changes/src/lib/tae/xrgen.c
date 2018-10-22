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



/* TJH CHECKOUT FILE_TIME= 9-NOV-1984 14:06 DUA1:[TAEV1.OLB]XRGEN.C;1 */
/* TDM CHECKOUT FILE_TIME=25-OCT-1983 20:36 DUA0:[TAEV1.OLB]XRGEN.C;12 */
/* TDM CHECKOUT FILE_TIME=15-SEP-1983 21:21 DUA0:[TAEV1.OLB]XRGEN.C;10 */
/*
 *	XR (native) bridge excluding the XRF routines.
 *	The routines in this source are only those associated directly
 *	with retrieving parameters from an already built PARBLK.
 *	The routines for building a PARBLK are in other files.
 *
 *	CHANGE LOG:
 *	08-sep-83	Check for v_file in XRFILE...palm
 *	13-sep-83	New XRSETM to set mode...palm
 *	13-sep-83	Fix declaration of filemode passed to xzhost...palm
 *	15-sep-83	XRFILE moved to XRFILE.C to keep non-file users
 *			from bringing in catman...palm
 *	24-oct-83	New xr calling sequences...dm
 *	25-oct-83	Change cvtlog() to c_cvtlog...dm	
 *	18-nov-83	Add P_BADNAME error check...dm
 *	14-feb-84	Fix 68000 portability on p_attr call...palm
 *	04-may-84	Move IMPORT into function...lim
 *	09-nov-84	New xrdble (PR 854)...palm
 *	28-may-85	Make bridge routines call p_find (PR737) ...joh
 *
 */

#include "taeconf.inp"
#include "parblk.inc"		/* get P_BADNAME, etc.			*/
#include "forstr.inp"		/* FORTRAN-77 string definition		*/
#include "symtab.inc"
#include "taeintproto.h"





/*
 *	XRATTR. Get parameter/variable attributes.
 */

FUNCTION VOID BRIDGE2_NAME(xrattr) 
(
    TAEINT		*block,		/* in: parameter block		*/
    FORSTR		*name,		/* in: parameter name		*/
    TAEINT		*type,		/* out: parameter type		*/
    TAEINT		*n,		/* out: number of components	*/
    TAELOG		*defalt,	/* out: .TRUE. if defaulted	*/
    TAEINT		*access,	/* out: access mode if a file	*/
    TAEINT		*status	/* out: status code		*/

 )
    {
    struct PARBLK *parblk;
    struct VARIABLE *v;
    CODE	code;
    TEXT	c_name[STRINGSIZ+1];	/* name in C string format	*/

    parblk = (struct PARBLK *) block;
    code = s_for2c(name, c_name, 0);		/* convert name to C string	*/
    if (code != SUCCESS) goto p__bnerr;
    s_strip(c_name);			/* remove trailing blanks	*/

    v = p_find(parblk,c_name);
    if (v == NULL) goto p__bnerr;
    *defalt = (*v).v_default;		/* default in var is already TRUE/FALSE */
    *n = (*v).v_count;
    *type = (*v).v_file ? P_FILE : (*v).v_type;       /* file or other types  */
    *access = (*v).v_file ? (*v).v_filemode : P_NONE;  /* none if not file */
    *status = SUCCESS;
    return;

p__bnerr:
    *status = P_BADNAME;
    return;
    }

/*
 *	XRINTG.  Get integer parameters/variables.
 */
FUNCTION VOID BRIDGE2_NAME(xrintg) 
(
    TAEINT		*block,		/* in: parameter block		*/
    FORSTR		*name,		/* in: parameter name		*/
    TAEINT		*dimen,		/* in: max # of values(dim of intg)	*/
    int			intg[],		/* out: array with intger var	*/
    TAEINT		*n,		/* out: integer value count	*/
    TAEINT		*status	/* out: status code		*/

 )
    {
    IMPORT TEXT		pm_dim[],pk_dim[],pm_type[],pk_type[];
    struct 	PARBLK 	*parblk;
    struct	VARIABLE *v;
    TEXT		c_name[STRINGSIZ+1];	/* name in C string format	*/
    COUNT		i;


    parblk = (struct PARBLK *) block;
    s_for2c(name, c_name, 0);			/* convert name to C string	*/
    s_strip(c_name);				/* remove trailing blanks	*/
    *n = 0;					/* caution in case error	*/ 

    v = p_find(parblk, c_name);
    if (v == NULL) goto p__bnerr;
    if ((*v).v_type != V_INTEGER) goto p__bterr;
    if ((*v).v_count > *dimen) goto p__bcerr;
    *n = (*v).v_count;
    for (i = 0; i < (*v).v_count; i++)
	intg[i] = IVAL(*v, i);
    *status = SUCCESS;
    return;

p__bnerr:
    *status = P_BADNAME;
    return;

p__bterr:
    *status = P_BADTYPE;
    x_error((*parblk).mode, pm_type, pk_type, (uintptr_t) c_name, 0, 0);
    return;

p__bcerr:
    *status = P_BADCOUNT;
    x_error((*parblk).mode, pm_dim, pk_dim, (uintptr_t) c_name, 0, 0);
    return;
    }

/*
 *	XRREAL.  Get real parameters/variables.
 */
FUNCTION VOID BRIDGE2_NAME(xrreal) 
(
    TAEINT		*block,		/* in: parameter block		*/
    FORSTR		*name,		/* in: parameter name		*/
    TAEINT		*dimen,		/* in: max # of values(dim of intg)	*/
    float		real[],		/* out: array with real var	*/
    TAEINT		*n,		/* out: real value count	*/
    TAEINT		*status	/* out: status code		*/

 )
    {
    IMPORT TEXT		pm_dim[],pk_dim[],pm_type[],pk_type[];
    struct 	PARBLK 	*parblk;
    struct      VARIABLE *v;
    TEXT		c_name[STRINGSIZ+1];	/* name in C string format */
    COUNT		i;


    parblk = (struct PARBLK *) block;
    s_for2c(name, c_name, 0);			/* convert name to C string	*/
    s_strip(c_name);				/* remove trailing blanks	*/
    *n = 0;					/* caution in case error	*/ 

    v = p_find(parblk, c_name);
    if (v == NULL) goto p__bnerr;
    if ((*v).v_type != V_REAL) goto p__bterr;
    if ((*v).v_count > *dimen) goto p__bcerr;
    *n = (*v).v_count;
    for (i = 0; i < (*v).v_count; i++)
	real[i] = RVAL(*v, i);
    *status = SUCCESS;
    return;

p__bnerr:
    *status = P_BADNAME;
    return;

p__bterr:
    *status = P_BADTYPE;
    x_error((*parblk).mode, pm_type, pk_type, (uintptr_t) c_name,0,0);
    return;

p__bcerr:
    *status = P_BADCOUNT;
    x_error((*parblk).mode, pm_dim, pk_dim, (uintptr_t) c_name,0,0);
    return;
    }

/*
 *	XRDBLE.  Get double precision real parameters/variables.
 */
FUNCTION VOID BRIDGE2_NAME(xrdble) 
(
    TAEINT		*block,		/* in: parameter block		*/
    FORSTR		*name,		/* in: parameter name		*/
    TAEINT		*dimen,		/* in: max # of values(dim of intg)	*/
    TAEFLOAT		dble[],		/* out: array with real var	*/
    TAEINT		*n,		/* out: real value count	*/
    TAEINT		*status	/* out: status code		*/

 )
    {
    IMPORT TEXT		pm_dim[],pk_dim[],pm_type[],pk_type[];
    struct 	PARBLK 	*parblk;
    struct      VARIABLE *v;
    TEXT		c_name[STRINGSIZ+1];	/* name in C string format */
    COUNT		i; 


    parblk = (struct PARBLK *) block;
    s_for2c(name, c_name, 0);			/* convert name to C string	*/
    s_strip(c_name);				/* remove trailing blanks	*/
    *n = 0;					/* caution in case error	*/ 

    v = p_find(parblk, c_name);
    if (v == NULL) goto p__bnerr;
    if ((*v).v_type != V_REAL) goto p__bterr;
    if ((*v).v_count > *dimen) goto p__bcerr;
    *n = (*v).v_count;
    for (i = 0; i < (*v).v_count; i++)
	dble[i] = RVAL(*v, i);
    *status = SUCCESS;
    return;

p__bnerr:
    *status = P_BADNAME;
    return;

p__bterr:
    *status = P_BADTYPE;
    x_error((*parblk).mode, pm_type, pk_type, (uintptr_t) c_name, 0, 0);
    return;

p__bcerr:
    *status = P_BADCOUNT;
    x_error((*parblk).mode, pm_dim, pk_dim, (uintptr_t) c_name, 0, 0);
    return;
    }

/*
 * 	XRHERR. Get host-dependent error code.
 */

FUNCTION  VOID  BRIDGE2_NAME(xrherr) 
(
    TAEINT		*block,			/* in: parameter block */
    TAEINT		*hcode			/* out: host error code	*/

 )
    {
    p_herr( (struct PARBLK *)block, hcode);	/* get error code  	*/
    return;
    }


/*	XRSETM.   Set mode in parameter block.
 */

FUNCTION VOID BRIDGE2_NAME(xrsetm) 
(
    TAEINT	block[],			/* in/out: parameter block */
    TAEINT	*mode				/* in: xcont or xabort	   */

 )
    {
    struct PARBLK *p;

    p = (struct PARBLK *) block;
    (*p).mode = *mode;
    return;
    }

/*
 *	XRSTR.  Fetch string parameter.
 */

FUNCTION VOID BRIDGE2_NAME(xrstr) 
(
    TAEINT		*block,		/* in: parameter block		*/
    FORSTR		*name,		/* in: parameter name		*/
    TAEINT		*dimen,		/* in: dimension of string	*/
    FORSTR		*string,	/* out: FOR-77 string(s)	*/
    TAEINT		length[],	/* out: length of each string	*/
    TAEINT		*n,		/* out: number of strings	*/
    TAEINT		*status	/* out: status code		*/

 )
    {
    IMPORT TEXT		pm_dim[],pk_dim[],pm_type[],pk_type[];
    struct PARBLK *parblk;
    struct VARIABLE *v;
    TEXT	**s;			/* ptr to value vector in block	*/
    TEXT	c_name[STRINGSIZ+1];	/* name in C string format	*/
    COUNT	i;
    CODE	code;


    parblk = (struct PARBLK *) block;
    s_for2c(name, c_name, 0);			/* convert name to C string	*/
    s_strip(c_name);				/* remove trailing blanks	*/
    *n = 0;					/* caution in case error	*/ 

    v = p_find(parblk, c_name);
    if (v == NULL) goto p__bnerr;
    if ((*v).v_type != V_STRING) goto p__bterr; /* error if not string    */
    s = (TEXT **) (*v).v_cvp;			/* value pointer          */
    *status = SUCCESS;
    *n = (*v).v_count;				/* number of strings 		*/
    for (i=0; i < (*v).v_count && i < *dimen; i++)
	{
        length[i] = s_length(s[i]);		/* pass this length to caller	*/
        code = s_c2for(s[i], string, i);	/* copy string to caller	*/
        if (code != SUCCESS)
       	    {
	    x_error((*parblk).mode, "String size overflows buffer", 
		    "TAE-OVER", 0, 0, 0);
	    *status = P_OVER;
	    return;
	    }
        }
     if ((*v).v_count > *dimen)
        {
        x_error((*parblk).mode, pm_dim, pk_dim,(uintptr_t) c_name, 0, 0);
        *status = P_BADCOUNT;		/* bad count		*/
        }
    return;

p__bnerr:
    *status = P_BADNAME;
    return;

p__bterr:
    *status = P_BADTYPE;
    x_error((*parblk).mode, pm_type, pk_type, (uintptr_t) c_name, 0, 0);
    return;
    }
