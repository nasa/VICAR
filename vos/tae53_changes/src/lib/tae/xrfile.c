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



/* TJH CHECKOUT FILE_TIME= 4-MAY-1984 16:06 DUA1:[TAEV1.OLB]XRFILE.C;1 */
/* TDM CHECKOUT FILE_TIME=15-SEP-1983 21:29 DUA0:[TAEV1.OLB]XRFILE.C;2 */
/*
 *	XRFILE.   Obtain file parameter.
 *
 *	CHANGE LOG:
 *	
 *	24-oct-83	New XR calling sequences...dm
 *	04-may-84	Move IMPORT into function...lim
 *	30-may-85	Call p_find instead of p_string...joh
 *	03-apr-88	Apollo conversion: macro for bridge routine names...ljn
 *
 */

#include "taeconf.inp"
#include "parblk.inc"		/* get P_BADNAME, etc.			*/
#include "forstr.inp"		/* FORTRAN-77 string definition		*/
#include "symtab.inc"
#include "taeintproto.h"





/*
 *	XRFILE.  Fetch file parameter.
 */

extern TEXT pm_trans[];	/* error message for file-spec error	*/
extern TEXT pk_trans[];	/* ...and key				*/
extern TEXT pm_type[];		
extern TEXT pk_type[];    

FUNCTION VOID BRIDGE2_NAME(xrfile) 
(
    TAEINT		*block,		/* in: parameter block		*/
    FORSTR		*name,		/* in: parameter name		*/
    TAEINT		*dimen,		/* in: dimension of string	*/
    FORSTR		*tae_file,	/* out: FOR-77 tae file string(s)	*/
    TAEINT		tae_length[],	/* out: length of each tae file string	*/
    FORSTR		*host_file,	/* out: FOR-77 host file string(s)	*/
    TAEINT		host_length[],	/* out: length of each host file string	*/
    TAEINT		*n,		/* out: number of strings	*/
    TAEINT		*status	/* out: status code		*/

 )
    {
    struct PARBLK 	*parblk;
    TEXT		**s;			/* ptr to value vector in block	*/
    TEXT		c_name[STRINGSIZ+1];	/* name in C string format	*/
    COUNT		i;
    struct VARIABLE	*v;		/* variable in parm block		*/
    TEXT		temp[FSPECSIZ+1];
    TAEINT 		filemode;
    CODE		code;

    parblk = (struct PARBLK *) block;
    s_for2c(name, c_name, 0);			/* convert name to C string	*/
    s_strip(c_name);				/* remove trailing blanks	*/
    *n = 0;					/* caution in case error	*/ 

    v = p_find(parblk, c_name);			/* get string			*/
    if (v == NULL) goto p__bnerr;		/* check for name error		*/
    if ((*v).v_type != V_STRING) goto p__bterr; /* check for type error		*/
    s = (TEXT **) (*v).v_cvp;			/* value pointer		*/

    if (!(*v).v_file) goto p__bterr;
    filemode = (*v).v_filemode;
    *status = SUCCESS;				
    *n = (*v).v_count;				/* number of strings		*/
    for (i=0; i < (*v).v_count && i < *dimen; i++) /* get all tae & host files	*/
	{
	tae_length[i] = s_length(s[i]);	/* pass this length to caller	*/
	code = s_c2for(s[i], tae_file, i);	/* copy tae file to caller*/
        if (code != SUCCESS)
	    {
	    x_error((*parblk).mode, "File name longer than buffer size.",
		    "TAE-OVER", 0, 0, 0);
   	    *status = P_OVER;
	    return;
	    }
	xzhost(s[i], temp, &filemode, status);	/* get host spec	*/
	if (*status != SUCCESS)
	    {
	      x_error((*parblk).mode, (TEXT *) pm_trans, (TEXT *) pk_trans, 
		      (uintptr_t) s[i], 0, 0);
	    *status = P_FAIL;			/* failed to translate	*/
	    return;
	    }
	host_length[i] = s_length(temp);	/* pass this length to caller	*/
	s_c2for(temp, host_file, i);	/* copy host file to caller	*/
	}
    if ((*v).v_count > *dimen)
	*status = P_BADCOUNT;		/* bad count			*/
    return;

p__bnerr:
    *status = P_BADNAME;
    return;

p__bterr:
    x_error((*parblk).mode, (TEXT *) pm_type, (TEXT *) pk_type, 
	    (uintptr_t) (*v).v_name, 0, 0);
    *status = P_BADTYPE;
    return;
    }

