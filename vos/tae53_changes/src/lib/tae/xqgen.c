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



/* TDM CHECKOUT FILE_TIME=11-OCT-1983 17:48 DUA0:[TAEV1.OLB]XQGEN.C;4 */
/*
 *	TAE version 1 XQ bridges for manipulation of already built
 *	parameter blocks only.  The other XQ routines are in other source files.
 *	FORTRAN-callable.
 *
 *	CHANGE LOG:
 *	13-sep-83	New XQSETM to set mode ...palm
 *	11-oct-83	Fix unix compilation errors...palm
 *	25-oct-83	set v_file bit in XQFILE function...dm
 *	31-oct-83	Fix p_fvar declaration...palm
 *	27-jun-84	re-write xqstr to avoid enormous automatic...palm
 *	09-nov-84	PR 854: new xqdble...palm
 *      16-aug-88       added BRIDGE_NAME2 to xqstr in xqfile...tpl
 *
 */

#include	"taeconf.inp"	/* TAE configuration (REQUIRED)		*/
#include	"parblk.inc"	/* parameter block definitions		*/
#include	"forstr.inp"	/* fortran-77 string struct		*/
#include "taeintproto.h"


FUNCTION VOID BRIDGE2_NAME(xqstr) 
(
    TAEINT	block[],	/* in/out: v-block			*/
    FORSTR	*name,		/* in: parameter name in FOR-77 format	*/
    TAEINT	*count,		/* in: number of strings		*/
    FORSTR	*string,	/* in: array of strings, FOR-77 format	*/
    TAEINT	*mode,		/* in: P_UPDAT or P_ADD			*/
    TAEINT	*status	/* out: SUCCESS or error code		*/

 );



/*
 *	xqfile.   Put file string values into v-block.
 */

FUNCTION VOID BRIDGE2_NAME(xqfile) 
(
    TAEINT	block[],	/* in/out: v-block			*/
    FORSTR	*name,		/* in: parameter name in FOR-77 format	*/
    TAEINT	*count,		/* in: number of strings		*/
    FORSTR	*file,		/* in: array of strings, FOR-77 format	*/
    TAEINT	*mode,		/* in: P_UPDAT or P_ADD			*/
    TAEINT	*status	/* out: SUCCESS or error code		*/

 )
    {
    CODE	ret_code;
    struct  	VARIABLE	*v;
    TEXT	c_name[STRINGSIZ+1];

    BRIDGE2_NAME(xqstr) (block, name, count, file, mode, &ret_code);	
    if (ret_code == SUCCESS)
	{
	s_for2c(name, c_name, 0);	/* convert to c string		*/
	s_strip(c_name);
	v = p_fvar((struct PARBLK *) block, c_name);
				/* get pointer to variable	*/
        (*v).v_file = TRUE;		/* set file bit			*/
    	}
    *status = ret_code;
    return;
    }

/*
 *	xqini.	Initialize V-block.
 */

FUNCTION VOID BRIDGE2_NAME(xqini) 
(
    TAEINT	block[],	/* out: initialized block		*/
    TAEINT	*dim,		/* in: dimension of block		*/
    TAEINT	*mode		/* in: P_ABORT or P_CONT		*/

 )
    {
    struct PARBLK	*p;
    COUNT		bytes;

    p = (struct PARBLK *) block;

    /* the size of the pool is the total size minus header stuff	*/
    bytes = (*dim) * sizeof(TAEINT) - ((GENPTR)(*p).pool - (GENPTR)p);

    q_init(p, bytes, *mode);
    return;
    }

/* 
 *	xqintg.   Put integer value(s) into a vblock.
 */

FUNCTION VOID BRIDGE2_NAME(xqintg) 
(
    TAEINT	block[],	/* in/out: vblock			*/
    FORSTR	*name,		/* in: parameter name in FOR-77 format	*/
    TAEINT	*count,		/* in: number of integers		*/
    TAEINT	intg[],		/* in: integer values			*/
    TAEINT	*mode,		/* in: P_ADD or P_UPDATE		*/
    TAEINT	*status	/* out: SUCCESS or error code		*/

 )
    {
    TEXT	cname[STRINGSIZ+1];
    CODE	code;

    s_for2c(name, cname,0);
    s_strip(cname);
    code = q_intg((struct PARBLK *)block, cname, *count, intg, *mode);
    *status = code;
    return;
    }

/* 
 *	xqreal.  Put real value(s) into a vblock.
 */

FUNCTION VOID BRIDGE2_NAME(xqreal) 
(
    TAEINT	block[],	/* in/out: vblock			*/
    FORSTR	*name,		/* in: parameter name in FOR-77 format	*/
    TAEINT	*count,		/* in: number of integers		*/
    float	real[],		/* in: REAL values			*/
    TAEINT	*mode,		/* in: P_ADD or P_UPDATE		*/
    TAEINT	*status	/* out: XSUCC or error code		*/

 )
    {
    TEXT	cname[STRINGSIZ+1];
    CODE	code;
    TAEFLOAT	r[MAXVAL];	
    COUNT	i;

    for (i=0; i < *count; i++)
        r[i] = real[i];		/* convert from FLOAT to TAEFLOAT	*/
    s_for2c(name, cname, 0);	/* convert string to c format		*/
    s_strip(cname);
    code = q_real((struct PARBLK *)block, cname, *count, r, *mode);
    *status = code;
    return;
    }

/* 
 *	xqdble.  Put double precision real value(s) into a vblock.
 */

FUNCTION VOID BRIDGE2_NAME(xqdble) 
(
    TAEINT	block[],	/* in/out: vblock			*/
    FORSTR	*name,		/* in: parameter name in FOR-77 format	*/
    TAEINT	*count,		/* in: number of integers		*/
    TAEFLOAT 	dble[],		/* in: DP REAL values			*/
    TAEINT	*mode,		/* in: P_ADD or P_UPDATE		*/
    TAEINT	*status	/* out: XSUCC or error code		*/

 )
    {
    TEXT	cname[STRINGSIZ+1];
    CODE	code;

    s_for2c(name, cname, 0);	/* convert string to c format		*/
    s_strip(cname);
    code = q_real((struct PARBLK *)block, cname, *count, dble, *mode);
    *status = code;
    return;
    }

/*	XQSETM.   Set mode in parameter block.
 */

FUNCTION VOID BRIDGE2_NAME(xqsetm) 
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
 *	xqstr.   Put string values into v-block.
 */

FUNCTION VOID BRIDGE2_NAME(xqstr) 
(
    TAEINT	block[],	/* in/out: v-block			*/
    FORSTR	*name,		/* in: parameter name in FOR-77 format	*/
    TAEINT	*count,		/* in: number of strings		*/
    FORSTR	*string,	/* in: array of strings, FOR-77 format	*/
    TAEINT	*mode,		/* in: P_UPDAT or P_ADD			*/
    TAEINT	*status	/* out: SUCCESS or error code		*/

 )
    {
    IMPORT TEXT	pm_room[], pk_room[];	/* error message and key	*/
    IMPORT TEXT	pm_size[], pk_size[];	/* error message and key	*/

    TEXT	cname[STRINGSIZ+1];
    struct VARIABLE	*v;		/* pointer into PARBLK		*/
    CODE	code;
    struct  PARBLK *p;
    COUNT	i;
    TEXT	cstring[STRINGSIZ+1];	/* local c string		*/

    s_for2c(name, cname, 0);		/* convert from FOR-77 to C format */
    s_strip(cname);			/* strip trailing blanks	   */
    p = (struct PARBLK *) block;
    code = q_prep (p, cname, *count, V_STRING, *mode, &v, 0);
    if (code != SUCCESS)
	{
	*status = code;
	return;
	}
    for (i=0; i < *count; i++)
        {	
        code = s_for2c(string, cstring, i);	/* convert ith string to c format    */
	if (code != SUCCESS)
	    {
	      x_error ((*p).mode, pm_size, pk_size, (uintptr_t) cname, 0, 0);
	    *status = P_OVER;
	    return;
    	    }
	s_strip(cstring);		/* strip trailing blanks	     */
	SVAL(*v,i) = q_save ((*p).pool, cstring);
	if (SVAL(*v,i) == NULL)
	    {
	      x_error ((*p).mode, pm_room, pk_room, (uintptr_t) cname, 0, 0);
	    *status = P_NOROOM;
	    return;
	    }
	}
    *status = SUCCESS;
    return;
    }
