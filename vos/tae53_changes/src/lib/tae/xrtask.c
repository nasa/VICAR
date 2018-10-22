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



/* TDM CHECKOUT FILE_TIME= 9-SEP-1986 14:15 DUA1:[TAEV1.OLB]XRTASK.C;2 */
/* TDM CHECKOUT FILE_TIME= 4-MAY-1984 19:58 DUA0:[TAEV1.OLB]XRTASK.C;7 */
/*
 *	XR (native) bridge excluding the XRF routines and excluding
 *	all XR routines not directly associated with intertask communication.
 *
 *	CHANGE LOG:
 *	23-sep-83	Installation exits...palm
 *	11-nov-83	Remove erroneous conditional on savesess...palm
 *	22-dec-83	New xrstr calling sequence...dm	
 *	15-feb-84	Use p_fvar not xrstr (cleaner); remove first_time
 *			flag, using parblk.msgtyp...palm
 *	04-may-84	VALUE_x to xVAL ... ces
 *	24-aug-84	Move session id retrieval to p_inim...dm
 *	22-jul-85	Change $sesssion variable name and other updates 
 *			to fix UNIX compilation errors...dm
 *	14-sep-86	Set application program type...dm
 *	25-mar-88	Fix typo VAX_VM -> VAX_VMS
 *	03-apr-88	Apollo conversion: macro for bridge routine names
 *			and for name of Fortran common...ljn
 */

#include "taeconf.inp"
#include "parblk.inc"		/* get P_BADNAME, etc.			*/
#include "symtab.inc"
#include "forstr.inp"
#include "taeintproto.h"





/*
 *	XRINIM.  Initialize BLOCK from Terminal Monitor.
 */

/*	The xrsess definition is VMS-dependent.  On VMS, C GLOBALS are psects
 *	available to FORTRAN as COMMON.
 */
#ifdef VAX_VMS
#define  XRSESS	 xrsess
#else			/* for UNIX systems		*/
#define  XRSESS  FOR_NAME(xrsess)
#endif
    GLOBAL TEXT 	XRSESS[STRINGSIZ+1];	/* VMS FORTRAN session ID...*/
    IMPORT TEXT		tae_sess[];		/* saves session id	    */
    IMPORT CODE		applic_type;		/* type of application	    */

FUNCTION VOID BRIDGE2_NAME(xrinim) 
(
    TAEINT	*block,		/* out: storage area			*/
    TAEINT	*dim,		/* in: dimension of block		*/
    TAEINT	*mode,		/* in: P_ABORT or P_CONT		*/
    TAEINT	*status	/* out: status code			*/

 )
    {

    static CODE		ret_code;	
    struct PARBLK	*p;		

/* initialize, get $SESSION, and put it into FORTRAN COMMON 		*/

    p = (struct PARBLK *) block;
    ret_code = p_inim(p, (*dim)*sizeof(TAEINT), *mode);
    *status = ret_code;
    if ((*p).msgtyp == M_INIPAR) 	/* is this first PARBLK?	*/
    	{
	oini_ins(p);			/* installation exit		*/
	s_copy (tae_sess, XRSESS);	/* save in FORTRAN COMMON	*/
    	}

/*  Note: The following should be set in XZINIT rather than here. We
    set it here to avoid another FORTRAN/C global interface.
 */
    applic_type = FORTRAN_TYPE;		/* for error message routing    */
    return;
    }
