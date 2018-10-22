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



/*
 *	TAE version 1 XQ bridges for intertask communication only.
 *	FORTRAN-callable.
 *	The other XQ routines are in other source files.
 *
 *	CHANGE LOG:
 *
 *	03-apr-88	Apollo conversion: macro for bridge routine names...ljn
 */

#include	"taeconf.inp"	/* TAE configuration (REQUIRED)		*/
#include	"parblk.inc"	/* parameter block definitions		*/
#include	"forstr.inp"	/* fortran-77 string struct		*/
#include "taeintproto.h"





/*
 *	xqdynp.  Ship vblock to TM for dynamic parameters.
 */
 
FUNCTION VOID BRIDGE2_NAME(xqdynp) 
(
    FORSTR	*pdfname,	/* in: pdf name in FOR-77 format	*/
    TAEINT	block[],	/* in: V-block				*/
    TAEINT	*mode,		/* in: M_FULLPDF or M_SUBPDF		*/
    TAEINT	*status	/* out: SUCCESS or error code		*/

 )
    {
    CODE	code;
    TEXT	cpdfname[STRINGSIZ+1];

    s_for2c(pdfname, cpdfname, 0);	/* convert name to  c string */
    s_strip(cpdfname);
    code = q_dynp((struct PARBLK *) block, cpdfname, *mode);
    *status = code;
    return;
    }

/*
 *	xqout. Send  vblock to the TAE monitor.
 */

FUNCTION  VOID  BRIDGE2_NAME(xqout) 
(
    TAEINT	block[],	/* in/out: vblock			*/
    TAEINT	*status	/* out: SUCCESS or error code		*/


 )
    {
    CODE	code;

    code = q_out((struct PARBLK *)block);
    *status = code;
    return;
    }
