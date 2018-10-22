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



/* "C" bridge */
/*
 *	TAE version 1 XQ bridges for intertask communication only.
 *	FORTRAN-callable.
 *	The other XQ routines are in other source files.
 *
 *	CHANGE LOG:
 *
 *	03-apr-88	Apollo conversion: macros string arguments
 *			and for bridge routine names...ljn
 */

#include	"taeconf.inp"	/* TAE configuration (REQUIRED)		*/
#include	"forstr.inp"	/* fortran-77 string struct		*/
#include "taeintproto.h"

FUNCTION VOID BRIDGE2_NAME(xqdynp) 
(
    FORSTR	*pdfname,	/* in: pdf name in FOR-77 format	*/
    TAEINT	block[],	/* in: V-block				*/
    TAEINT	*mode,		/* in: M_FULLPDF or M_SUBPDF		*/
    TAEINT	*status	/* out: SUCCESS or error code		*/

 );
FUNCTION  VOID  BRIDGE2_NAME(xqout) 
(
    TAEINT	block[],	/* in/out: vblock			*/
    TAEINT	*status	/* out: SUCCESS or error code		*/


 );



/*
 *	xqdynp.  Ship vblock to TM for dynamic parameters.
 */

FUNCTION VOID BRIDGE1_NAME(xqdynp) 
(
    TEXT	*pdfname,	/* in: pdf name in FOR-77 format	*/
    TAEINT	block[],	/* in: V-block				*/
    TAEINT	*mode,		/* in: M_FULLPDF or M_SUBPDF		*/
    TAEINT	*status,	/* out: SUCCESS or error code		*/
    STRLEN	pdfnamel

 )
    {
	FORSTR	pdfnamed;

	pdfnamed.length = GETLEN (pdfnamel);
	pdfnamed.pointer = pdfname;

	BRIDGE2_NAME(xqdynp) (&pdfnamed, block, mode, status);

    return;
    }

/*
 *	xqout. Send  vblock to the TAE monitor.
 */

FUNCTION  VOID  BRIDGE1_NAME(xqout) 
(
    TAEINT	block[],	/* in/out: vblock			*/
    TAEINT	*status	/* out: SUCCESS or error code		*/


 )
    {
	BRIDGE2_NAME(xqout) (block,status);

    return;
    }
