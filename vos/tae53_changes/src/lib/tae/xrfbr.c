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
 *	XRF (native) bridge routines.
 *
 *	CHANGE LOG:
 *
 *	03-apr-88	Apollo conversion: macros for string arguments
 *			and for bridge routine names...ljn
 */

#include "taeconf.inp"
#include "forstr.inp"		/* FORTRAN-77 string definition		*/
#include "taeintproto.h"

FUNCTION VOID  BRIDGE2_NAME(xrfini) 
(
    TAEINT	*block			/* parameter block		*/

 );
FUNCTION  VOID  BRIDGE2_NAME(xrforp) 
(
    TAEINT		*block,		/* in: parameter block		*/
    TAEINT		*length,	/* in: length of output line	*/
    FORSTR		*fstring,	/* output line in FOR-77 format	*/
    TAEINT		*status	/* out: status code		*/

 );




/*
 * 	xrfini. Initialise for parameter formatting.
 */

FUNCTION VOID  BRIDGE1_NAME(xrfini) 
(
    TAEINT	*block			/* parameter block		*/

 )
    {
	BRIDGE2_NAME(xrfini) (block);

    return;
    }

/*
 * 	xrforp.  Format parameters/variables.
 */

FUNCTION  VOID  BRIDGE1_NAME(xrforp) 
(
    TAEINT		*block,		/* in: parameter block		*/
    TAEINT		*length,	/* in: length of output line	*/
    TEXT		*fstring,	/* output line in FOR-77 format	*/
    TAEINT		*status,	/* out: status code		*/
    STRLEN		fstringl

 )
    {
	FORSTR	fstringd;

	fstringd.length = GETLEN (fstringl);
	fstringd.pointer = fstring;

	BRIDGE2_NAME(xrforp) (block, length, &fstringd, status);

    return;
    }
