/******************************************************************************
 *	Copyright (c) 1990, 1991, 1992,
 *	National Aeronautics and Space Administration
 *	ALL RIGHTS RESERVED
 *
 *	The software (programs, data bases and/or documentation) on or in
 *	any media can not be reproduced, disclosed or used except under
 *	terms of the license between COSMIC and your organization.
 *****************************************************************************/

/* "C" bridge */
/*
 *	TAE version 1 XQ bridges for manipulation of already built
 *	parameter blocks only.  The other XQ routines are in other source files.
 *	FORTRAN-callable.
 *
 *	CHANGE LOG:
 *
 *	03-apr-88	Apollo conversion: macros for string arguments
 *			and for bridge routine names...ljn
 *	17-jun-88	Corrected mispell of GETLEN as GELEN...ljn
 *	07-aug-91	Bridge1 for xqdble...ljn
 */

#include	"taeconf.inp"	/* TAE configuration (REQUIRED)		*/
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
FUNCTION VOID BRIDGE2_NAME(xqfile)
(
    TAEINT	block[],	/* in/out: v-block			*/
    FORSTR	*name,		/* in: parameter name in FOR-77 format	*/
    TAEINT	*count,		/* in: number of strings		*/
    FORSTR	*file,		/* in: array of strings, FOR-77 format	*/
    TAEINT	*mode,		/* in: P_UPDAT or P_ADD			*/
    TAEINT	*status	/* out: SUCCESS or error code		*/

);
FUNCTION VOID BRIDGE2_NAME(xqini)
(
    TAEINT	block[],	/* out: initialized block		*/
    TAEINT	*dim,		/* in: dimension of block		*/
    TAEINT	*mode		/* in: P_ABORT or P_CONT		*/

 );
FUNCTION VOID BRIDGE2_NAME(xqintg)
(
    TAEINT	block[],	/* in/out: vblock			*/
    FORSTR	*name,		/* in: parameter name in FOR-77 format	*/
    TAEINT	*count,		/* in: number of integers		*/
    TAEINT	intg[],		/* in: integer values			*/
    TAEINT	*mode,		/* in: P_ADD or P_UPDATE		*/
    TAEINT	*status	        /* out: SUCCESS or error code		*/

 );
FUNCTION VOID BRIDGE2_NAME(xqreal)
(
    TAEINT	block[],	/* in/out: vblock			*/
    FORSTR	*name,		/* in: parameter name in FOR-77 format	*/
    TAEINT	*count,		/* in: number of integers		*/
    float	real[],		/* in: REAL values			*/
    TAEINT	*mode,		/* in: P_ADD or P_UPDATE		*/
    TAEINT	*status	/* out: XSUCC or error code		*/

 );
FUNCTION VOID BRIDGE2_NAME(xqdble)
(
    TAEINT	block[],	/* in/out: vblock			*/
    FORSTR	*name,		/* in: parameter name in FOR-77 format	*/
    TAEINT	*count,		/* in: number of integers		*/
    TAEFLOAT 	dble[],		/* in: DP REAL values			*/
    TAEINT	*mode,		/* in: P_ADD or P_UPDATE		*/
    TAEINT	*status	        /* out: XSUCC or error code		*/

 );
FUNCTION VOID BRIDGE2_NAME(xqdble)
(
    TAEINT	block[],	/* in/out: vblock			*/
    FORSTR	*name,		/* in: parameter name in FOR-77 format	*/
    TAEINT	*count,		/* in: number of integers		*/
    TAEFLOAT 	dble[],		/* in: DP REAL values			*/
    TAEINT	*mode,		/* in: P_ADD or P_UPDATE		*/
    TAEINT	*status	        /* out: XSUCC or error code		*/

 );
FUNCTION VOID BRIDGE2_NAME(xqstr)
(
    TAEINT	block[],	/* in/out: v-block			*/
    FORSTR	*name,		/* in: parameter name in FOR-77 format	*/
    TAEINT	*count,		/* in: number of strings		*/
    FORSTR	*string,	/* in: array of strings, FOR-77 format	*/
    TAEINT	*mode,		/* in: P_UPDAT or P_ADD			*/
    TAEINT	*status	/* out: SUCCESS or error code		*/

 );
FUNCTION VOID BRIDGE2_NAME(xqsetm)
(
    TAEINT	block[],			/* in/out: parameter block */
    TAEINT	*mode				/* in: xcont or xabort	   */

 );




/*
 *	xqfile.   Put file string values into v-block.
 */

FUNCTION VOID BRIDGE1_NAME(xqfile)
(
    TAEINT	block[],	/* in/out: v-block			*/
    TEXT	*name,		/* in: parameter name in FOR-77 format	*/
    TAEINT	*count,		/* in: number of strings		*/
    TEXT	*file,		/* in: array of strings, FOR-77 format	*/
    TAEINT	*mode,		/* in: P_UPDAT or P_ADD			*/
    TAEINT	*status,	/* out: SUCCESS or error code		*/
    STRLEN	namel,
    STRLEN	filel

 )
    {
	FORSTR	named;
	FORSTR	filed;

	named.length = GETLEN (namel);
	named.pointer = name;

	filed.length = GETLEN (filel);
	filed.pointer = file;

	BRIDGE2_NAME(xqfile) (block, &named, count, &filed, mode, status);

    return;
    }

/*
 *	xqini.	Initialize V-block.
 */

FUNCTION VOID BRIDGE1_NAME(xqini)
(
    TAEINT	block[],	/* out: initialized block		*/
    TAEINT	*dim,		/* in: dimension of block		*/
    TAEINT	*mode		/* in: P_ABORT or P_CONT		*/

 )
    {
	BRIDGE2_NAME(xqini) (block, dim, mode);

    return;
    }

/*
 *	xqintg.   Put integer value(s) into a vblock.
 */

FUNCTION VOID BRIDGE1_NAME(xqintg)
(
    TAEINT	block[],	/* in/out: vblock			*/
    TEXT	*name,		/* in: parameter name in FOR-77 format	*/
    TAEINT	*count,		/* in: number of integers		*/
    TAEINT	intg[],		/* in: integer values			*/
    TAEINT	*mode,		/* in: P_ADD or P_UPDATE		*/
    TAEINT	*status,	/* out: SUCCESS or error code		*/
    STRLEN	namel

 )
    {
	FORSTR	named;

	named.length = GETLEN (namel);
	named.pointer = name;

	BRIDGE2_NAME(xqintg) (block, &named, count, intg, mode, status);

    return;
    }

/*
 *	xqreal.  Put real value(s) into a vblock.
 */

FUNCTION VOID BRIDGE1_NAME(xqreal)
(
    TAEINT	block[],	/* in/out: vblock			*/
    TEXT	*name,		/* in: parameter name in FOR-77 format	*/
    TAEINT	*count,		/* in: number of integers		*/
    float	real[],		/* in: REAL values			*/
    TAEINT	*mode,		/* in: P_ADD or P_UPDATE		*/
    TAEINT	*status,	/* out: XSUCC or error code		*/
    STRLEN	namel

 )
    {
	FORSTR	named;

	named.length = GETLEN (namel);
	named.pointer = name;

	BRIDGE2_NAME(xqreal) (block, &named, count, real, mode, status);

    return;
    }

/*
 *	xqdble.  Put double precision real value(s) into a vblock.
 */

FUNCTION VOID BRIDGE1_NAME(xqdble) 
(
    TAEINT	block[],	/* in/out: vblock			*/
    TEXT	*name,		/* in: parameter name in FOR-77 format	*/
    TAEINT	*count,		/* in: number of integers		*/
    TAEFLOAT	dble[],		/* in: DP REAL values			*/
    TAEINT	*mode,		/* in: P_ADD or P_UPDATE		*/
    TAEINT	*status,	/* out: XSUCC or error code		*/
    STRLEN	namel

 )
    {
	FORSTR	named;

	named.length = GETLEN (namel);
	named.pointer = name;

	BRIDGE2_NAME(xqdble) (block, &named, count, dble, mode, status);

    return;
    }

/*	xqsetm.   Set mode in parameter block.
 */

FUNCTION VOID BRIDGE1_NAME(xqsetm)
(
    TAEINT	block[],			/* in/out: parameter block */
    TAEINT	*mode				/* in: xcont or xabort	   */

 )
    {
	BRIDGE2_NAME(xqsetm) (block,mode);

    return;
    }

/*
 *	xqstr.   Put string values into v-block.
 */

FUNCTION VOID BRIDGE1_NAME(xqstr) 
(
    TAEINT	block[],	/* in/out: v-block			*/
    TEXT	*name,		/* in: parameter name in FOR-77 format	*/
    TAEINT	*count,		/* in: number of strings		*/
    TEXT	*string,	/* in: array of strings, FOR-77 format	*/
    TAEINT	*mode,		/* in: P_UPDAT or P_ADD			*/
    TAEINT	*status,	/* out: SUCCESS or error code		*/
    STRLEN	namel,
    STRLEN	stringl

 )
    {
	FORSTR	named;
	FORSTR	stringd;

	named.length = GETLEN (namel);
	named.pointer = name;

	stringd.length = GETLEN (stringl);
	stringd.pointer = string;

	BRIDGE2_NAME(xqstr) (block, &named, count, &stringd, mode, status);

    return;
    }
