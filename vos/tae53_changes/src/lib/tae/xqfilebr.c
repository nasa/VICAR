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
 *	TAE version 1 XQ bridgesfor writing to disk file only.
 *	FORTRAN-callable.
 *	The other XQ routines are in other source files.
 *
 *	CHANGE LOG:
 *
 *	03-apr-88	Apollo conversion: macros for string arguments
 *			and for bridge routine names...ljn
 */

#include	"taeconf.inp"	/* TAE configuration (REQUIRED)		*/
#include	"forstr.inp"	/* fortran-77 string struct		*/
#include "taeintproto.h"


FUNCTION VOID BRIDGE2_NAME(xqwrtb) 
(
    FORSTR	*filespec,	/* in: file spec			*/
    TAEINT	*lun,		/* in: lun to use			*/
    TAEINT	block[],	/* in: V-block to write			*/
    TAEINT	*status	/* out: status code			*/

 );



/*
 *	xqwrtb.   Write vblock to disk.
 */

FUNCTION VOID BRIDGE1_NAME(xqwrtb) 
(
    TEXT	*fspec,		/* in: file spec			*/
    TAEINT	*lun,		/* in: lun to use			*/
    TAEINT	block[],	/* in: V-block to write			*/
    TAEINT	*status,	/* out: status code			*/
    STRLEN	fspecl

 )
    {
	FORSTR	fspecd;

	fspecd.length = GETLEN (fspecl);
	fspecd.pointer = fspec;

	BRIDGE2_NAME(xqwrtb) (&fspecd, lun, block, status);

    return;
    }
