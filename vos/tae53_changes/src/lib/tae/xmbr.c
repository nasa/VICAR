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
 *	XMPUT.   TAE version 1 Fortran-callable message logger.
 *
 *	CHANGE LOG:
 *
 *	03-apr-88	Apollo conversion: macros for string args and
 *			for bridge routine names...ljn
 *
 */


#include	"taeconf.inp"	/* TAE configuration (REQUIRED)		*/
#include 	"forstr.inp"	/* Fortran-77 string structure		*/
#include "taeintproto.h"

FUNCTION VOID BRIDGE2_NAME(xmput) 
(
    FORSTR	*message,	/* in: FORTRAN-77 message string to log	*/
    FORSTR	*key,		/* in: for-77 message key			*/
    TAEINT	*status	/* out: status code			*/

 );


FUNCTION VOID BRIDGE1_NAME(xmput) 
(
    TEXT	*message,	/* in: FORTRAN-77 message string to log	*/
    TEXT	*key,		/* in: for-77 message key		*/
    TAEINT	*status,	/* out: status code			*/
    STRLEN	messagel,
    STRLEN	keyl

 )
    {
	FORSTR	messaged;
	FORSTR	keyd;

	messaged.length = GETLEN (messagel);
	messaged.pointer = message;

	keyd.length = GETLEN (keyl);
	keyd.pointer = key;

	BRIDGE2_NAME(xmput) (&messaged, &keyd, status);

    return;
    }
