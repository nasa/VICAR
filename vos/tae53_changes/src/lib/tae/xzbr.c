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
 * 	TAE XZEXIT bridge.  FORTRAN callable.
 *
 *	CHANGE LOG:
 *
 *	03-apr-88	Apollo conversion: macros for string arguments
 *			and for bridge routine names...ljn
 */

#include	"taeconf.inp"		/* TAE configuration		*/
#include	"forstr.inp"		/* fortran-77 string struct	*/
#include "taeintproto.h"




/*
 *	XZEXIT.	Terminate application process formally.
 */

FUNCTION  VOID  BRIDGE2_NAME(xzexit) 
(
    TAEINT		*sfi,		/* in: success/fail indicator	*/
    FORSTR		*skey		/* in: error key string		*/

 );
FUNCTION  VOID  BRIDGE1_NAME(xzexit) 
(
    TAEINT		*sfi,		/* in: success/fail indicator	*/
    TEXT		*skey,		/* in: error key string		*/
    STRLEN		skeyl

 )
    {
	FORSTR	skeyd;

	skeyd.length = GETLEN (skeyl);
	skeyd.pointer = skey;

	BRIDGE2_NAME(xzexit) (sfi, &skeyd);

    return;
    }
