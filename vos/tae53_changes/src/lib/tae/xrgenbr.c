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
 *	XR (native) bridge excluding the XRF routines.
 *	The routines in this source are only those associated directly
 *	with retrieving parameters from an already built PARBLK.
 *	The routines for building a PARBLK are in other files.
 *
 *	CHANGE LOG:
 *
 *	03-apr-88	Apollo conversion: macros for string arguments
 *			and for bridge routine names...ljn
 *	07-aug-91	xrdble() needs first-order bridge routine...ljn
 */

#include "taeconf.inp"
#include "forstr.inp"		/* FORTRAN-77 string definition		*/
#include "taeintproto.h"

FUNCTION VOID BRIDGE2_NAME(xrattr) 
(
    TAEINT		*block,		/* in: parameter block		*/
    FORSTR		*name,		/* in: parameter name		*/
    TAEINT		*type,		/* out: parameter type		*/
    TAEINT		*n,		/* out: number of components	*/
    TAELOG		*defalt,	/* out: .TRUE. if defaulted	*/
    TAEINT		*access,	/* out: access mode if a file	*/
    TAEINT		*status	/* out: status code		*/

 );
FUNCTION VOID BRIDGE2_NAME(xrintg) 
(
    TAEINT		*block,		/* in: parameter block		*/
    FORSTR		*name,		/* in: parameter name		*/
    TAEINT		*dimen,		/* in: max # of values(dim of intg)	*/
    int			intg[],		/* out: array with intger var	*/
    TAEINT		*n,		/* out: integer value count	*/
    TAEINT		*status	/* out: status code		*/

 );
FUNCTION VOID BRIDGE2_NAME(xrreal) 
(
    TAEINT		*block,		/* in: parameter block		*/
    FORSTR		*name,		/* in: parameter name		*/
    TAEINT		*dimen,		/* in: max # of values(dim of intg)	*/
    float		real[],		/* out: array with real var	*/
    TAEINT		*n,		/* out: real value count	*/
    TAEINT		*status	/* out: status code		*/

 );
FUNCTION VOID BRIDGE2_NAME(xrdble) 
(
    TAEINT		*block,		/* in: parameter block		*/
    FORSTR		*name,		/* in: parameter name		*/
    TAEINT		*dimen,		/* in: max # of values(dim of intg)	*/
    TAEFLOAT		dble[],		/* out: array with real var	*/
    TAEINT		*n,		/* out: real value count	*/
    TAEINT		*status	/* out: status code		*/

 );
FUNCTION  VOID  BRIDGE2_NAME(xrherr) 
(
    TAEINT		*block,			/* in: parameter block */
    TAEINT		*hcode			/* out: host error code	*/

 );
FUNCTION VOID BRIDGE2_NAME(xrsetm) 
(
    TAEINT	block[],			/* in/out: parameter block */
    TAEINT	*mode				/* in: xcont or xabort	   */

 );
FUNCTION VOID BRIDGE2_NAME(xrstr) 
(
    TAEINT		*block,		/* in: parameter block		*/
    FORSTR		*name,		/* in: parameter name		*/
    TAEINT		*dimen,		/* in: dimension of string	*/
    FORSTR		*string,	/* out: FOR-77 string(s)	*/
    TAEINT		length[],	/* out: length of each string	*/
    TAEINT		*n,		/* out: number of strings	*/
    TAEINT		*status	/* out: status code		*/

 );




/*
 *	xrattr. Get parameter/variable attributes.
 */

FUNCTION VOID BRIDGE1_NAME(xrattr) 
(
    TAEINT		*block,		/* in: parameter block		*/
    TEXT		*name,		/* in: parameter name		*/
    TAEINT		*type,		/* out: parameter type		*/
    TAEINT		*n,		/* out: number of components	*/
    TAELOG		*defalt,	/* out: .TRUE. if defaulted	*/
    TAEINT		*access,	/* out: access mode if a file	*/
    TAEINT		*status,	/* out: status code		*/
    STRLEN		namel

 )
    {
	FORSTR	named;

	named.length = GETLEN (namel);
	named.pointer = name;

	BRIDGE2_NAME(xrattr) (block, &named, type, n, defalt, access, status);

    return;
    }

/*
 *	XRINTG.  Get integer parameters/variables.
 */
FUNCTION VOID BRIDGE1_NAME(xrintg) 
(
    TAEINT		*block,		/* in: parameter block		*/
    TEXT		*name,		/* in: parameter name		*/
    TAEINT		*dimen,		/* in: max # of vals(dim of int)*/
    int			intg[],		/* out: array with intger var	*/
    TAEINT		*n,		/* out: integer value count	*/
    TAEINT		*status,	/* out: status code		*/
    STRLEN		namel

 )
    {
	FORSTR	named;

	named.length = GETLEN (namel);
	named.pointer = name;

	BRIDGE2_NAME(xrintg) (block, &named, dimen, intg, n, status);

    return;
    }

/*
 *	XRREAL.  Get real parameters/variables.
 */
FUNCTION VOID BRIDGE1_NAME(xrreal) 
(
    TAEINT		*block,		/* in: parameter block		*/
    TEXT		*name,		/* in: parameter name		*/
    TAEINT		*dimen,		/* in: max # of vals(dim of int)*/
    float		real[],		/* out: array with real var	*/
    TAEINT		*n,		/* out: real value count	*/
    TAEINT		*status,	/* out: status code		*/
    STRLEN		namel

 )
    {
	FORSTR	named;

	named.length = GETLEN (namel);
	named.pointer = name;

	BRIDGE2_NAME(xrreal) (block, &named, dimen, real, n, status);

    return;
    }

/*
 *	XRDBLE.  Get double precision real parameters/variables.
 */
FUNCTION VOID BRIDGE1_NAME(xrdble) 
(
    TAEINT		*block,		/* in: parameter block		*/
    TEXT		*name,		/* in: parameter name		*/
    TAEINT		*dimen,		/* in: max # of vals(dim of int)*/
    TAEFLOAT		dble[],		/* out: array with DP real var	*/
    TAEINT		*n,		/* out: DP real value count	*/
    TAEINT		*status,	/* out: status code		*/
    STRLEN		namel

 )
    {
	FORSTR	named;

	named.length = GETLEN (namel);
	named.pointer = name;

	BRIDGE2_NAME(xrdble) (block, &named, dimen, dble, n, status);

    return;
    }

/*
 * 	XRHERR. Get host-dependent error code.
 */

FUNCTION  VOID  BRIDGE1_NAME(xrherr) 
(
    TAEINT		*block,			/* in: parameter block */
    TAEINT		*hcode			/* out: host error code	*/

 )
    {
	BRIDGE2_NAME(xrherr) (block, hcode);

    return;
    }


/*	XRSETM.   Set mode in parameter block.
 */

FUNCTION VOID BRIDGE1_NAME(xrsetm) 
(
    TAEINT	block[],			/* in/out: parameter block */
    TAEINT	*mode				/* in: xcont or xabort	   */

 )
    {
	BRIDGE2_NAME(xrsetm) (block, mode);

    return;
    }

/*
 *	XRSTR.  Fetch string parameter.
 */

FUNCTION VOID BRIDGE1_NAME(xrstr) 
(
    TAEINT		*block,		/* in: parameter block		*/
    TEXT		*name,		/* in: parameter name		*/
    TAEINT		*dimen,		/* in: dimension of string	*/
    TEXT		*string,	/* out: FOR-77 string(s)	*/
    TAEINT		length[],	/* out: length of each string	*/
    TAEINT		*n,		/* out: number of strings	*/
    TAEINT		*status,	/* out: status code		*/
    STRLEN		namel,
    STRLEN		stringl

 )
    {
	FORSTR	named;
	FORSTR	stringd;

	named.length = GETLEN (namel);
	named.pointer = name;

	stringd.length = GETLEN (stringl);
	stringd.pointer = string;

	BRIDGE2_NAME(xrstr) (block, &named, dimen, &stringd, length, n, status);

    return;
    }
