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
 *	XR (native) bridge excluding the XRF routines and excluding
 *	all XR routines not directly associated with intertask communication.
 *
 *	CHANGE LOG:
 *
 *	03-apr-88	Apollo conversion: macros for bridge routine names...ljn
 */

#include "taeconf.inp"
#include "forstr.inp"		/* FORTRAN-77 string definition		*/
#include "taeintproto.h"



FUNCTION VOID BRIDGE2_NAME(xrinim) 
(
    TAEINT	*block,		/* out: storage area			*/
    TAEINT	*dim,		/* in: dimension of block		*/
    TAEINT	*mode,		/* in: P_ABORT or P_CONT		*/
    TAEINT	*status	/* out: status code			*/

 );


/*
 *	XRINIM.  Initialize BLOCK from Terminal Monitor.
 */

FUNCTION VOID BRIDGE1_NAME(xrinim) 
(
    TAEINT	*block,		/* out: storage area			*/
    TAEINT	*dim,		/* in: dimension of block		*/
    TAEINT	*mode,		/* in: P_ABORT or P_CONT		*/
    TAEINT	*status	/* out: status code			*/

 )
    {
	BRIDGE2_NAME(xrinim) (block, dim, mode, status);

    return;
    }
