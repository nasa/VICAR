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
 *	XRRDB.  Read parameter block from disk.
 *
 *	CHANGE LOG:
 *	
 *	03-apr-88	Apollo conversion: macros for string arguments
 *			and for bridge routine names...ljn
 */

#include "taeconf.inp"
#include "forstr.inp"		/* FORTRAN-77 string definition		*/
#include "taeintproto.h"

FUNCTION VOID BRIDGE2_NAME(xrrdb) 
(
    FORSTR	*blknam,	/* in: file spec in FOR-77 format	*/
    TAEINT	*lun,		/* in: lun to use			*/
    TAEINT	*block,		/* out: block to be filled		*/
    TAEINT	*dim,		/* in: dimension of block		*/
    TAEINT	*mode,		/* in: P_ABORT or P_CONT		*/
    TAEINT	*status	/* out: status code			*/

 );



/*
 *	xrrdb.  Read V-block from disk.
 *
 */

FUNCTION VOID BRIDGE1_NAME(xrrdb) 
(
    TEXT	*blknam,	/* in: file spec in FOR-77 format	*/
    TAEINT	*lun,		/* in: lun to use			*/
    TAEINT	*block,		/* out: block to be filled		*/
    TAEINT	*dim,		/* in: dimension of block		*/
    TAEINT	*mode,		/* in: P_ABORT or P_CONT		*/
    TAEINT	*status,	/* out: status code			*/
    STRLEN	blknaml

 )
    {
	FORSTR	blknamd;

	blknamd.length = GETLEN (blknaml);
	blknamd.pointer = blknam;

	BRIDGE2_NAME(xrrdb) (&blknamd, lun, block, dim, mode, status);

    return;
    }
