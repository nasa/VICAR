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



/* TDM CHECKOUT FILE_TIME=11-OCT-1983 18:03 DUA0:[TAEV1.OLB]XRRDB.C;3 */
/*
 *	XRRDB.  Read parameter block from disk.
 *
 *	CHANGE LOG:
 *	
 *	11-oct-83	Unix compilation errors...palm
 *	18-NOV-83	Add P_BADNAME error check...dm
 *	03-apr-88	Apollo Conversion: macro for bridge routine names...ljn
 *
 */

#include "taeconf.inp"
#include "forstr.inp"		/* FORTRAN-77 string definition		*/
#include "parblk.inc"
#include "taeintproto.h"





/*
 *	xrrdb.  Read V-block from disk.
 *
 */

FUNCTION VOID BRIDGE2_NAME(xrrdb) 
(
    FORSTR	*blknam,	/* in: file spec in FOR-77 format	*/
    TAEINT	*lun,		/* in: lun to use			*/
    TAEINT	*block,		/* out: block to be filled		*/
    TAEINT	*dim,		/* in: dimension of block		*/
    TAEINT	*mode,		/* in: P_ABORT or P_CONT		*/
    TAEINT	*status	/* out: status code			*/

 )
    {
    CODE	code;
    CODE	ret_code;
    TEXT	c_fspec[STRINGSIZ+1];

    code = s_for2c(blknam, c_fspec, 0);			/* cvt to c string */
    if (code != SUCCESS)
	{
	*status = P_BADNAME;
	return;
	}
    s_strip(c_fspec);					/* strip blanks */
    ret_code = p_rdb(c_fspec, 				/* file spec	*/
    	         *lun,					/* lun		*/
    		 (struct PARBLK *)block,		/* block	*/
    		 *dim * sizeof(TAEINT),			/* bytes	*/
    		 *mode );				/* mode 	*/
    *status = ret_code;
    return;
    }
