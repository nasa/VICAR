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



/* TDM CHECKOUT FILE_TIME= 2-SEP-1983 10:50 DUA0:[TAEV1.OLB]XQFILE.C;2 */
/*
 *	TAE version 1 XQ bridgesfor writing to disk file only.
 *	FORTRAN-callable.
 *	The other XQ routines are in other source files.
 *
 *	CHANGE LOG:
 *
 *	25-oct-83	Match error codes with PGM...dm
 *	03-apr-88	Apollo conversion: macro for bridge routine names...ljn
 *
 */

#include	"taeconf.inp"	/* TAE configuration (REQUIRED)		*/
#include	"parblk.inc"	/* parameter block definitions		*/
#include	"forstr.inp"	/* fortran-77 string struct		*/
#include "taeintproto.h"





/*
 *	xqwrtb.   Write vblock to disk.
 */
    
FUNCTION VOID BRIDGE2_NAME(xqwrtb) 
(
    FORSTR	*filespec,	/* in: file spec			*/
    TAEINT	*lun,		/* in: lun to use			*/
    TAEINT	block[],	/* in: V-block to write			*/
    TAEINT	*status	/* out: status code			*/

 )
    {
    struct PARBLK	*p;
    CODE		code;
    TEXT		cfilespec[STRINGSIZ+1];

    code = s_for2c(filespec, cfilespec, 0);	/* convert to  c string	*/
    if (code != SUCCESS)
	{
	*status = P_BADNAME;			/* name too big		*/
	return;					/* name too big		*/
	}
    s_strip(cfilespec);
    p = (struct PARBLK *) block;
    code = q_wrtb((TEXT *) cfilespec, *lun, p);
    *status = code;
    return;
    }
