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



/*
 * 	TAE XZEXIT bridge.  FORTRAN callable.
 *
 *	CHANGE LOG:
 *
 *	11-jul-83	Purged change log, deleted checkout records,
 *			and audited global definitions...jtm
 *	08-sep-83	Changed V_NOTFILE to V_NOCHECK...palm
 *	13-sep-83	Separated XZEXIT from XZ file management functions
 *			so the file management functions can be changed
 *			for catalog manager...palm
 *	11-oct-83	Separated from xzhost...palm
 *	03-apr-88	Apollo conversion: macro for bridge routine names...ljn
 *
 */

#include	"taeconf.inp"		/* TAE configuration		*/
#include	"forstr.inp"		/* fortran-77 string struct	*/
#include 	"symtab.inc"
#include "taeintproto.h"




/*
 *	XZEXIT.	Terminate application process formally.
 */

FUNCTION  VOID  BRIDGE2_NAME(xzexit) 
(
    TAEINT		*sfi,		/* in: success/fail indicator	*/
    FORSTR		*skey		/* in: error key string		*/

 )
    {
    TEXT		cskey[STRINGSIZ+1];	/* key string in c	*/

    s_for2c(skey, cskey,0);		/* convert to c_string		*/
    s_strip(cskey);			/* delete trailing blanks	*/
    z_exit(*sfi, cskey);		
    return;
    }
