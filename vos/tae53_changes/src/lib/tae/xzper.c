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
 * 	TAE XZ file routine: XZPER
 *
 *	CHANGE LOG:
 *
 *	11-jul-83	Purged change log, deleted checkout records,
 *			and audited global definitions...jtm
 *	08-sep-83	Changed V_NOTFILE to V_NOCHECK...palm
 *	13-sep-83	Separated from XZEXIT so these can be replaced
 *			by installation for catalog manger...palm
 *	14-sep-83	Make strings fortran format...palm
 *
 */

#include	"stdh.inp"		/* system standard		*/
#include	"taeconf.inp"		/* TAE configuration		*/
#include 	"forstr.inp"		/* fortran strings		*/
#include "taeintproto.h"



/* xzper - Get host spec when TAE spec begins with percent character
 *
 */
FUNCTION VOID xzper
(
    FORSTR 		*tae_spec,	/* in: tae spec FORTRAN string	*/
    FORSTR 		*host_spec,	/* out: host spec FORTRAN string*/
    TAEINT		*filemode,	/* in: filemode			*/
    TAEINT		*status	/* out: status			*/

 )
    {
    TEXT	string[STRINGSIZ+1];	/* temp string			*/
    
    s_for2c(tae_spec, string, 0);
    s_c2for(&string[1], host_spec, 0);	/* strip leading %		*/
    *status = SUCCESS;
    return;
    }
