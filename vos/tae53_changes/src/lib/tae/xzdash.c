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
 * 	TAE XZ file routine: xzdash.
 *
 *	CHANGE LOG:
 *
 *	11-jul-83	Purged change log, deleted checkout records,
 *			and audited global definitions...jtm
 *	08-sep-83	Changed V_NOTFILE to V_NOCHECK...palm
 *	13-sep-83	Separated from XZEXIT so these can be replaced
 *			by installation for catalog manger...palm
 *
 */

#include	"stdh.inp"		/* system standard		*/
#include	"taeconf.inp"		/* TAE configuration		*/
#include	"forstr.inp"		/* fortran strings		*/
#include "taeintproto.h"




/* xzdash - Get host spec when TAE spec begins with minus character
 *
 *	This is the default version of xzdash, which does a straight
 *	copy of the tae file name to the host name.
 */
FUNCTION VOID xzdash 
(
    FORSTR		*tae_spec,	/* in: FORTRAN string		*/
    FORSTR		*host_spec,	/* out: FORTRAN string		*/
    TAEINT		*filemode,	/* in: filemode			*/
    TAEINT		*status	/* out: status			*/

 )
    {
    TEXT	string[STRINGSIZ+1];

    s_for2c(tae_spec, string, 0);
    s_c2for(string, host_spec, 0);
    *status = SUCCESS;
    return;
    }
