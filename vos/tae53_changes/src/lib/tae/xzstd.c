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
 * 	TAE XZ file routine: xzstd
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
#include 	"symtab.inc"
#include 	"forstr.inp"		/* FORTRAN strings		*/
#include "taeintproto.h"




/* xzstd - Get host spec when TAE spec does not begin with percent character
 *
 */
FUNCTION VOID xzstd
(
    FORSTR 		*tae_spec,	/* in: tae spec FORTRAN string	*/
    FORSTR 		*host_spec,	/* out: host spec FORTRAN string*/
    TAEINT		*filemode,	/* in: filemode			*/
    TAEINT		*status	/* out: status			*/

 )
    {
    TEXT	string[STRINGSIZ+1];

    *status = SUCCESS;			/* assume success		*/

#ifdef XXXXXXX				/* code no longer good -- this	*/
					/* calls prototype XA and 	*/
					/* assumes packed/eos		*/
#define XASUCCESS 1
    if (*filemode == V_IN || *filemode == V_INOUT)
    	xahost(tae_spec, host_spec, status);	/* cat man for existing file	*/
    else if (*filemode == V_NOCHECK)
    	s_copy(tae_spec, host_spec);
    else if (*filemode == V_OUT)
    	xafile(host_spec, status);	/* new file spec generated	*/
    if (*status == XASUCCESS)
    	*status = SUCCESS;		/* TAE standard success code	*/

#else
    s_for2c(tae_spec, string, 0);
    s_c2for(string, host_spec, 0);	/* copy string if no CATMAN	*/
#endif
    return;
    }    
