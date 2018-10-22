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



/*TDM         CHECKOUT FILE_TIME=25-MAR-1983 18:13 DMA1:[TAEV1.OLB]XM.C;13 */
/*
 *	XMPUT.   TAE vesrion 1 Fortran-callable message logger.
 *
 *
 *
 * 	CHANGE LOG:
 *
 * 	21-jun-83	Increase key size to STRINGSIZ+1...dm
 *	03-apr-88	Apollo conversion: macro for bridge routine names...ljn
 */


#include	"taeconf.inp"	/* TAE configuration (REQUIRED)		*/
#include 	"forstr.inp"	/* Fortran-77 string structure		*/
#include "taeintproto.h"



FUNCTION VOID BRIDGE2_NAME(xmput) 
(
    FORSTR	*message,	/* in: FORTRAN-77 message string to log	*/
    FORSTR	*key,		/* in: for-77 message key			*/
    TAEINT	*status	/* out: status code			*/

 )
    {
    TEXT	c_msg[STRINGSIZ+1];
    TEXT	c_key[STRINGSIZ+1];
    CODE 	code;
    
    s_for2c(message, c_msg, 0);			/* convert message to  c form */
    s_for2c(key, c_key, 0);			/* convert key to c form */
    code = p_mput(c_msg, c_key );		/* send msg with key */
    *status = code;
    return;
    }
