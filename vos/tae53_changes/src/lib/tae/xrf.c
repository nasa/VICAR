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
 *	XRF (native) bridge routines.
 *
 *	CHANGE LOG:
 *
 *	03-apr-88	Apollo conversion: macro for bridge routine names...ljn
 */

#include "taeconf.inp"
#include "parblk.inc"		/* get P_BADNAME, etc.			*/
#include "forstr.inp"		/* FORTRAN-77 string definition		*/
#include "taeintproto.h"





/*
 * 	XRFINI. Initialise for parameter formatting.
 */

FUNCTION VOID  BRIDGE2_NAME(xrfini) 
(
    TAEINT	*block			/* parameter block		*/

 )
    {
    p_inifor((struct PARBLK *)block);
    return;
    }

/*
 * 	XRFORP.  Format parameters/variables.
 */
 
FUNCTION  VOID  BRIDGE2_NAME(xrforp) 
(
    TAEINT		*block,		/* in: parameter block		*/
    TAEINT		*length,	/* in: length of output line	*/
    FORSTR		*fstring,	/* output line in FOR-77 format	*/
    TAEINT		*status	/* out: status code		*/

 )
    {
    struct 	PARBLK *parblk;
    CODE		ret_code;
    COUNT		nchar;		/* number of characters		*/
    TEXT		*p;		/* pointer to text string	*/
    COUNT		i;

    p = s_fortxt(fstring);		/* ptr to start of text in fstring */
    *p = EOS;				/* initialize to null string 	*/
    parblk = ( struct PARBLK *) block;
    ret_code = p_forp(parblk, p, (*length-1));	/* format, leave room for EOS	*/
    if (ret_code == SUCCESS)
	{				/* convert to FOR-77 format 	*/
	nchar = s_length(p);		/* # of char in formatted line	*/
   	p += nchar;			/* pointer to logical end 	*/
	for (i = nchar; i < *length; i++, p++)
	    *p = ' ';			/* blank fill the rest		*/
	}
    *status = ret_code;		
    return;
    }
