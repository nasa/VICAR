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



#include	"stdh.inp"
#include	"taeconf.inp"
#include	"symtab.inc"		/* symbol table definitions		*/
#include	"syninc.inc"		/* syntax support			*/
#include "taeintproto.h"
    
/*
 *  CHANGE LOG:
 *  22-oct-92	Prototyping tae_alloc is unnecessary and Ultrix 4.3 does not
 *		like it...rt
 *
 */

/*      TAE string allocation package for C callers.      */


/*
 *    s_save.  Copy string to dynamic storage and return its address.
 *   NULL is returned if no dynamic storage is available.
 */

    FUNCTION TEXT *s_save(

    FAST TEXT *s
    )

    {
    FAST TEXT *p;

    p = tae_alloc(1, s_length(s)+1);	/* allocate for string + eos  */
    if (p != NULL) s_copy(s, p);	/* copy string		      */
    return(p);
    }    

/*
 *	s_free.  De-allocate string which was allocated by s_save.
 */

    FUNCTION VOID s_free(TEXT *s)
    {
    tae_free(s);
    return;
    }
