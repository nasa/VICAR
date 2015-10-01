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



/*	Move a symbol table.
 */

/*	Change log:
 *
 */

/*
 *	Standard TAE include files.
 */

#include	"stdh.inp"	/* system standard  (REQUIRED)		*/
#include	"taeconf.inp"	/* TAE configuration (REQUIRED)		*/
#include	"symtab.inc"	/* TM symbol table			*/
#include	"tminc.inc"	/* TM definitions			*/
#include "taeintproto.h"


/*	movest.  Move a symbol table.
 *	Assumes that the caller has allocated both of the actual symbol table
 *	structures.  Deletes the destination table (if it's not initially
 *	empty) before doing the move.
 */

FUNCTION CODE movest 
(
    struct SYMTAB	*st1,		/* in:  symbol table to move from	*/
    struct SYMTAB	*st2		/* out: symbol table to move to		*/

 )
    {
    struct VARIABLE	*v1;
    struct VARIABLE	*v2;
    CODE		code;

    deltab(st2);			/* clear the destination symbol table	*/
    for (v1 = (*st1).link; v1 != NULL; v1 = (*v1).v_link)
	{
	v2 = allvar(st2);
	if (v2 == NULL)
	    {
	    deltab(st2);
	    return(FAIL);
	    }
	code = vcopy(v1, v2);
	if (code != SUCCESS)
	    {
	    deltab(st2);
	    return(FAIL);
	    }
	}
    return(SUCCESS);
    }
