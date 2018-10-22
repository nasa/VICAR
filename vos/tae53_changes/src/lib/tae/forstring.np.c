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



/*<<VAX/UNIX>>*/

/*
 *	host-dependent string manipulation for FORTRAN-77 strings.
 *
 *	CHANGE LOG:
 *	
 *	11-jul-83	Purged change log, deleted checkout records,
 *			and audited global definitions...jtm
 */


#include  "stdh.inp"
#include  "taeconf.inp"
#include  "forstr.inp"
#include "taeintproto.h"


/*
 * 	s_c2for.  Convert C string to FORTRAN-77 format.  VAX-dependent.
 *	Returns FAIL if the FORTRAN-77 string is not large enough.
 */
FUNCTION CODE s_c2for
(
 TEXT		*c_string,	/* in: source string		*/
 FORSTR		*for_string,	/* out: FORTRAN-77 string	*/
 FUNINT		index		/* in: index in for_string	*/
				/* (zero if for_string scalar)	*/
 )
	{
	TEXT	*p;
	COUNT	l;
        COUNT	i;

	p = (*for_string).pointer;	/* get start of for_string	*/
	l = (*for_string).length;	/* get length of one string	*/
	p += l*index;			/* start of ith string		*/

	for(i=0; *c_string != 0 && i < l; i++, c_string++, p++)
	    *p  = *c_string;		/* move characters		*/
	if (i == l && *c_string != 0)
	    return(FAIL);		/* string too long for caller	*/
	for(; i < l; i++, p++)
	    *p = ' ';			/* blank fill			*/
	return(SUCCESS);
	}	    

/*
 *	s_declfor  Declare a FORTRAN-77 string, given the C structure.
 *
 */
FUNCTION VOID s_declfor
(
 FORSTR		*for_string,		/* in/out: uninitialized FORTRAN string	*/
 TEXT		*address,		/* in: address of string	*/
 FUNINT		length			/* in: fixed-length size	*/
)
    {
    (*for_string).pointer = address;		/* set start of for_string	*/
    (*for_string).length = length;	/* get length of one string	*/
    return;
    }

/*
 *	s_for2c.   Convert FORTRAN-77 string to C string.  VAX-dependent.
 *	
 *	Returns FAIL if the FORTRAN string is longer than STRINGSIZ.
 */

FUNCTION CODE s_for2c
(
 FORSTR		*for_string,		/* in: FORTRAN string		*/
 TEXT		c_string[STRINGSIZ+1],	/* out: C string		*/
 FUNINT		index			/* in: index in fortran string	*/
						/* (zero if for_string scalar)	*/
 )
	{
	TEXT	*p;			/* pointer into FORTRAN string	*/
	COUNT	l;			/* working string length	*/
        COUNT   i;

	p = (*for_string).pointer;	/* working pointer		*/
	l = (*for_string).length;	/* string length		*/
	if (l > STRINGSIZ) return(FAIL);
	p += l*index;			/* start of string #(index+1)	*/

	for(i=0; i < l;  i++, p++)
	    c_string[i] = *p;
	c_string[l] = EOS;
	return(SUCCESS);
	}

/*
 * 	s_forlen.   Return length of FORTRAN-77 string.
 */

FUNCTION COUNT s_forlen
(
 FORSTR	*for_string
)
    {
    return((*for_string).length);
    }

/*
 *	s_fortxt.	Return start address  of FORTRAN-77 string text.
 */

FUNCTION  TEXT  * s_fortxt
(
 FORSTR	*for_string
)
{
  return ((*for_string).pointer);
}

