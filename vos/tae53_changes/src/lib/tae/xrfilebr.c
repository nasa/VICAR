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



/* "C" bridge */
/*
 *	XRFILE.   Obtain file parameter.
 *
 *	CHANGE LOG:
 *	
 *	03-apr-88	Apollo conversion: macros for string arguments
 *			and for bridge routine names...ljn
 *	26-mar-91	Added f_force_lower...krw
 *	27-sep-91	hpux should be __hpux...ljn
 */

#include "taeconf.inp"
#include "forstr.inp"		/* FORTRAN-77 string definition		*/
#include "taeintproto.h"


FUNCTION VOID BRIDGE2_NAME(xrfile) 
(
    TAEINT		*block,		/* in: parameter block		*/
    FORSTR		*name,		/* in: parameter name		*/
    TAEINT		*dimen,		/* in: dimension of string	*/
    FORSTR		*tae_file,	/* out: FOR-77 tae file string(s)	*/
    TAEINT		tae_length[],	/* out: length of each tae file string	*/
    FORSTR		*host_file,	/* out: FOR-77 host file string(s)	*/
    TAEINT		host_length[],	/* out: length of each host file string	*/
    TAEINT		*n,		/* out: number of strings	*/
    TAEINT		*status	/* out: status code		*/

 );

FUNCTION BOOL f_force_lower(BOOL flag);


/*
 *	xrfile.  Fetch file parameter.
 */

FUNCTION VOID BRIDGE1_NAME(xrfile) 
(
    TAEINT		*block,		/* in: parameter block		*/
    TEXT		*name,		/* in: parameter name		*/
    TAEINT		*dimen,		/* in: dimension of string	*/
    TEXT		*t_file,	/* out: FOR-77 tae file string(s)	*/
    TAEINT		t_length[],	/* out: length of each tae file string	*/
    TEXT		*h_file,	/* out: FOR-77 host file string(s)	*/
    TAEINT		h_length[],	/* out: length of each host file string	*/
    TAEINT		*n,		/* out: number of strings	*/
    TAEINT		*status,	/* out: status code		*/
    STRLEN		namel,
    STRLEN		t_filel,
    STRLEN		h_filel

 )
    {
	FORSTR	named;
	FORSTR	t_filed;
	FORSTR	h_filed;

	named.length = GETLEN (namel);
	named.pointer = name;

	t_filed.length = GETLEN (t_filel);
	t_filed.pointer = t_file;

	h_filed.length = GETLEN (h_filel);
	h_filed.pointer = h_file;

	BRIDGE2_NAME(xrfile) (block, &named, dimen, &t_filed, t_length,
		&h_filed, h_length, n, status);

    return;
    }

#ifndef __hpux
/*
 *      f_force_lower -- Set/Clear lowercase filespecs determination
 *      See $TGENLIB/file.np.c
 *
 *      TBD: If FORTRAN bindings are ever supported on HP-UX, then fix.
 */

FUNCTION BOOL BRIDGE1_NAME(f_force_lower)
	(TAEINT	*flag)
        {
                return (BRIDGE2_NAME(f_force_lower) (*flag));
        }
#endif
