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
 * 	xzhost. 
 *
 *	CHANGE LOG:
 *
 *	11-jul-83	Purged change log, deleted checkout records,
 *			and audited global definitions...jtm
 *	08-sep-83	Changed V_NOTFILE to V_NOCHECK...palm
 *	13-sep-83	Separated XZEXIT from XZ file management functions
 *			so the file management functions can be changed
 *			for catalog manager...palm
 *	11-oct-83	Separated from xzexit...palm
 *	11-oct-83	Removed condtional from xzper, xzdash, 
 *			calls...palm
 *	19-mar-84	Add p_file...palm
 *
 */

#include	"taeconf.inp"		/* TAE configuration		*/
#include	"forstr.inp"		/* fortran-77 string struct	*/
#include 	"symtab.inc"
#include "taeintproto.h"



/* xzhost - Get host spec given TAE spec and file mode
 *
 *	We call XZSTD, XZPER, and XZDASH for different header characters:
 *	These subroutines may be in FORTRAN, so we use FORTRAN-type
 *	arguments.
 *
 */
FUNCTION VOID xzhost
(
    TEXT		tae_spec[],	/* in: tae spec, C string	*/
    TEXT		host_spec[FSPECSIZ+1],
    					/* out: host spec, C string 	*/
    TAEINT		*filemode,	/* in: filemode			*/
    TAEINT		*status	/* out: status			*/

 )
    {
#ifdef VAX_VMS
#define this_okay 0
#endif
#ifdef UNIX
#define this_okay 0
#endif

#ifdef this_okay			/* for VMS and UNIX		*/
    FORSTR	tae_for, host_for;	/* FORTRAN string descriptors	*/

    tae_for.pointer = tae_spec;
    tae_for.length = s_length(tae_spec);
    host_for.pointer = host_spec;
    host_for.length = FSPECSIZ;
    if (tae_spec[0] == '%')
    	xzper(&tae_for, &host_for, filemode, status);	/* % means host	*/
    else if (tae_spec[0] == '-')
	xzdash(&tae_for, &host_for, filemode, status);	/* special	*/
    else
    	xzstd(&tae_for, &host_for, filemode, status);	/* standard	*/
    s_for2c(&host_for, host_spec, 0);		/* convert to C string    */
    s_strip (host_spec);			/* remove trailing spaces */
    return;    
#else
    This is an intentional compilation error.  The code above is
    dependent upon the FORSTR structure and is good for VAX_VMS and
    UNIX.
#endif
    }

/*	p_file.   Convert TAE file spec to host file spec.
 *
 *	This is for the C applications programmer.
 */
FUNCTION CODE p_file 
(
    TEXT	taefile[],	/* in: TAE file spec		*/
    FUNINT	access,		/* in: V_IN, V_OUT, ...		*/
    TEXT	hostfile[]	/* out: translated file spec	*/

 )
    {
    CODE	filemode;
    TAEINT	status;
    

    filemode = access;
    xzhost (taefile, hostfile, &filemode, &status);
    return (status);
    }
