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




/********************************************************************
 *	TAE subroutine package error handling.	
 *
 *	This is in a separate module so that 
 *	a program can override TAE's outboard
 *	error handling, for example, you might
 *	want to use some of TAE's library without
 *	running under TAE and expecting to send
 *	error messages to TM.
 *
 ***************************************************************************
 * 
 * Change log:
 *
 *
 *	01-aug-88   Also handle applications running independent of tm...dm
 *
 */
#include	"taeconf.inp"
#include	"parblk.inc"
#include "syninc.inc"
#include "taeintproto.h"



/* 
 *	x_error.   Report error and abort if in P_ABORT mode; continue
 *	otherwise.
 *
 */

    FUNCTION VOID x_error(

    FUNINT	mode,			/* in: P_ABORT or P_CONT	*/
    TEXT	message[],		/* in: message text		*/
    TEXT	key[],			/* in: message key 		*/
    uintptr_t 	A1,			/* in: integer or string ptrs 	*/
    uintptr_t	A2,
    uintptr_t	A3
    )
    {
   
    TEXT	msgstring[2*STRINGSIZ+1];   /* formatted message string	*/

    if (mode == P_ABORT || ( (mode & P_MODE_ABORT) == P_MODE_ABORT ) )
	{
	if (c_child_proc())		    /* running as child of tm   */
	    {
	    parm_err(message, key, A1, A2, A3);
	    z_exit(-1, key);		/* set $sfi, $skey		*/
	    }
	else
	    /* task running independent of tm */
	    {
            sprintf(msgstring, message, A1, A2, A3);	
            if (s_length(msgstring) > STRINGSIZ)     /* allow upto STRINGSIZ  */
	        s_copy("...", &msgstring[STRINGSIZ-3]);	    /* truncate */
	    printf(msgstring);
  	    printf("\n");
	    exit(1);
	    }
	}
    else
	return;
    }
