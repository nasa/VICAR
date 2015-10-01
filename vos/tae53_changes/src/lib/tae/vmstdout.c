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



/*	Vm_OpenStdout - open stdout for TAE 'c' application programs.
 *
 *	CHANGE LOG:
 *
 *      16-dec-87 	Created...tpl
 *	07-feb-89	Conditionalize s_lower() for UNIX; rename to .c ...ljn
 *	22-jul-92	PR1519: Label as CLASSIC...kbs
 *
 */
#include	"taeconf.inp"
#include	"terminc.inc"
#include	"parblk.inc"
#include	"tmhost.inp"
#include        "vminc.inc"
#include	<stdio.h>
#include "taeintproto.h"

   
/*
 */
    CLASSIC_FUNCTION  FILE * Vm_OpenStdout (

    Id	h			    /* in: parblk handle */
    )

    {
    IMPORT  FILE	*stdo_file;	/* pointer to stdout file	*/
    IMPORT  BOOL	term_std;	/* true if terminal is stdout   */
    IMPORT  CODE	applic_type;	/* application type ('c')	*/
    IMPORT  TEXT	pm_type[], pk_type[];
    struct  VARIABLE	*v;
    CODE		termtype;
    COUNT		termlines;
    COUNT		termcols;
    TEXT		msgbuf[STRINGSIZ+1];
    TEXT		**stdr_ptr = 0;	/* stdrec string vector pointer	*/
    COUNT		len;
    TEXT		filemode[2];	/* create or append		*/
    FILE		*stdo_ptr;
    struct    VM_STRUCT *vm;

    vm = (struct VM_STRUCT *)h;

    t_init(&termlines, &termcols, &termtype);	/* initialize terminal pkg */

    Vm_Call (h);				/* application init	   */

    v = Vm_Find(h, "_STDOUT");		        /* get the string	   */
    if ( v == NULL )
      x_error( (*vm).npblk.mode,pm_type, pk_type, (uintptr_t) "_STDOUT", 0, 0);
    else if ((*v).v_type != V_STRING)
      x_error( (*vm).npblk.mode,pm_type, pk_type, (uintptr_t) "_STDOUT", 0, 0);
    else
        stdr_ptr = (TEXT **) (*v).v_cvp;	/* get value pointer	   */

    term_std = (s_equal(TERMINAL, stdr_ptr[0]));   /* filename = terminal ? */
    s_copy ( "w", filemode );
    if (!s_equal(stdr_ptr[1] , "CREATE"))
	 s_copy ( "a", filemode );
#ifdef UNIX
    s_lower ( stdr_ptr[0] );
#endif
    stdo_ptr = fopen(stdr_ptr[0], filemode);    	/* open the file */
    if (stdo_ptr == NULL)
	{
	if ((*vm).npblk.mode == P_ABORT)
	    {
	    len = s_copy("Could not open standard output file ", msgbuf);
	    s_bcopy(stdr_ptr[0], &msgbuf[len], STRINGSIZ);
	    t_write(msgbuf, T_STDCC);			/* errmsg to terminal*/
	    z_exit(-1, "TAE-STDOPEN");			/* set SFI, SKEY     */
 	    }
	}
    else
	stdo_file = stdo_ptr;			/* save stdout ptr globally */

    applic_type = C_TYPE;			/* 'c' language application */

    return (stdo_ptr); 
    }
