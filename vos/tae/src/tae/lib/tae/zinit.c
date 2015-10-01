/******************************************************************************
 *	Copyright (c) 1990, 1991, 1992,
 *	National Aeronautics and Space Administration
 *	ALL RIGHTS RESERVED
 *
 *	The software (programs, data bases and/or documentation) on or in
 *	any media can not be reproduced, disclosed or used except under
 *	terms of the license between COSMIC and your organization.
 *****************************************************************************/

/*	zinit. Initialization call fot TAE 'c' application programs.
 *
 *
 *	CHANGE LOG:
 *	
 *	15-may-84	Create p_mput() for c applications...dm
 *	22-may-84	Make stdout name lower case ...dm
 *	11-jul-85	Fix pr #809 under UNIX...dm
 *	03-dec-86	PR737: call p_find instead of p_string...peb
 *	03-dec-86	Fix bug in t_init calling seq...peb
 *	03-dec-86	PR1138/1139: Set own application (language)
 *			type so that m_msg can be called later from
 *			p_/q_ to send err msg to stdout...peb
 *	21-jan-87	PR1184: p_inim be called with sizeof(PARBLK)...lia
 *	07-feb-89	Conditionalize s_lower() for UNIX; rename .c ...ljn
 *
 */

#include	"taeconf.inp"
#include	"terminc.inc"
#include	"parblk.inc"
#include	"tmhost.inp"
#include	<stdio.h>

    GLOBAL	v10zinit = 0;		/* source version number	*/


/*
 */
    FUNCTION  FILE *z_init (block, mode)

#if LARGE_PARBLK_FIX
    struct LARGE_PARBLK *block;         /* out: parameter block         */
#else
    struct	PARBLK	*block;		/* out: parameter block		*/
#endif
    FUNINT		mode;		/* in: mode: P_ABORT or P_CONT  */

    {
    IMPORT  FILE	*stdo_file;	/* pointer to stdout file	*/
    IMPORT  BOOL	term_std;	/* true if terminal is stdout   */
    IMPORT  CODE	applic_type;	/* application type ('c')	*/
    IMPORT  TEXT	pm_type[], pk_type[];
    struct VARIABLE	*v, *p_find();
    CODE		termtype;
    COUNT		termlines;
    COUNT		termcols;
    TEXT		msgbuf[STRINGSIZ+1];
    TEXT		**stdr_ptr;	/* stdrec string vector pointer	*/
    COUNT		len;
    CODE		code;
    TEXT		filemode[2];	/* create or append		*/
    FILE		*stdo_ptr;
#ifndef __ALPHA
    FILE		*fopen();	/* file open library routine	*/
#endif

    t_init(&termlines, &termcols, &termtype);	/* initialize terminal pkg */
#ifdef LARGE_PARBLK_FIX
    code = p_inim(block, sizeof(struct LARGE_PARBLK), mode);    
#else
    code = p_inim(block, sizeof(struct PARBLK), mode);	
#endif
                                                /* receive parblk from TM  */
    if (code != SUCCESS)
	return (NULL);
    z_call (block);				/* application init	   */

    v = p_find (block, "_STDOUT");		/* get the string	*/	
    if ((*v).v_type != V_STRING)
	x_error ((*block).mode, pm_type, pk_type, "_STDOUT");
    else
	stdr_ptr = (TEXT **) (*v).v_cvp;	/* get value pointer	*/
    term_std = (s_equal(TERMINAL, stdr_ptr[0]));   /* filename = terminal ? */
#ifdef VICAR_BATCH_LOG
#ifdef VAX_VMS
    if (s_equal("SYS$OUTPUT:.;", stdr_ptr[0]))  /* alternate form of terminal */
        term_std = TRUE;
#endif
#endif
    s_copy("w", filemode);			/* assume create	    */
    if (!s_equal(stdr_ptr[1] , "CREATE"))
	s_copy("a", filemode);
#ifdef UNIX
    s_lower(stdr_ptr[0]);			/* make file name lower case */
#endif
    stdo_ptr = fopen(stdr_ptr[0], filemode);    	/* open the file */
    if (stdo_ptr == NULL)
	{
	if (mode == P_ABORT)
	    {
	    len = s_copy("Could not open standard output file ", msgbuf);
	    s_bcopy(stdr_ptr[0], &msgbuf[len], STRINGSIZ);
	    t_write(msgbuf, T_STDCC);			/* errmsg to terminal*/
	    z_exit(-1, "TAE-STDOPEN");			/* set SFI, SKEY     */
 	    }
	stdo_ptr = stdout;	/* Allow running with no terminal! */
				/* (without triggering shell-vicar) */
	}
    else
	stdo_file = stdo_ptr;			/* save stdout ptr globally */
    applic_type = C_TYPE;			/* 'c' language application */
    return (stdo_ptr);
    }
