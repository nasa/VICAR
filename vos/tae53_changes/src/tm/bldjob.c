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
 *	Portable modules for batch processing
 *	-------------------------------------
 *
 *	CHANGE LOG:
 *
 *	26-dec-83	Fix for unix portability
 *	17-feb-84	Fix CURRENT_VAL macro definition, 
 *			exclude $MENUS from being passed to batch...dm
 *	22-mar-84	Use LET to pass values for user defined globals...dm
 *	04-may-84	VALUE_x to xVAL...ces
 *	03-aug-84	New batch/async activation strategy...palm
 *	20-aug-84	Add subcommand to batch/async command line...palm
 *	27-sep-84	Change bld_tcl to always do SLOGON & not use 
 *			LET's in async
 *	16-oct-84	Change for internal procs
 *	02-nov-84	Updates for RCJM (remote) job processing...dm
 *	09-dec-84	Remove remote agent job file building to a 
 *			separate function bld_remtcl...dm
 *	07-jan-85	Add asterisked line before proc execution command
 *			line for readability...dm
 *	21-feb-85	Add same line to log file...nhe
 *
 *********************************************************************
 * CHANGES MADE IN THE RCJM TREE:
 *
 *	06-feb-85	Update for no remote agent architecture...dm
 *	20-feb-85	Remove logoff command from remote job file...dm
 *	19-mar-85	RESTORE cmd in job file only if PST file exists...dm
 *	26-mar-85	Assume remote context file always as a parfile...dm
 *	26-apr-85	Modify bld_remtcl calling sequence ...dm
 **********************************************************************
 *
 *	11-sep-86	Do it properly this time...nhe
 *	08-may-87	PR1181: change hardcoded filenames to lowercase...ljn
 *	24-mar-88	Delete TAE_RCJM conditionals...ljn
 *	14-feb-89	Do not generate TCL delete for job and jen if
 *			in norun mode...palm
 *	01-may-89	add job name to job file...palm 
 *	03-may-89	new job_ins1 installation exit...palm 
 *	23-may-90	No more RCJM...ljn
 */

#include	"taeconf.inp"	/* TAE configuration definitions	    */
#include	"tminc.inc"	/* TM-only host-independent definitions	    */
#include	"tmhost.inp"	/* TM host-dependent			    */
#include	"symtab.inc"	/* symbol table definitions		    */
#include	"parblk.inc"	/* parameter block definitions		    */
#include "taeintproto.h"




/*	bld_tcl.   Builds TCL portion of
 *	.job file for batch and async jobs.
 *
 *	The caller has already opened the job file.
 *
 */
FUNCTION CODE bld_tcl 
(
    struct CONTXT	*cmdctx,	/* in: command context 		*/
    struct SFILE	*job,		/* in/out: SFILE for job	*/
    FUNINT		run_type	/* in: type of job:		*/
					/*     ASYNC or BATCH		*/

 )
    {
    IMPORT struct CONTXT	primctx;
    IMPORT struct SYMTAB	glbtab;

    TEXT		jen_spec[FSPECSIZ+2];
    TEXT		job_spec[FSPECSIZ+2];
    struct FSBLOCK	jen_fsblock;
    struct FSBLOCK	locpdf;
    CODE		code;
    struct	SFILE	jen;
    TEXT		proc_cmd[STRINGSIZ+1];
    TEXT		errstr[STRINGSIZ+1];
    struct VARIABLE	*rt;
    BOOL		deleteFiles ;	/* when RUNTYPE=(BATCH,NORUN)	*/
					/* do not delete .jen and .job  */

    /*
    	Here, we create parameter file for parameters, level 0 locals, 
        and all globals.   The file name is the same as the
    	job file, but with a type of JEN_TYPE.   Note that
        the VMS version number ("attribute") is also the same
 	to prevent racing with other users logged into the
        current account.
    */

    deleteFiles = TRUE;			/* assume .jen and .job should go */
    if (run_type == ASYNC)
	f_write (job, "ASYNC   !tell TAE Monitor the mode");
    else
	{
        rt = lookex(&(*cmdctx).qualst, "RUNTYPE");
	if (rt && (*rt).v_count >= 2 && s_equal(SVAL(*rt,1), "NORUN"))
	    deleteFiles = FALSE;
	f_write (job, "BATCH   !tell TAE Monitor the mode");
	}
    f_write (job, (*cmdctx).pdf.name);		/* tell job name */
    f_write (job, "slogon");
    f_crack ((*job).full_spec, "", "", "", &jen_fsblock, errstr);
    s_copy (JEN_TYPE, jen_fsblock.type);
    code = f_opnblk (&jen, SAVELUN, &jen_fsblock, F_OVER);
    if (code != SUCCESS)
        {
	tmmsg (PROCFAIL, "Unable to create parameter file. %s.", 
	       "TAE-PARCREATE", (uintptr_t) jen.errmsg, 0, 0, 0, 0);
	return (code);
	}
    code = save_pfile (&jen, 0, NULL, 0, &(*cmdctx).parmst,
		       &primctx.locst,
		       &glbtab);
    if (code != SUCCESS)
        {
	tmmsg (PROCFAIL, "Unable to create parameter file. %s.",
	       "TAE-PARCREATE", (uintptr_t) jen.errmsg, 0, 0, 0, 0);
	f_close (&jen, F_KEEP);
	return (code);
	}
    f_close (&jen, F_KEEP);		       
    /*
    	Write TCL commands to job file for restoration of the
        current level 0 environment and execution of the
        target command.
    */
    s_copy ("\t", jen_spec);			/* tab before...	*/
    s_append (jen.full_spec, jen_spec);		/* PAR file spec	*/
    f_write (job, "RESTORE-GLOBAL +	!restore global environment");
    f_write (job, jen_spec);
    f_write (job, "RESTORE-LOCAL +	!restore level 0 locals");
    f_write (job, jen_spec);
    f_write (job, "DISPLAY  !display level 0 locals and globals");
    f_write (job, "DISPLAY-PARFILE CLASS=PARMS FILE=+   !display parameters");
    f_write (job, jen_spec);
    job_ins1 (job, cmdctx);			/* installation exit	*/
    /*
        Build and write the proc execution line.
    */
    f_write (job, " ");				/* spacer line		*/
    f_write(job, 
        "write \"************* Execute The Proc ********************  \"");
    MOVE_STRUCT ((*cmdctx).pdf, locpdf);
    if (s_equal(locpdf.libr, "/LOCAL/")) 	/* proc is internal	*/
    	locpdf.libr[0] = EOS;
    f_spec (&locpdf, proc_cmd);			/* full PDF spec	*/
    if (!NULLSTR((*cmdctx).subcmd))		/* if subcmd needed	*/
        {
	s_append ("-", proc_cmd);
	s_append ((*cmdctx).subcmd, proc_cmd);
	}
    s_append ("| RESTORE = ", proc_cmd);
    s_append (jen.full_spec, proc_cmd);
    s_append (" |", proc_cmd);
    f_write (job, proc_cmd);
    /*
	Wrapup, cleanup, housekeep, and miscellaneous job administration
    */
    f_write (job, " ");				/* spacer line		*/
    f_write (job, "DISPLAY ($SFI, $SKEY)	!show proc status");
    f_write (job, "LOCAL _SFI INTEGER INITIAL=&$SFI ! remember return code");
    if (run_type == ASYNC)
	f_write (job, "EMIT $SFI=&_SFI		!send final status");
    if (deleteFiles)
	{
	f_write (job, DELETE_CMD);		/* "DCL DELETE +"	*/
	f_write (job, jen_spec);		/* delete .jen file	*/
	f_write (job, DELETE_CMD);		/* "DCL DELETE +"	*/
	s_copy ("\t", job_spec);
	s_append ((*job).full_spec, job_spec);
	f_write (job, job_spec);		/* delete .job file	*/
	}
    f_write (job, "LOGOFF $SFI=&_SFI  ! return proc's $sfi to host");
    return (SUCCESS);
    }
