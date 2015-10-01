/******************************************************************************
 *	Copyright (c) 1990, 1991, 1992,
 *	National Aeronautics and Space Administration
 *	ALL RIGHTS RESERVED
 *
 *	The software (programs, data bases and/or documentation) on or in
 *	any media can not be reproduced, disclosed or used except under
 *	terms of the license between COSMIC and your organization.
 *****************************************************************************/

/*
 *	Installation Exits.  Note that the installation exit for file
 *	verification has been moved to the fileins.c source module.
 *
 */


#include	"stdh.inp"	/* system standard  (REQUIRED)		*/
#include	"taeconf.inp"	/* TAE configuration (REQUIRED)		*/
#include	"fileinc.inp"	/* file package				*/
#include	"parblk.inc"	/* parameter block definitions		*/
#include	"symtab.inc"	/* TM symbol table			*/
#include	"tmhost.inp"	/* TM host-dependent definitions	*/
#include	"tminc.inc"	/* TM definitions			*/

#include	"vicartae.inc"	/* VICAR-specific definitions		*/

#if INS_USE_TAPE

/************************************************************************/
/* TCL globals for tape handling:					*/
/*									*/
/* $TAPES, $TFILE, and $TREC are columns of a table for allocated	*/
/* and/or mounted tapes.  Each element of $TAPES is of the form		*/
/* "symbolicname=devicename".  $TFILE is the current physical position	*/
/* (file number), zero if position unknown, and -1 if the tape is not	*/
/* mounted.  $TREC is the record position within the file (starts at 1).*/
/*									*/
/* The globals are implicitly passed to every process so the process	*/
/* knows the tape position.  The globals are returned as output values	*/
/* from each process automatically by the VICAR RTL.			*/
/*									*/
/* At BOT, $TFILE and $TREC are both 1.					*/
/************************************************************************/

GLOBAL struct VARIABLE *tapes_gbl;	/* $TAPES */
GLOBAL struct VARIABLE *tfile_gbl;	/* $TFILE */
GLOBAL struct VARIABLE *trec_gbl;	/* $TREC */

#endif

#if INS_USE_AUTOUSAGE
GLOBAL struct VARIABLE *autousage_gbl;	/* $AUTOUSAGE */
#endif

/*
 *  Change log
 *
 *	11-jul-83	Purged change log, deleted checkout records,
 *			and audited global definitions...jtm
 *	23-aug-83	Add dp_ins()...peb
 *	26-aug-83	Change to comment in dp_ins()...peb
 *	07-sep-83	Added batchgbl_ins()...dm
 *	24-sep-83	Update dp_ins calling sequence...peb
 *	24-sep-83	Added ds_ins()...peb
 *	26-sep-83	Removed ds_ins; new dp_ins arguments...palm
 *	26-sep-83	New gp_ins...palm
 *	02-feb-84	Add job_ins()...peb
 *	22-jul-87	Add get_commands_ins() as part of effort to force TM 
 *			into libraries...ljn
 *	09-aug-87	Make cmd table GLOBAL; see explanation in
 *			intrinsic.c...palm
 *	15-feb-89	New job_ins1...palm
 *	01-may-89	new set_process_name stub....palm
 *	28-jun-90	Removed get_commands_ins()...ljn
 */

    GLOBAL	vexits19 = 0;

/*

At strategic places in TM, function calls are made to allow for 
installation-specific logic in TM.EXE.  These functions are called 
"installation exits".

The source module, EXITS.C, defines the standard TAE processing for
the installation exits.  EXITS.C will be delivered with the standard
TAE distribution; all include files will also have to be delivered so
that EXITS.C may be compiled.  (Compiling EXITS.C is not a requirement
for installing TAE because we do not require existence of a C
compiler.) 

An installation may re-LINK TM, replacing EXITS.OBJ with 
installation-specific code.

In the function descriptions below, the TM function that performs the
call is shown in brackets. 

In addition to writing installation-specific code for the exits,
new TCL commands may be defined.   Also, to assist in writing 
installation-specific code, a new field, ins_context, has been
reserved in the CONTXT block.

*/

/*
 *	init_ins.  TM initialization exit.

This call is made after the greeting message, after the initialization 
of *primctx, and after definition of implicit global variables.
The call is made before the SLOGON proc is executed.

If a fatal error is detected in init_ins, the function should diplay 
an error message and terminate TM.

 */

    FUNCTION VOID init_ins (primctx)

    struct CONTXT *primctx;	/* in/out: primary level context  */
    {

#define DEF_GLOBAL(gblptr, a1, a2, a3, a4, a5, a6, a7, a8, a9)	\
    if ((gblptr = defglobal(a1, a2, a3, a4, a5, a6, a7, a8, a9)) == NULL) \
	goto error

    IMPORT struct VARIABLE *defglobal();

#if INS_USE_PRSTYLE		/* Prompt style: define valids */
    static TEXT *dynamic[] = {"PROMPT"};		/* initial setting */
    static TEXT *dyn_valid[] = {"PROMPT", "STANDARD"};
    struct VARIABLE *dummy;
#endif

#if INS_USE_AUTOUSAGE		/* Resource monitor: define valids */
    static TEXT *autousage[] = {"BATCH"};		/* initial setting */
    static TEXT *autou_valid[] = {"ALL", "BATCH", "NONE"};
#endif

#if INS_USE_PRSTYLE		/* Prompt style: define global variable */
    DEF_GLOBAL(dummy,
	"$DYNAMIC", V_STRING, 8, 1, 1, 1, (GENPTR)dynamic, dyn_valid, 2);
#endif

#if INS_USE_AUTOUSAGE		/* Resource monitor: define global variable */
    DEF_GLOBAL(autousage_gbl,
	"$AUTOUSAGE", V_STRING, 5, 1, 1, 1, (GENPTR)autousage, autou_valid, 3);
#endif

#if INS_USE_TAPE		/* Tape support: define global variables */
    DEF_GLOBAL(tapes_gbl,
	"$TAPES",V_STRING,TAPENAMESIZE+TAPEDEVSIZE+1,0,MAXTAPES,0,NULL,NULL,0);

    DEF_GLOBAL(tfile_gbl,
	"$TFILE", V_INTEGER,		   0, 0, MAXTAPES, 0, NULL, NULL, 0);

    DEF_GLOBAL(trec_gbl,
	"$TREC",  V_INTEGER,		   0, 0, MAXTAPES, 0, NULL, NULL, 0);
#endif

#if INS_USE_RM
    rm_init(primctx);			/* resource monitor init */
#endif

    return;			/* stub 			  */

error:
    tmmsg(PROCFAIL, "Failure creating VICAR-extension globals.", "TAE-NOGBL");
    return;

    }


/*
 *	pa_ins.  Proc activation user exit.

This call is made before execution of a proc (process, procedure, 
global PDF, or PARMSET PDF). The call is made for every level of 
procedure nesting.  pa_ins is only called when the PDF has been 
successfully found and the parameters have been verified.

 */

    FUNCTION VOID pa_ins (pctx)

    struct CONTXT *pctx;	/* in/out: context of proc	*/
    {

#if INS_USE_RM
    rm_pa(pctx);                /* resource monitor */
#endif

    return;			/* stub				*/
    }

/*
 *	pt_ins.  Proc termination exit.

pt_ins is called upon proc termination and before clsctx is executed 
against the CONTXT block. 

 */

    FUNCTION VOID pt_ins (pctx)

    struct CONTXT *pctx;	/* in/out: context of proc	*/
    {
#if INS_USE_RM
    rm_pt(pctx);                /* resource monitor */
#endif
    return;
    }

/*	dynamic_ins.  Dynamic message processing.
 *
dynamic_ins is called when an unrecognized PARBLK is received by TM 
from an executing process.  An unrecognized PARBLK is one with 
PARBLK.msgtyp set to a value other than the defined "M_" codes in the
PARBLK include file.
 *
 */
    FUNCTION VOID dynamic_ins (pctx, parblk)

    struct CONTXT	*pctx;		/* in/out: process CONTXT */
    struct PARBLK	*parblk;	/* in: PARBLK from process*/

    {
    tmmsg(PROCFAIL, "Unrecognized message (%d) from application process.",
	  "TAE-BADMSG", (*parblk).msgtyp);
    return;	
    }

/*
 *	dp_ins.  Called by dynget before processing a dynamic parameter
 *	request (either from an application process or a GETPAR statement
 *	in a procedure).  The context block passed in here is the block
 *	to apply the user's parameters against.  
 *
 *	The dummy function does no processing, but installation specific
 *	code could get the requested parameters from the user in a way not
 *	normally supported by TM.  This function should return DP_NOTDONE
 *	unless it has completed the dynamic parameter request.  DP_NOTDONE will
 *	cause dynget to complete the parameter request itself. If dp_ins has
 *	completed the request it should return DO_EXIT or DO_RUN
 *	(similar to dyncom).
 *
 *	Note that if command processing is started by dp_ins and
 *	the user EXITs, DO_EXIT should be returned as this counts as completing
 *	the parameter request.
 */

    FUNCTION CODE dp_ins (parblk, pdf, ctx, dash_present)

    struct PARBLK	*parblk;		/* in: parblk from process */
    struct SFILE	*pdf;			/* in: opened PDF	   */
    struct CONTXT	*ctx;			/* in/out: dynamic context */
    FUNINT		dash_present;		/* in: TRUE if '-' present */
    {

#if INS_USE_PRSTYLE

    IMPORT struct SYMTAB glbtab;		/* global symbol table */
    struct VARIABLE	*v;
    CODE		code;
    struct VARIABLE	*lookex();
    BOOL		s_equal();

    v = lookex(&glbtab, "$DYNAMIC");
    if (v != NULL)
	if (s_equal(SVAL(*v, 0), "PROMPT"))	/* if $DYNAMIC == "PROMPT" */
	    {
	    do
		{
		code = psdyn(parblk, pdf, ctx, dash_present);
		} while (code == SUCCESS);
	    return(code);
	    }

#endif

    return(DP_NOTDONE);
    }

/*	gp_ins.	   Called for getpar command.
 */
    FUNCTION CODE gp_ins (pdf, ctx)

    struct SFILE	*pdf;			/* in: SFILE for opened pdf*/
    struct CONTXT	*ctx;			/* in/out: dynamic context */

    {

#if INS_USE_PRSTYLE

    IMPORT struct SYMTAB glbtab;		/* global symbol table */
    struct VARIABLE	*v;
    CODE		code;
    struct VARIABLE	*lookex();
    BOOL		s_equal();

    v = lookex(&glbtab, "$DYNAMIC");
    if (v != NULL)
	if (s_equal(SVAL(*v, 0), "PROMPT"))	/* if $DYNAMIC == "PROMPT" */
	    {
	    code = psparm(pdf, ctx, "", "");  /* get parms using prompt style */
	    return(code);
	    }

#endif

    return (DP_NOTDONE);
    }

/*
 *	term_ins.   Called by exit_do and logoff_do after the
 *	proc execution and just before the image exit
 */
    
    FUNCTION VOID term_ins()

    {

#if INS_USE_RM
    rm_term();                  /* resource monitor */
#endif

    return;
    }

/*
 *	refs_ins.   Called when implicit references are being added
 *	to a CONTXT block.  This function allows installation globals
 *	to be implicitly referenced (just like $SFI is).
 *
 *	To add a reference:
 *		addref(pctx, "global-name");
 */

    FUNCTION VOID refs_ins(pctx)

    struct CONTXT	*pctx;		/* in/out: context block	*/

    {

#if INS_USE_TAPE

    /* don't worry about refs array overflow... we are initializing and	*/
    /* MAXREF is large enough to cover all implicits.			*/

    (*pctx).refs[((*pctx).numrefs)++] = tapes_gbl;	/* $TAPES */
    (*pctx).refs[((*pctx).numrefs)++] = tfile_gbl;	/* $TFILE */
    (*pctx).refs[((*pctx).numrefs)++] = trec_gbl;	/* $TREC */
    (*tapes_gbl).v_refcnt++;
    (*tfile_gbl).v_refcnt++;
    (*trec_gbl).v_refcnt++;

#endif

    return;
    }

/*
 *	bchgbl_ins. Installation-specific globals for non-batch processing.
 *	
 *		Called when the batch job file is created to check if 
 *		the global parameter should be inhibited for  batch.
 *	
 *	Return codes:   TRUE : Global variable to be inhibited for batch.
 *			FALSE: Otherwise.
 */

    FUNCTION  BOOL  bchgbl_ins(gblname)

    TEXT	gblname[];		/* IN: name of global variable	*/

    {
    static  TEXT  *instal_gbl = { 
			      "" };	/* null entry: required at the end */
    COUNT	  i;


    for (i=0; instal_gbl[i] != NULL; i++)
	if (s_equal(gblname, instal_gbl[i]))
	    return (TRUE);		/* found in list		*/
    return (FALSE);
    }

/*	job_ins.  Called from run_batch just before call to bld_cmd.
 *	This installation exit is useful to insert additional TAE commands
 *	into the job file before the invocation of the requested proc.
 *	E.g., JPL uses this to insert a LISTPROC command
 *
 *		LISTPROC procname
 *
 *	where "procname" is the text string in (*cmdctx).pdf.name.
 *
 *	This function should return SUCCESS unless an error is encountered.
 *	This function must do its own error reporting.
 */

    FUNCTION CODE job_ins (jobfile, cmdctx)

    struct SFILE	*jobfile;		/* in/out: already open job file*/
    struct CONTXT	*cmdctx;		/* in:  command context		*/

    {
    return(SUCCESS);
    }



/*	Another installation exit for job (called from bldjob.c)
 *	See notes above for job_ins.
 *
 */

    FUNCTION CODE job_ins1 (jobfile, cmdctx)

    struct SFILE	*jobfile;		/* in/out: already open job file*/
    struct CONTXT	*cmdctx;		/* in:  command context		*/

    {
    TEXT		buf[STRINGSIZ+1];
    CODE		code;

#if INS_USE_BATCH
    s_copy("LISTPROC ", buf);
    s_append((*cmdctx).pdf.name, buf);
    code = f_write(jobfile, "");
    if (code == SUCCESS) code = f_write(jobfile, "");
    if (code == SUCCESS) code = f_write(jobfile, buf);
    if (code == SUCCESS) code = f_write(jobfile, "");
    if (code != SUCCESS)
	{
	tmmsg(PROCFAIL, "Error writing to batch job file in job_ins2.");
	return(DO_CHECK);
	}
#endif
    return (SUCCESS);
    }

/*	set process name for ASYNC and BATCH
 *
 */

#if INS_USE_PNAME+INS_USE_ATTACH
#include <ssdef.h>
#include <descrip.h>
#define PNAMESIZE       15             /* max length of a VMS process name */
#endif

 FUNCTION VOID set_process_name (runtype, name)

     CODE	runtype;				/* BATCH or ASYNC */
     TEXT	name[];					/* job name	*/

{

#if INS_USE_PNAME

    int status, i;
    char pnam[20], tmpstr[80], *ptr;
    $DESCRIPTOR(p_name, pnam);
    char *strrchr();

    strncpy(pnam, name, PNAMESIZE);
    pnam[PNAMESIZE] = '\0';
    p_name.dsc$w_length = strlen(pnam);

    status = SYS$SETPRN(&p_name);

    if (strlen(pnam) > PNAMESIZE-3)
      pnam[PNAMESIZE-3] = '\0';	   /* truncate 3 chars to leave room for "_n" */

    for (strcat(pnam, "_0"), i=1; status == SS$_DUPLNAM; i++)
	{
	ptr = strrchr(pnam, '_') + 1;		/* terminate with new	   */
	sprintf(ptr,"%d",i);			/* "_n" index until unique */

	p_name.dsc$w_length = strlen(pnam);
	status = SYS$SETPRN(&p_name);
	}

    if (status != SS$_NORMAL)
	{
	sprintf(tmpstr, "Error setting process name, host code = %d", status);
	put_stdout (tmpstr);
        put_stdout ("Process name not changed");
	}

#endif

return;
}


/*
 *	Installation-Specific Commands.

A global ITRCMD table named commands_ins becomes an extension to the 
TM intrinsic command table.  Installation-specific commands may be 
defined in this table.
 
 *
 */

/************************************************************************/
/* Parameter structures first */

#if INS_USE_TAPE

#if UNIX_OS

    static struct RESIDVAR p_mount[] =
    {
"DEVICE", V_STRING, 0, 1, 1, TAPEDEVSIZE,  0, NULL, NULL,	/* required */
"NAME",   V_STRING, 0, 1, 1, TAPENAMESIZE, 0, NULL, NULL	/* required */
    };

    static struct RESIDVAR p_dismount[] =
    {
"DEVICE", V_STRING, 0, 0, 1, TAPEDEVSIZE,  0, NULL, NULL,
"NAME",   V_STRING, 0, 0, 1, TAPENAMESIZE, 0, NULL, NULL
    };

#endif

#if VMS_OS

    static struct RESIDVAR p_alloc[] =
    {
"DEVICE", V_STRING, 0,1,MAXTAPEDEVS, TAPEDEVSIZE,  0, NULL, NULL, /* required */
"NAME",   V_STRING, 0,1,1,           TAPENAMESIZE, 0, NULL, NULL  /* required */
    };

    static struct RESIDVAR p_dealloc[] =
    {
"DEVICE",  V_STRING, 0,0,1, TAPEDEVSIZE,  0, NULL,      NULL,
"NAME",    V_STRING, 0,0,1, TAPENAMESIZE, 0, NULL,      NULL
    };

    static TAEINT den_default[] = {1600};
    static TAEINT blk_default[] = {0, 0};
    static TEXT   *pro_default[] = {"READ"};
    static TEXT   *init_default[] = {"NOINIT"};

    BEGIN_VALIDS(v_protectt, 2)
	"READ", "WRITE"
    END_VALIDS

    BEGIN_VALIDS(v_init, 2)
	"NOINIT", "INIT"
    END_VALIDS

    static struct {
	COUNT vc;
	struct {
	    TAEINT low;
	    TAEINT high;
	} vr[3];
    } v_density = {3, 800,800, 1600,1600, 6250,6250};

    TEXT *null_string[] = "";
    static struct RESIDVAR p_mount[] =
    {
"DEVICE",  V_STRING, 0,0,MAXTAPEDEVS,TAPEDEVSIZE,0,NULL,         NULL,
"LABEL",   V_STRING, 0,0,MAXTAPES,   LABELSIZ,0, NULL,          NULL,
"NAME",    V_STRING, 0,0,1,          TAPENAMESIZE,0,NULL,       NULL,
"DENSITY", V_INTEGER,0,1,1,          0,  1, (GENPTR)&v_density, den_default,
"BLOCKING",V_INTEGER,0,1,2,          0,       2, NULL,          blk_default,
"COMMENT", V_STRING, 0,1,1,          78,      1, NULL,          null_string,
"PROTECT", V_STRING, 1,1,1,          5,  1, (GENPTR)&v_protectt,pro_default,
"INIT",    V_STRING, 1,1,1,          6,  1, (GENPTR)&v_init,    init_default
    };

    BEGIN_VALIDS(v_option, 2)
	"UNLOAD", "NOUNLOAD"
    END_VALIDS

    static TEXT *opt_default[] = {"UNLOAD"};

    static struct RESIDVAR p_dismount [] =
    {
"DEVICE",  V_STRING, 0,0,1, TAPEDEVSIZE,  0, NULL,              NULL,
"NAME",    V_STRING, 0,0,1, TAPENAMESIZE, 0, NULL,              NULL,
"OPTION",  V_STRING, 1,1,1, 8,            1, (GENPTR)&v_option, opt_default
    };

    static struct RESIDVAR p_rewind[] =
    {
"DEVICE",  V_STRING, 0,0,1, TAPENAMESIZE, 0, NULL, NULL
    };

#endif	/* VMS_OS */

#endif	/* INS_USE_TAPE */

#if INS_USE_FLAG

    BEGIN_VALIDS(v_flags, 14)
	"DEBUG", "LOGGING", "ADEBUG", "TUTLIB", "SYNTAX", "NOMESSAGE",
	"P0", "P1", "P2", "P3", "P4", "P5", "P6", "P7"
    END_VALIDS

    static TEXT *flags_default[] = {"LOGGING"};

    static struct RESIDVAR p_flag[] =
    {
"FLAGS",  V_STRING, 0,1,10, VALIDSIZ, 1, (GENPTR)&v_flags, (GENPTR)flags_default
    };

#endif

#if INS_USE_VRDI

    static TEXT *device_default[] = {"DEFAULT"};

    static struct RESIDVAR p_usedisp[] =
    {
"DEVICE", V_STRING, 0,0,1, VRDIDEVSIZE, 1, NULL, device_default
    };

    static struct RESIDVAR p_freedisp[] =
    {
"DEVICE", V_STRING, 0,0,1, VRDIDEVSIZE, 1, NULL, device_default
    };

#endif

#if INS_USE_MAL

    BEGIN_VALIDS(v_mal, 6)
	"BYTE", "FULL", "HALF", "REAL", "DOUBLE", "COMPLEX"
    END_VALIDS

    static TEXT *format_default[] = {"BYTE"};

    static struct RESIDVAR p_mal[] =
    {
"FILE",   V_STRING, 0,1,1, FNAMESIZ,   -1, NULL,           NULL,
"NL",     V_INTEGER,0,1,1, 0,          -1, NULL,           NULL,
"NS",     V_INTEGER,0,1,1, 0,          -1, NULL,           NULL,
"FORMAT", V_STRING, 1,1,1, 8,           1, (GENPTR)&v_mal, format_default
    };

    static struct RESIDVAR p_dal[] =
    {
"FILE",   V_STRING, 0,1,1, FNAMESIZ,   -1, NULL,           NULL
    };

#endif

#if INS_USE_LISTPROC

    static struct RESIDVAR p_listproc[] =
    {
"PROC", V_STRING, 0,1,1, FNAMESIZ+1, -1, NULL, NULL
    };

#endif

#if INS_USE_ATTACH

    static struct RESIDVAR p_attach[] =
    {
"PROCESS", V_STRING, 0,0,1, PNAMESIZ+1, 0, NULL, NULL
    };

#endif

#if INS_USE_DEFINE
    static struct RESIDVAR p_define_add[] =
    {
"NAME",  V_STRING, 0,1,1, STRINGSIZ+1, 0, NULL, NULL,
"VALUE", V_STRING, 0,1,1, STRINGSIZ+1, 0, NULL, NULL
    };

    static struct RESIDVAR p_define_delete[] =
    {
"NAME",  V_STRING, 0,1,1, STRINGSIZ+1, 0, NULL, NULL
    };
#endif

/************************************************************************/

#if INS_USE_TAPE
#if UNIX_OS
    GLOBAL CODE mount_do(), dismount_do();
#endif
#if VMS_OS
    GLOBAL CODE mount_do(), dismount_do(), alloc_do(), dealloc_do(),
	        rewind_do();
#endif
#endif

#if INS_USE_EMACS
    GLOBAL CODE emacs_do();
#endif

#if INS_USE_PRSTYLE
    GLOBAL CODE passthru_do(), nopassthru_do();
#endif

#if INS_USE_FLAG
    GLOBAL CODE flag_do();
#endif

#if INS_USE_RM
    GLOBAL CODE rm_do_usage();
#endif

#if INS_USE_VRDI
    GLOBAL CODE usedisp_do(), freedisp_do();
#endif

#if INS_USE_MAL
    GLOBAL CODE mal_do(), dal_do();
#endif

#if INS_USE_LISTPROC
    GLOBAL CODE listproc_do();
#endif

#if INS_USE_ATTACH
    GLOBAL CODE attach_do();
#endif

#if INS_USE_DEFINE
    GLOBAL CODE deflog_do(), defsym_do();
#endif

/************************************************************************/

#define Y_GS Y_GENERAL | Y_SYNTAX

	GLOBAL struct ITRCMD commands_ins[] =
    {

#if INS_USE_TAPE
{4, "MOUNT",   "",      Y_GS,      I_NPM(p_mount),    p_mount,    mount_do},
{4, "DISMOUNT","",      Y_GS,      I_NPM(p_dismount), p_dismount, dismount_do},
#if VMS_OS
{4, "ALLOC",   "",      Y_GS,      I_NPM(p_alloc),    p_alloc,    alloc_do},
{4, "DEALLOC", "",      Y_GS,      I_NPM(p_dealloc),  p_dealloc,  dealloc_do},
{3, "REWIND",  "",      Y_GS,      I_NPM(p_rewind),   p_rewind,   rewind_do},
#endif
#endif

#if INS_USE_EMACS
{2, "EMACS",   "",      Y_GS,      0,                 NULL,       emacs_do},
#endif

#if INS_USE_PRSTYLE
{3, "PASSTHRU","",      Y_GS,      0,                 NULL,       passthru_do},
{3, "NOPASSTHRU","",    Y_GS,      0,                 NULL,      nopassthru_do},
#endif

#if INS_USE_FLAG
{2, "FLAG",    "SET",   Y_GENERAL|Y_DEFSUB,
                                   I_NPM(p_flag),     p_flag,     flag_do},
{2, "FLAG",    "ADD",   Y_GENERAL, I_NPM(p_flag),     p_flag,     flag_do},
{2, "FLAG",    "DELETE",Y_GENERAL, I_NPM(p_flag),     p_flag,     flag_do},
{2, "FLAG",    "SHOW",  Y_GENERAL, 0,                 NULL,       flag_do},
#endif

#if INS_USE_RM
{3, "USAGE",   "",      Y_GENERAL, 0,                 NULL,       rm_do_usage},
{3, "USAGE",   "SHOW",  Y_GENERAL, 0,                 NULL,       rm_do_usage},
#endif

#if INS_USE_VRDI
{3, "USEDISP", "",      Y_GS,      I_NPM(p_usedisp),  p_usedisp,  usedisp_do},
{4, "FREEDISP","",      Y_GS,      I_NPM(p_freedisp), p_freedisp, freedisp_do},
#endif

#if INS_USE_MAL
{3, "MAL",     "",      Y_GS,      I_NPM(p_mal),      p_mal,      mal_do},
{3, "DAL",     "",      Y_GS,      I_NPM(p_dal),      p_dal,      dal_do},
#endif

#if INS_USE_LISTPROC
{5, "LISTPROC","",      Y_GENERAL, I_NPM(p_listproc), p_listproc, listproc_do},
#endif

#if INS_USE_ATTACH
{2, "ATTACH",  "",   Y_GS|Y_ONFAIL,I_NPM(p_attach),   p_attach,   attach_do},
#endif

#if INS_USE_DEFINE
{4, "DEFLOG",  "ADD",    Y_GENERAL|Y_DEFSUB,
			I_NPM(p_define_add),    p_define_add,    deflog_do},
{4, "DEFLOG",  "DELETE", Y_GENERAL,
			I_NPM(p_define_delete), p_define_delete, deflog_do},
{4, "DEFSYM",  "ADD",    Y_GENERAL|Y_DEFSUB,
			I_NPM(p_define_add),    p_define_add,    defsym_do},
{4, "DEFSYM",  "DELETE", Y_GENERAL,
			I_NPM(p_define_delete), p_define_delete, defsym_do},
#endif

    {0, ""}		/* terminator entry	*/
    };
