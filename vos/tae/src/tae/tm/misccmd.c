/******************************************************************************
 *	Copyright (c) 1990, 1991, 1992,
 *	National Aeronautics and Space Administration
 *	ALL RIGHTS RESERVED
 *
 *	The software (programs, data bases and/or documentation) on or in
 *	any media can not be reproduced, disclosed or used except under
 *	terms of the license between COSMIC and your organization.
 *****************************************************************************/

/* This file contains the functions to perform miscellaneous
 * individual TAE intrinsic commands.
 * A table of associated intrinsic command structure entries,
 * and the associated resident PDFs are also in this file.
 *
 * CHANGE LOG:
 *
 *	11-jul-83	Purged change log, deleted checkout records,
 *			and audited global definitions...jtm
 *	12-jul-83	Convert LOCALs to PARMs in parm_select()...dm
 *	04-aug-83	Change delgbl_do to imitate prccmd...peb
 *	04-aug-83	Delete v_option...palm
 *	15-aug-83	Fix bug in new delgbl_do...peb
 *	16-aug-83	Add comment to delgbl_do header...peb
 *	21-aug-83	Nullables; STOP command...palm
 *	25-aug-83	getpar_do to copy local symbol table...peb
 *	30-aug-83	changed v_kiv to v_keyword...palm
 *	08-sep-83	Getpar returns $SKEY of TAE-EXIT if killed...palm
 *	14-sep-83	New getpar_do for new tutor interface...palm
 *	16-sep-83	Remove logic for defaults in parm_select...palm
 *	21-sep-83	Fix RESIDVARs for UNIX...palm
 *	22-sep-83	Add dp_ins call to getpar_do...palm
 *	26-sep-83	dp_ins changed to gp_ins...palm
 *	30-sep-83	Implement DELETE commands...dm
 *	11-oct-83	Fix unix compilations...palm
 *	18-oct-83	Fix illegal struct member, L434 (unix)...palm
 *	30-oct-83	Fix delgbl_do for subcmds, add prim_act to getpar_do...peb
 *	11-nov-83	Remove default subcommand for DELETE and make
 *			it abbreviatable...palm
 *	17-nov-83	Allow COMMAND command in procedure...palm
 *			Also: fix DELETE abbreviation...palm
 *	23-nov-83	Fix DELCMD RESIDVAR to be 10 chars (PR 603)...palm
 *	23-jan-84	Implement READ command...palm
 *			GETPAR now allows globals...palm
 *	03-feb-84	Add ABORT...nhe
 *	06-feb-84	Update Y_ flags for latest analysis...nhe
 *	15-feb-84	Add $DEFCMD1,2,3... for lots of defined commands...palm
 *	17-feb-84	De-static parm_select...nhe
 *	18-feb-84	De-compile the READ command;  fixes to $DEFCMD...palm
 *	24-feb-84	New PREFACE parm in GETPAR; new dynamic rules
 *			for GETPAR...palm
 *	28-feb-84	clean up dynamic comments...palm
 *	29-feb-84	Update GETPAR for async...nhe
 *	15-mar-84	Add arg to opn_tutor...dm
 *	15-mar-84	Make exit, logoff act right in async...nhe
 *	16-mar-84	Update call to tutor() to delete last 2 args...dm
 *	19-mar-84	Make delete-variables multi-valued...palm
 *	04-may-84	VALUE_x to xVAL ... ces
 *	04-may-84	set_value() for 1 string to set_string()...peb
 *	06-may-84	Conform to no .defalt in RESIDVAR...peb
 *	07-may-84	Clean-up IMPORT declarations...lim
 *			No more dyncmd mode...nhe
 *			'toprefs'-->'numrefs'...nhe
 *	14-jun-84	Implement default intrinsic subcommand...lim
 *	14-jun-84	ABORT-BATCH; DELETE-COMMAND made same as DELCMD...palm
 *	25-jun-84	PR738: fix v_pdf for $DEFCMDi...palm
 *	27-jun-84	TCL-41B: options=interrupt ...palm
 *	01-aug-84	file_name-->file_spec in struct MON_MSG...nhe
 *	02-aug-84	"Terminal Monitor" --> "TAE Monitor" (PR396)...peb
 *	06-aug-84	New logic for calling async_exit
 *			from exit_do and logoff_do...palm
 *	16-aug-84	Set flag inbody of current proc's CONTXT(PR#628)...lia
 *	16-aug-84	pr396: Consolidate I/O error keys...peb
 *	24-aug-84	Change dynamic tutor request to NOFORCE...lia
 *	09-sep-84	Complain if "BODY" in a process...nhe
 *	27-sep-84	Do SLOGOFF in async...nhe
 *	11-oct-84	Put conditional c_cndp/c_snpa call and other bug fixes
 *			as implemented for UNIX 1.2...dm
 *	12-oct-84	Conditional compilation of all async calls
 *			(as for UNIX 1.2)...dm
 *	14-oct-84	Add internal procs...nhe
 *	17-oct-84	PR870: Add subcommand name to _PROC value set by
 *			get_parms_remote to send to parent monitor...jtm
 *	17-oct-84	TCL 117: process_do, etc. to set ctx.compiled...peb
 *	18-oct-84	PR715: Change movest call in getpar_do to plcini...jtm
 *	19-oct-84	Add internal procs in body
 *	23-oct-84	TCL 117: subcmd_do logic for compilation...peb
 *	29-oct-84	Call RSLOGOFF pdf if executing at remote node ...dm
 *	09-nov-84	PR 888: Allow $SKEY in return to be MAXSTRSIZ...palm
 *	09-nov-84	TCL 78: Inherhent batch, async, inter declaration...nhe
 *	07-dec-84	PR 908: eliminate extra error msg displayed...lia
 *	12-dec-84	TCL 97: save line number of internal proc call...lia
 *	17-dec-84	TCL 67: Audit NAMESIZ: replace by F_Q_NAMESIZ? ...peb
 *	19-dec-84	Shorten error message for onfail cmd...lia
 *	26-dec-84	Withdraw TCL 78...nhe
 *	27-dec-84	Update exit_do and logoff_do for RCJM's looping
 *			remote tm...dm
 *	28-dec-84	PR 905: add extra check for request of delete on local
 *			variables...lia
 *	04-jan-85	De-STATIC del-gbl/loc...nhe
 *	14-jan-85	Fix getpar_do to clear ipbptr in initializing ctx...nhe
 *
 *****************************************************************************
 * CHANGES  MADE IN THE RCJM TREE
 *	17-jan-85	Implement INCLUDE to get input commands from file...dm
 *	07-feb-85	Update exit_do and logoff_do for remote log off...dm
 *	13-mar-85	New MON_MSG member names...dm
 *	06-apr-85	Update get_parm_remote for RCJM case handling; delete
 *			reference to ASYNCINC...dm
 *	26-may-85	Delete INCLUDE command...dm
 *
 ****************************************************************************
 * MERGE WITH THE FOLLOWING TAE-V1.3 CHANGES...dm (26-may-85)
 *
 *	23-jan-84	Fix getpar_do to return error on batch...nhe
 *	12-feb-85	Fix delcmd for multiple cmds...joh
 *	10-apr-85       PR625: Add message for attempt to delete implicit locals
 *  			...joh
 *	08-may-85	Change dimension for procspec in get_parms_remote...dm
 *	19-jun-85	Fix warning error with new C compiler V2.0...lia
 *
 ****************************************************************************
 *
 *	01-jul-85	Fix UNIX compilation error...dm
 *	08-jul-85	Conditional compilation of ABORT-BATCH...dm
 *	25-jul-85	Fix UNIX lint errors...dm
 *	30-jul-85	Optimize cmdmatch (and consequently delete 
 *			cmd_prs)...palm
 *	30-jul-85	Fix bug in cmdmatch...palm
 *	26-aug-85	PR 806: get_parms_remote and getpar_do check for exit 
 *			from dyn tutor (see get_parm_locpar)...dab
 *	29-aug-85	PR 967: modified getpar_do() to save parm qualifier 
 *			values...lia
 *	09-sep-85	PR 961: changed getpar-do to set context block sent to
 *			tutor to indicate tutoring for synchronous proc...dab
 *	11-sep-85	PR 871: fix getpar_do and get_parms_remote to handle
 *			multiple preface lines...lia
 *	17-feb-86	new OPTIONS=INTERRUPT for PROCEDURE command.
 *			EXIT now honors host_enable flag.
 *	16-jul-86	New ACTION= for PARMSETs...palm
 *	16-jul-86	Allow "GLOBALS" for "GLOBAL" command while maintaining 
 *			backwards compatibility, allowing 6 chars...palm
 *
 *************************************************************************
 *
 * RETROFITTING UNIX CHANGES SINCE V1.3
 *
 *	13-nov-86	Made 12-sep-85 under UNIX: Call term_async()
 *			to cleanup at exit time...pb for dm
 *
 *************************************************************************
 *	22-dec-86	TAE Facelift changes (call to w_exit)...krw
 *	08-may-87	PR1181: "RSLOGOFF" and "SLOGOFF" and "ABORT" (Y_PDF) 
 *				to lower...ljn
 *	13-jul-87	Allow GLOBALS proc as local...palm
 *	13-jul-87	Delete some code that checks tae_alloc return for
 *			NULL: tae_alloc now aborts on no memory...palm
 *	20-jul-87	Fix internal error generated by "DELETE X.Q"...palm
 *	22-jul-87	Add get_misccmd () as part of effort to force TM 
 *			into libraries...ljn
 *	09-aug-87	Make cmd table GLOBAL; see explanation in
 *			intrinsic.c...palm
 *      25-sep-87       retrofit UNIX changes in exit_do and logoff_do
 *                      ( call disable_recvar() ) ...tpl
 *	24-Feb-88	PR1504: change label arg in cmd_parse call to NULL...ljn
 *      01-Mar-88       Search for _VIEWPDF in getpar_do...tpl
 *	24-mar-88	Apollo requires braces on static array initializers...ln
 *	24-mar-88	Delete TAE_RCJM conditionals...ljn
 *	19-may-88	New DEFCMD-REPLACE to allow replacing an existing 
 *			command without complaints...palm
 *	28-nov-88	New options=selftutor...palm
 *	08-dec-88	New NO_SELFTUTOR in $TUTOPT to suppress selftutor...palm
 *	09-dec-88	New COMPRESSTUTOR in OPTIONS...palm
 *	26-jan-89	new valid frmat; MAXVAL -> INT_MAXVAL in RESIDVAR...palm
 *	03-feb-89	New NOP command...palm
 *	03-feb-89	New LOGOFF command with $SFI that becomes the
 *			image exit code if present...palm
 *	01-may-89	new strategy for checking cmd_mode in exit_do and
 *			logoff_do...palm 
 *	12-jun-89	Removed TAE_FACELIFT...ljn
 *	23-may-90	Removed RCJM stuff referring to old TAE_RCJM...ljn
 *	28-jun-90	Removed get_misccmd()...ljn
 *	01-aug-91	Braces for static string initializers...ljn
 *	22-oct-92	Prototype of tae_alloc unnecessary and Ultrix 4.3 does
 *			not like it...rt
 *      30-apr-93       Made reference to s_table to be noshare for VMS
 *                      because variable is referenced by DEC C++ as noshare.
 *                      taeconf.inp defines noshare appropriately for each 
 *                      platform.
 *	6-oct-94	Changed logoff command to do the same thing as exit
 *			i.e. will not terminate vms session
 *
 */

#include	"taeconf.inp"		/* TAE configuration definitions	*/
#include	"eventinc.inp"		/* event package */
#include	"fileinc.inp"		/* file & file position context structs	*/
#include	"syninc.inc"		/* syntax package structure and defs	*/
#include	"tmhost.inp"		/* host-dependent defs			*/
#include	"symtab.inc"		/* symbol table				*/
#include	"tminc.inc"		/* TM-only host-independent definitions	*/
#include	"sessinc.inc"		/* session log  defs and struct		*/
#include 	"pgminc.inc"		/* for M_ codes				*/


    GLOBAL	v213misc = 0;		/* source version			*/
                    
    static CODE		saved_search;   /* for saving search types		*/
    static COUNT	saved_line;	/* for saving search cmd line #		*/
    static BOOL		saved_onfail;	/* for saving search onfail cmd		*/
    					/* we can get away with the static here 	*/
    					/* because contxt doesn't change in search */

    BOOL		s_equal();	/* strings equal (case insignificant)?	*/
    BOOL		s_lseq();	/* 1st string left substring of 2nd?	*/
    struct VARIABLE	*lookex();	/* find a particular parm in sym tab	*/
    TEXT		*s_save();	/* put string in dynamic memory		*/
    struct VARIABLE	*allvar();	/* allocate variable			*/
    GENPTR		allval();	/* allocate value in variable		*/
    struct VARIABLE	*search();	/* search for a variab among all symbols def'd in this proc*/
    struct VARIABLE	*usearch ();	/* unresolved search			*/
    struct SUBCMD	*allsub();	/* add SUBCMD struct to chain		*/

/* Memory-resident PDF data for commands performed in this file.
 * The parameters for each command are defined
 * by a vector of RESIDVAR structures.
 * The RESIDVAR structure is explained in the TMINC include file.
 */

    static TEXT	*null_string[] = {""};	/* gp value vector of null	*/
    static TAEINT  val_one[]= {1};


    static struct RESIDVAR ptabo[] =	/* JOB for abort		*/
	{
/* name    type      k m maxc             size     dc val      dvp*/

"JOB",    V_STRING, 0, 1, INT_MAXVAL, JOBNAMESIZ,  -1, NULL,  NULL
	};

    static struct RESIDVAR ptdefc[] =
	{				/* Note: COMMAND name includes '*' */
/* name    type      k  m maxc    size        dc  val      dvp*/

"COMMAND", V_STRING, 0, 1, 1,    FNAMESIZ+1,  -1, NULL,    NULL,
"STRING",  V_STRING, 0, 1, 1,    STRINGSIZ-4-FNAMESIZ,
						-1, NULL,  NULL
	};


    static struct RESIDVAR ptdelc[] =
	{
	/* name    type      k  m maxc               size     dc val      dvp*/

	"COMMAND", V_STRING, 0, 1, INT_MAXVAL,  FNAMESIZ+1,   -1, NULL,    NULL
	};

    static struct RESIDVAR ptdelg[] =
	{
	/* name    type      k  m maxc    size     dc val      dvp*/

	"PROC",    V_STRING, 0, 1, 1,    FSPECSIZ,   -1, NULL,    NULL
	};

    static  struct RESIDVAR  ptdelv[] =		/* delete a variables	  */
	{
	/* name		type	k  minc  maxc	          size	 dc  val  dvp */
	
	"NAME",     V_STRING,   0, 1,   INT_MAXVAL, F_Q_NAMESIZ, -1,NULL, NULL
	};

    static struct RESIDVAR ptgetp[] =
	{
	/* name    type      k  m maxc    size     dc val      dvp*/

	"PARMS",    V_STRING, 0,1,INT_MAXVAL,
					  F_Q_NAMESIZ,-1,NULL, NULL,
	"PREFACE",  V_STRING, 0,0,    12,MAXSTRSIZ,0, NULL,   NULL
	};


    static TEXT	*interrupt[] = {"INTERRUPT", "NOSELFTUTOR"};	

    BEGIN_VALIDS(opt_valid, 5)
        "INTERRUPT", "NOINTERRUPT", "SELFTUTOR", "NOSELFTUTOR", "COMPRESSTUTOR"
    END_VALIDS


    static struct RESIDVAR ptglbl[] =	/* GLOBAL PDF			*/
	{
/* name    type      k  m maxc      size     dc val      dvp*/

"HELP",    V_STRING, 0, 1, 1,      FSPECSIZ, 1, NULL,    (GENPTR)null_string,
"NAME",    V_STRING, 0, 0, 1,      FNAMESIZ,  0, NULL,  NULL,
"OPTIONS", V_STRING, 0, 1, 3,            16,  2, (GENPTR) &opt_valid,
						  	 (GENPTR)interrupt
	};




    static TEXT	*action[] = {"NONE"};	

    BEGIN_VALIDS(act_valid, 4)
        "NONE", "WINDOW", "GLOBALS", "PARFILE"
    END_VALIDS


    static struct RESIDVAR ptprcd[] =	/* PROCEDURE PDF		*/
	{
/* name    type      k  m maxc      size     dc val      dvp*/

"HELP",    V_STRING, 0, 1, 1,      FSPECSIZ,  1, NULL,  (GENPTR)null_string,
"NAME",    V_STRING, 0, 0, 1,      FNAMESIZ,  0, NULL,  NULL,
"OPTIONS", V_STRING, 0, 1, 3,            16,  2, (GENPTR) &opt_valid,
						  	 (GENPTR)interrupt,
"ACTION",  V_STRING, 0, 1, 1,      	 16, 1, (GENPTR) &act_valid,
							 (GENPTR)action
	};


    static struct RESIDVAR ptprcs[] =	/* PROCESS PDF			*/
	{
/* name    type      k  m maxc      size     dc val      dvp*/

"EXECUTE", V_STRING, 0, 1, 1,      FSPECSIZ, 1, NULL,   (GENPTR)null_string,
"HELP",    V_STRING, 0, 1, 1,      FSPECSIZ, 1, NULL,    (GENPTR)null_string,
"OPTIONS", V_STRING, 0, 1, 3,            16, 2, (GENPTR) &opt_valid,
						  	 (GENPTR)interrupt,
"NAME",    V_STRING, 0, 0, 1,      FNAMESIZ,  0, NULL,  NULL
	};

    static struct RESIDVAR ptret[] =	/* RETURN			*/
        {
"$SFI",   V_INTEGER, 0, 1, 1,             0, 1, NULL,    (GENPTR)val_one,
"$SKEY",  V_STRING , 0, 1, 1,     MAXSTRSIZ, 1, NULL,    (GENPTR)null_string
        };

    static struct RESIDVAR ptscmd[] =	/* SUBCMD PDF			*/
	{
	/* name    type      k  m maxc      size     dc val      dvp*/

	"NAME",    V_STRING, 0, 1, 1,      SUBCMDSIZ, -1, NULL,    NULL
	};

    static struct RESIDVAR ptswitch[] = /* SWITCH PDF			*/
        {
        "VALUE",   V_STRING, 0, 1, 1,      NAMESIZ,  -1, NULL,    NULL
        };


/*
 *	Note that all of the TCL "END" commands are defined in INTRINSIC.C
 */
    CODE	body_do(),	cmd_do(), 	cont_do(), 	defcmd_do(),
    		delcmd_do(), 	delgbl_do(), 	delloc_do(),	delvar_do(),
		exit_do(),      getpar_do(), 	global_do(), 	logoff_do(),
    		proced_do(), 	process_do(), 	return_do(), 	stop_do(),
	    	subcmd_do(),	parmset_do(),  switch_do(), 	
    		abort_do(),     nop_do();

/* Conglomerate flag definitions for ITRCMD structure */

#define	Y_BODYFLAG	Y_PROC | Y_PREBODY | Y_PROCSYN | Y_SUBSRCH | Y_CMD | Y_ABI
#define	Y_CONTINUE	Y_BODY | Y_PROCSYN | Y_ONFAIL  | Y_CMD | Y_ABI
#define	Y_PROCINTRO	Y_PROC | Y_INTRO | Y_PROCSYN | Y_CLEAR | Y_SUBSRCH \
    			   | Y_CMD | Y_ABI | Y_SEARCH | Y_PROCSEARCH \
    			   | Y_PREBODY | Y_BODY | Y_FALSE
#define	Y_PROCINTR2	Y_PROC | Y_INTRO | Y_PROCSYN | Y_CLEAR | Y_SUBSRCH \
    			   | Y_CMD | Y_ABI 	/* intro only */
#define Y_AI		Y_INTER | Y_ASYNC		/* i.e., no batch */
#define Y_GENASY       Y_PRIM|Y_PROC|Y_BODY|Y_PROCSYN|Y_CLEAR|Y_AI|Y_CMD  /*general case w/o batch */
#define Y_NOPCMD	Y_PRIM | Y_PROC | Y_PREBODY | Y_BODY | Y_CLEAR  | \
			Y_CMD  | Y_DYNCMD |  Y_ABI | Y_ANYSUB

    GLOBAL struct ITRCMD misccmd[] = 	/* miscellaneous commands  */
	{
{1, "ABORT",	"SYNC",     Y_GENERAL|Y_DEFSUB,
					0,	       NULL,    abort_do   },
#ifdef TAE_ASYNC
{1, "ABORT",	"ASYNC",    Y_GENERAL,  I_NPM(ptabo),  ptabo,   abort_do   },
#endif
#ifdef VAX_VMS
{1, "abort",	"BATCH",    Y_PDF,      0,	       NULL,    NULL	   },
#endif
{0, "BODY",	"",  	    Y_BODYFLAG,	0,	       NULL,    body_do	   },
{0, "COMMAND",	"",  	    Y_GENERAL,  0,	       NULL,    cmd_do	   },
{4, "CONTINUE",	"",         Y_CONTINUE,	0,	       NULL,    cont_do	   },
{4, "DEFCMD",	"",  	    Y_GENERAL,  I_NPM(ptdefc), ptdefc,	defcmd_do  },
{4, "DEFCMD",	"REPLACE",  Y_GENERAL,  I_NPM(ptdefc), ptdefc,	defcmd_do  },
{4, "DELCMD",	"",  	    Y_GENERAL,	I_NPM(ptdelc), ptdelc,  delcmd_do  },
{4, "DELETE",   "VARIABLE", Y_GENERAL|Y_DEFSUB,
					I_NPM(ptdelv), ptdelv,  delvar_do  },
{4, "DELETE",   "GLOBALS",  Y_GENERAL,	I_NPM(ptdelg), ptdelg,  delgbl_do  },
{4, "DELETE",   "LOCALS",   Y_GENERAL,	0,	       NULL,    delloc_do  },
{4, "DELETE",	"COMMAND",  Y_GENERAL,	I_NPM(ptdelc), ptdelc,  delcmd_do  },
{2, "EXIT",	"",         Y_GENERAL,	I_NPM(ptret),  ptret,   exit_do	   },
/* use GENERAL temporarily in the following, pending fix to chk_do
    should be GENASY	*/
{0, "GETPAR",	"",  	    Y_GENERAL,	 I_NPM(ptgetp), ptgetp, getpar_do  },
{6, "GLOBALS",	"",  	    Y_PROCINTRO, I_NPM(ptglbl), ptglbl,	global_do  },
{6, "GLOBALS",	"COMPILED", Y_PROCINTRO, I_NPM(ptglbl), ptglbl,	global_do  },
{2, "LOGOFF",	"",  	    Y_GENERAL,	 I_NPM(ptret),	ptret,  exit_do  },
{3, "NOP",	"",  	    Y_NOPCMD,	 0,	 	NULL,   nop_do     },
{0, "PARMSET",  "",  	    Y_PROCINTRO, I_NPM(ptprcd), ptprcd, parmset_do },
{0, "PARMSET",  "COMPILED", Y_PROCINTRO, I_NPM(ptprcd), ptprcd, parmset_do },
{0, "PROCEDURE","",  	    Y_PROCINTRO, I_NPM(ptprcd), ptprcd, proced_do  },
{0, "PROCEDURE","COMPILED", Y_PROCINTRO, I_NPM(ptprcd), ptprcd, proced_do  },
{0, "PROCESS",	"",  	    Y_PROCINTRO, I_NPM(ptprcs), ptprcs, process_do },
{0, "PROCESS",	"COMPILED", Y_PROCINTRO, I_NPM(ptprcs), ptprcs, process_do },
{0, "RETURN",	"",  	    Y_RETURN,	 I_NPM(ptret),  ptret,  return_do  },
{0, "STOP",     "",  	    Y_RETURN,    0,  		NULL,   stop_do    },
{0, "SUBCMD",	"",  	    Y_BODYFLAG,	 I_NPM(ptscmd), ptscmd,	subcmd_do  },
{0, "SUBCMD",	"DEFAULT",  Y_BODYFLAG,  I_NPM(ptscmd), ptscmd,	subcmd_do  },
{3, "SWITCH",   "",  	    Y_GENERAL,   I_NPM(ptswitch),
							ptswitch, switch_do },

{0, ""}	/* TERMINATOR ENTRY: REQUIRED AT END */
	};


/*	setOptions for parmset, procedure, process, ... commands
 *
 */

static FUNCTION VOID setOptions (cpctx, npctx)

    struct CONTXT	*cpctx;		/* in:  containing proc context	*/
    struct CONTXT	*npctx;		/* in:  proc ctx for CONT cmd line */
{
struct VARIABLE *v;
IMPORT struct VARIABLE *tutoropt_gbl;

v = lookex (&((*npctx).parmst), "OPTIONS");
if (v)
    {
    (*cpctx).interrupt = 
	search_vector ((*v).v_cvp, (*v).v_count, "INTERRUPT");
    (*cpctx).selftutor = 
	search_vector ((*v).v_cvp, (*v).v_count, "SELFTUTOR") &&
	!search_vector ((*tutoropt_gbl).v_cvp, (*tutoropt_gbl).v_count, 
		"NO_SELFTUTOR");
    (*cpctx).compress = search_vector 
			    ((*v).v_cvp, (*v).v_count, "COMPRESSTUTOR") 
			||  
			search_vector 
			    ((*tutoropt_gbl).v_cvp, (*tutoropt_gbl).v_count, 
							            "COMPRESS");
    }
}


/*
 *	abort_do - perform ABORT statement.
 *
 *	Just check if proc interrupt mode; return DO_ABORT if it is.
 */

    FUNCTION CODE abort_do (cpctx, npctx)

    struct CONTXT	*cpctx;		/* in:  containing proc context		*/
    struct CONTXT	*npctx;		/* in:  proc ctx for CONT cmd line	*/

    {
    IMPORT CODE		cmd_mode;	/* INTMODE if interrupt mode	*/

    CODE		code;

#ifdef TAE_ASYNC
    if (s_equal (&(*npctx).subcmd[0], "ASYNC"))
	{
	code = async_abo(cpctx, npctx);
	return(code);
	}
#endif
    if (cmd_mode == INTMODE)
        return(DO_ABORT);
    tmmsg(PROCFAIL, "'ABORT' (no subcommand) is only available in proc interrupt mode.",
    		"TAE-BADCMD") ;
    return(DO_SUCCESS);		/* don't worry about DOCHECK in int mode	*/
    }

/*
 *	body_do - perform BODY statement.
 */

    FUNCTION CODE body_do (cpctx, npctx)

    struct CONTXT	*cpctx;		/* in:  context of proc		*/
    struct CONTXT	*npctx;		/* in:  context of command	*/

    {
    if ((*cpctx).proctype != Y_PROCEDURE)
	{	
	tmmsg (PROCFAIL, "'BODY' command is allowed only in procedures.",
		"TAE-PRCDONLY");
	return (DO_RETURN);
	}
    (*cpctx).subblk = FALSE;		/* in case end-sub missing	*/
    (*cpctx).inbody = TRUE;		/* mark proc in procedure body	*/
    return(DO_SUCCESS);
    }

/*
 *	cmd_do - perform COMMAND command.
 */

    FUNCTION CODE cmd_do (cpctx, npctx)

    struct CONTXT	*cpctx;		/* in:  containing proc context		*/
    struct CONTXT	*npctx;		/* in:  proc ctx for COMMAND cmd line	*/

    {
    IMPORT CODE		  usermode;	/* MENUMODE or CMDMODE			*/

    usermode = CMDMODE;
    return(DO_SUCCESS);
    }

/*
 *	cont_do - perform CONTINUE statement.
 */

    FUNCTION CODE cont_do (cpctx, npctx)

    struct CONTXT	*cpctx;		/* in:  containing proc context		*/
    struct CONTXT	*npctx;		/* in:  proc ctx for CONT cmd line	*/

    {
    IMPORT CODE		cmd_mode;	/* INTMODE if interrupt mode		*/

    if (cmd_mode == INTMODE)
        return(DO_CONTINUE);		/* required for proc interrupt mode	*/
    return(DO_SUCCESS);			/* CONTINUE is a transparent statement	*/
    }

/*
 *	defcmd_do - perform DEFCMD command, i.e., defines a command (alias).
 *	Assumes there are no default values for parameters.
 */

    FUNCTION CODE defcmd_do (cpctx, npctx)

    struct CONTXT	*cpctx;		/* in:  containing proc context		*/
    struct CONTXT	*npctx;		/* in:  proc ctx for DEFCMD cmd line	*/

    {
    IMPORT struct VARIABLE *alpha_var();
    IMPORT struct SYMTAB   glbtab;
    IMPORT struct VARIABLE *sfi_gbl;

    FAST struct VARIABLE *v;			/* variable in symbol table	*/
    struct VARIABLE *dv;
    GENPTR	allval();
    TEXT	defcbuf[STRINGSIZ+1];		/* buffer  defcmd cmd=string	*/
    TEXT	*svector[1];
    COUNT	i, count, mi;
    CODE	code;
    static TEXT dname[] = "$DEFCMDi";
    struct DEFPDF *defpdf;
    BOOL	replaceMode, newCommand;

    replaceMode = s_equal ("replace", (*npctx).subcmd);
    v  = lookex(&((*npctx).parmst), "COMMAND");	
    s_copy(SVAL(*v, 0), defcbuf);		/* local copy of command */
    if (defcbuf[0] == '*')
	goto dc_abberr;				/* invalid syntax       */
    code = cmdmatch(defcbuf, &dv, &mi);		/* command exists?	*/
    if (code != FAIL  &&  !replaceMode)
	 goto dc_cnflct;			/* not a new command */	
    newCommand = code == FAIL;			/* really replacing? */
    if (newCommand)				/* build new command: */
	{
	for (i=0; i <= 9; i++)
	    {
	    dname[sizeof(dname)-2] = '0' + i;	/* build $DEFCMDi name	*/
	    dv = lookex (&glbtab, dname);		/* find VARIABLE */
	    if (dv == NULL)				/* must make it? */ 
	        {
	        dv = alpha_var (dname, &glbtab);    /* allocate VARIABLE */
	        s_copy (dname, (*dv).v_name);	/* build new $DEFCMDi:	*/
	        (*dv).v_type = V_STRING;
	        (*dv).v_class = V_GLOBAL;
	        (*dv).v_protect = TRUE;
	        (*dv).v_intrinsic = FALSE;
	        (*dv).v_pdf = defpdf = (*sfi_gbl).v_pdf;   /* use TAEGBL...*/
	        (*defpdf).refcount++;		       /* as PDF	*/
	        (*dv).v_refcnt = 1;
	        (*dv).v_count = 0;
	        (*dv).v_minc = 1;
	        (*dv).v_maxc = INT_MAXVAL;
	        (*dv).v_size = MAXSTRSIZ;
	        (*dv).v_nullable = TRUE;
	        (*dv).v_file = FALSE;
	        (*dv).v_valid = NULL;
	        (*dv).v_cvp = allval (dv);
	        (*dv).v_dcount = -1;
	        (*dv).v_dvp = NULL;
		mi = 0;				/* index to use         */
	        break;
	        }
	    if ((*dv).v_count < (*dv).v_maxc)	/* found one with room? */
		{
		mi = (*dv).v_count ;		/* index to use		*/
		break;
		}
	    }	
        if (i > 9)
	    goto dc_noroom;			/* all $DEFCMDi are full */
 	}
    v = lookex(&(*npctx).parmst, "STRING");	/* replacement string    */ 
    s_append("=", defcbuf);
    s_append(SVAL(*v, 0), defcbuf);		/* build the defcmd string */
    svector[0] = defcbuf;
    if (newCommand)
        fill_value(dv, mi+1);			/* get count bumped for... */
    code = set_component(dv, (GENPTR)svector, mi+1); 	/* note: TCL index */
    return(DO_SUCCESS);


dc_abberr:
    tmmsg(PROCFAIL, "Abbreviations must contain at least one character",
		 "TAE-BADABB");
    return(DO_CHECK);

dc_cnflct:
    tmmsg(PROCFAIL, "New command conflicts with existing command '%s'.",
	    "TAE-CMDCNFLCT", SVAL(*dv, mi));
    return(DO_CHECK);

dc_noroom:
    tmmsg(PROCFAIL,
	    "Maximum number of DEFCMDs exceeded.", "TAE-MAXDEFC");
    return(DO_CHECK);
    }

/*
 *	delcmd_do - perform DELCMD command, i.e., deletes a verb (alias) previously
 *	defined with DEFCMD, from the defined verb list.
 */

    FUNCTION CODE delcmd_do (cpctx, npctx)

    struct CONTXT	*cpctx;		/* in:  containing proc context		*/
    struct CONTXT	*npctx;		/* in:  proc ctx for DELCMD command	*/

    {
    struct VARIABLE	*v;		/* variable in symbol table	*/
    struct VARIABLE *dv;		/* matching VARIABLE		*/
    CODE 	code;
    TEXT	**s;
    COUNT	i, j, mi, last;

    v  = lookex(&((*npctx).parmst), "COMMAND");	/* look up verb in symbol table	*/
    for (i=0; i < (*v).v_count; i++)		/* for each command entered */
	{
	    code = cmdmatch(SVAL(*v, i), &dv, &mi);	/* find the command	*/
	    if (code != SUCCESS)		/* match not found, error	*/
		goto de_nderr;
	    s = (TEXT **) (*dv).v_cvp;		/* string value vector ptr	*/
	    tae_free (s[mi]);			/* free the matching string	*/
	    last = (*dv).v_count - 1;		/* index of last value		*/
	    for (j=mi; j < last; j++)
		s[j] = s[j+1];			/* move lower strings upward	*/
	    s[last] = NULL;			/* make last index null		*/
	    --(*dv).v_count;			/* decrement count		*/
	}
    return(DO_SUCCESS);

de_nderr:
    tmmsg(PROCFAIL, "'%s' not defined.", "TAE-UNDEFCMD",
	SVAL(*v, 0));
    return(DO_CHECK);
    }

/*
 *	delgbl_do - Perform DELETE-GLOBALS command.
 *	Delete globals listed in a GLOBAL PDF.  This function imitates the
 *	proc processing part of prccmd through the pdftab call.
 *	It then closes the PDF & deletes the globals named in the resulting
 *	parameter symbol table.
 */

    FUNCTION CODE delgbl_do (cpctx, npctx)

    struct CONTXT	*cpctx;		/* in:  containing proc context		*/
    struct CONTXT	*npctx;		/* in:  proc ctx for DELGBL command	*/

    {
    IMPORT struct SYMTAB  glbtab;	/* global symbol table			*/
    IMPORT struct SFILE	  prcfil;	/* proc file context			*/

    struct CONTXT	delctx;		/* context for DELGBL simulated proc	*/
    struct VARIABLE	*v;	
    struct VARIABLE	*gv;		/* each global variable			*/
    CODE		code;
    TEXT		proc[FSPECSIZ+1];
    struct SYNBLK	sb;		/* syntax package block			*/
    static TEXT		nongbl_msg[] = "A GLOBAL proc must be used to delete global variables.";
    static TEXT		nongbl_key[] = "TAE-NONGBL";

    delctx.prclevel = (*cpctx).prclevel + 1;	/* mark simultaed proc as next level*/
    if (inictx(&delctx) != SUCCESS)
	return(DO_RETURN);
    delctx.backlink = cpctx;			/* link to previous		*/
    delctx.intrinsic = FALSE;
    if ((*cpctx).prclevel > 0)			/* if DELGBL not from primary level...*/
	clssav(cpctx);				/* close containing proc & save posit*/

    ini_status();				/* init $SFI & $SKEY		*/
    v = lookex(&(*npctx).parmst, "PROC");	/* find name of GLOBAL proc to delete from*/
    code = cmd_parse(&sb, SVAL(*v, 0), NULL, proc, delctx.subcmd);
    if (code != SUCCESS)
	goto syntax_err;
    if (opnpdf(cpctx, &delctx, proc, &prcfil) != SUCCESS) /* open the PDF		*/
	goto re_open;
    if (plcini(&delctx) != SUCCESS)		/* init implicit locals		*/
	goto pdf_error;
    if (pdftab(&delctx, &prcfil) != SUCCESS)	/* build symb tabs from proc sttmts*/
	goto pdf_error;
    f_close(&prcfil, F_KEEP);
    if ((*cpctx).prclevel > 0)			/* if DELETE not from primary level...*/
	if (opnsav(cpctx) != SUCCESS)
	    return(DO_CHECK);

/* now delete the globals. */

    if (delctx.proctype != Y_GLOBAL)
	goto nonglobal_err;
    for (v = delctx.parmst.link; v != NULL; v = (*v).v_link)
	{
	gv  = lookex(&glbtab, (*v).v_name);	/* point to global variable	*/
	if (gv == NULL)
	    goto undef_err;
	if (del_gbl(&glbtab, gv) != SUCCESS) 	/* try to delete the global	*/
	    goto close_ctx;
	}
    clsctx(&delctx);
    return(DO_SUCCESS);

pdf_error:
    if (delctx.proctype != Y_UNKNOWN  &&  delctx.proctype != Y_GLOBAL)
	tmmsg(PROCFAIL, nongbl_msg, nongbl_key);
    f_close(&prcfil, F_KEEP);
    goto re_open;

syntax_err:
    tmmsg(PROCFAIL, sb.errmsg, "TAE-SYNERR");
re_open:
    clsctx(&delctx);
    if ((*cpctx).prclevel > 0)			/* if DELGBL not from primary level...*/
	opnsav(cpctx);
    return(DO_CHECK);

nonglobal_err:
    tmmsg(PROCFAIL, nongbl_msg, nongbl_key);
    goto close_ctx;
undef_err:
    tmmsg(PROCFAIL,	"Reference to undefined global '%s'.",
	    "TAE-UNDEFGBL", (*v).v_name);
    goto close_ctx;

close_ctx:
    clsctx(&delctx);
    return(DO_CHECK);
    }

/*
 *	delloc_do. Delete all local parameters.
 *	
 *	NOTE: certain locals such as _STDOUT and _ONFAIL are never deleted.
 *
 */

    FUNCTION CODE delloc_do (cpctx, npctx)

    struct CONTXT	*cpctx;		/* in:  containing proc context		*/
    struct CONTXT	*npctx;		/* in:  proc ctx for DELETE command	*/

    {
    struct VARIABLE	*v;


    for (v = (*cpctx).locst.link; v != NULL; v = (*v).v_link)	/* step thru locst */
	{
	del_loc(cpctx, v);
	}
    return (DO_SUCCESS);
    }

/*
 *	delvar_do. Delete a global or local parameter.
 */


    FUNCTION CODE delvar_do (cpctx, npctx)

    struct CONTXT	*cpctx;		/* in:  containing proc context		*/
    struct CONTXT	*npctx;		/* in:  proc ctx for DELETE command	*/

    {
    IMPORT struct SYMTAB  glbtab;	/* global symbol table			*/

    struct VARIABLE	*v, *n;
    COUNT		i;
    CODE		code;
    TEXT		*name;

    n = lookex(&(*npctx).parmst, "NAME");	/* find name of variable to delete */
    for (i=0; i < (*n).v_count; i++)		/* for each name 	   */
        {
	name = SVAL(*n, i);		
	if (s_index (name, '.')  >  0)		/* is name qualified?	   */
	    {
	    tmmsg (PROCFAIL, 
		"Delete not allowed for (qualified) variable '%s'.", 
		"TAE-QUALDEL", name);
	    continue;
	    }
	v = lookex(&(*cpctx).locst, name);
	if (v != NULL)				/* found there		    */
	    del_loc(cpctx, v);
	else
	    {
	    v = lookex(&glbtab, name);		/* search global symbol table*/
	    if (v == NULL) 	
    		tmmsg(PROCFAIL, "Reference to undefined local/global variable '%s'.",
	    	"TAE-UNDEFVAR", name);
    	    else
	        del_gbl(&glbtab, v);
	    }
        }
    return (DO_CHECK);
    }

/*
 *	del_gbl.  Delete a global variable.
 */

    FUNCTION  CODE del_gbl(st, gv)

    struct  SYMTAB	*st;			/* in/out: gbl symbol table   */
    struct  VARIABLE	*gv;			/* in: ptr to variable in tbl */

    {
    if ((*gv).v_refcnt > 0  || (*gv).v_protect)	/* if this glbl is currently referenced	*/
	goto refcnt_err;	
    delvar(st, gv);				/* delete it from gbl sym tab */
    return (SUCCESS);

refcnt_err:
    tmmsg(PROCFAIL,	"Attempt to delete currently referenced global '%s'.",
	    "TAE-DELREF", (*gv).v_name);
    return (FAIL);
    }

/*
 *	del_loc.  Delete a local variable.
 */

    FUNCTION  CODE del_loc(cpc, vptr)

    struct  CONTXT	*cpc;		/* in/out: containing proc context */
    struct  VARIABLE	*vptr;		/* in: ptr to variable in tbl */

    {
    struct  ICBLOCK	*icbptr;	/* pointer to icb linked to proc contxt */
    struct  VARIABLE	*v;

    icbptr = (struct ICBLOCK *) (*cpc).icbptr;
    for ( ; icbptr != NULL; icbptr = (*icbptr).i_link)	/* if in iteration */
	{				/* check current use by a FOR loop */
	if ((*icbptr).type == TYPE_FOR && (*icbptr).v == vptr)
	    goto used_err;
	}
    for (v = (*cpc).parmst.link; v !=NULL; v = (*v).v_link)  /* step thru parmst */
	{
	if ((*v).v_type == V_NAME &&
	    ((*v).v_ref == vptr || (*v).v_dref == vptr))
	    goto used_err;
	}
    if (!exclude(vptr))			/* if not excluded from delete	*/
	delvar (&(*cpc).locst, vptr);	/* delete from local symtab */
    else
	{
	tmmsg(PROCFAIL,    "Attempt to delete implicit local variable '%s'.",
		"TAE-DELVAR",(*vptr).v_name);
	return (FAIL);
	}
    return (SUCCESS);

used_err:
    tmmsg(PROCFAIL,     "Attempt to delete currently referenced local '%s'.",
            "TAE-DELREF", (*vptr).v_name);
    return (FAIL);
    }

/*
 *	endgbl_do - perform END-GLOBAL statement.
 */

    FUNCTION CODE endgbl_do (cpctx, npctx)

    struct CONTXT	*cpctx;		/* in:  context of containing proc	*/
    struct CONTXT	*npctx;		/* in:  proc ctx for END-GLOBAL command	*/

    {
    return(DO_RETURN);
    }

/*
 *	endproc_do - perform END-PROC statement.
 */

    FUNCTION CODE endproc_do (cpctx, npctx)

    struct CONTXT	*cpctx;		/* in/out: context of containing proc	*/
    struct CONTXT	*npctx;		/* in:  ctx from END-PROC cmd (null)	*/

    {
    struct SEARCH	*srch;		/* Pointer to search block */
    TEXT		msg[STRINGSIZ+1];

    if ((*cpctx).srchblk != NULL)	/* Still in search mode? */
	{
	srch = (struct SEARCH *) (*cpctx).srchblk;
	if ((*srch).type == SRCH_GOTO)
	    {
	    if ((*srch).onfailcmd)
		{
		s_copy ("Undefined label '%s' in _ONFAIL command,", msg);
		s_append ("\n\ractivated after command in procedure.", msg);
		tmmsg (PROCFAIL, msg, "TAE-BADONFAIL", (*srch).label);
		}
	    else
		tmmsg (PROCFAIL, "Reference to undefined label '%s'.",
		       "TAE-UNDLABEL", (*srch).label);
	    return (DO_RETURN);
	    }
    	if ((*srch).type == SRCH_PROC) 		/* scanning an internal proc */
    	    {
    	    if ((toptin ((*srch).srchstk) != TYPE_NONE))
    		{
		poptin ((*srch).srchstk);	/* nested internal proc	    */
    	    	if (toptin ((*srch).srchstk) != TYPE_PROC)
		    {
    		    (*srch).type = saved_search;
		    (*srch).cmd_line = saved_line;
		    (*srch).onfailcmd = saved_onfail;
		    }
    		}
    	    else
    		{				/* nothing in stack, srch done */
		tae_free ((*cpctx).srchblk);	/* Deallocate structure     */
		(*cpctx).srchblk = NULL;	/* Clear pointer to structure */
    		}
    	    return(DO_SUCCESS);
    	    }
	}
    else if ((*cpctx).proctype == Y_PROCESS)
    	(*cpctx).inbody = TRUE;		/* mark process PDF 'past parms'	*/
    if (toptin ((*cpctx).typstk) != TYPE_NONE)	/* Type stack empty? */
	{
	tmmsg (PROCFAIL, "Missing END-FOR, END-LOOP, or END-IF.",
	  	"TAE-UNBALCON");
	return (DO_RETURN);
	}
    return(DO_RETURN);
    }

/*
 *	endsub_do - perform END-SUB statement.
 */

    FUNCTION CODE endsub_do (cpctx, npctx)

    struct CONTXT	*cpctx;		/* in/out: context of containing proc	*/
    struct CONTXT	*npctx;		/* in:  ctx from END-SUB cmd (null)	*/

    {
    if (!(*cpctx).subblk)
	{
	tmmsg(PROCFAIL, "END-SUB with no matching SUBCMD.", "TAE-NOSUB");
	return(DO_RETURN);
	}
    (*cpctx).subblk = FALSE;		/* no longer in SUBCMD block		*/
    (*cpctx).subact = FALSE;		/* no longer in active SUBCMD block	*/
    return(DO_SUCCESS);
    }

/*
 *	exit_do - perform EXIT command.
 *
 *	NOTE:
 *	Since under UNIX the exit handler is not automatically triggered
 *	at normal termination (exit) time, we explicitly terminate the
 *	asynchronously running jobs here.
 */

    FUNCTION CODE exit_do (cpctx, npctx)

    struct CONTXT	*cpctx;		/* in:  containing proc context		*/
    struct CONTXT	*npctx;		/* in:  proc ctx for EXIT command	*/

    {
    IMPORT CODE 	cmd_mode;	/* current command mode		*/
    IMPORT CODE		run_type;	/* INTER or ASYNC or BATCH	*/
    IMPORT BOOL		host_enabled;	/* TRUE if escape to host allowed   */

    TEXT		cmdstr[CMDLINSIZ+1];

    if (!host_enabled)			/* escape to host not allowed?	*/
	{
	tmmsg (PROCFAIL, "'EXIT' command has been disabled.", "TAE-DISHOST");
	return (DO_CHECK);
	}	
    if (cmd_mode != NORMCMD)		/* only allowed in normal cmd mode */
    	{
    	tmmsg(PROCFAIL, "'EXIT' not available in proc interrupt mode.",
    		"TAE-BADCMD");
    	return(DO_CHECK);
    	}
#ifdef TAE_ASYNC
    if (run_type == ASYNC)
    	async_exit();			/* local async job termination	*/
#endif
#ifdef UNIX
    term_async();			/* terminate async offsprings	*/
#endif
    s_copy("slogoff", cmdstr);	/* execute SLOGOFF at local node */
    prccmd(cmdstr, cpctx);		/* execute SLOGOFF proc		*/
    disable_recvar ();		/* DISABLE-RECVAR 		*/
    term_ins();			/* installation exit		*/
    exit(TM_EXIT);
    }

/*
 *	getpar_do - perform GETPAR command
 */
    FUNCTION CODE getpar_do (proc_ctx, getpar_ctx)

    struct CONTXT	*proc_ctx;	/* in/out: ctx for proc described by PDF*/
    struct CONTXT	*getpar_ctx;	/* in:  ctx for GETPAR sttmt		*/

    {
    IMPORT struct SFILE	   prcfil;	/* proc file context			*/
    IMPORT struct VARIABLE *sfi_gbl, *skey_gbl;
    IMPORT CODE		run_type;	/* INTER or BATCH or ASYNC		*/
#ifdef TAE_ASYNC
    IMPORT CODE		parent_runtype;	/* run_type of parent monitor		*/

#endif
    struct CONTXT 	paramctx;	/* param context for tutor		*/
    COUNT		i;
    struct VARIABLE	*ppt, *p, *p1, *v, *v1;
    struct VARIABLE	*lookex(), *preface;
    TEXT		*preftab[12];
    TEXT		**names, **prefpt;
    CODE		code;
    TINY		prefcnt;

#ifdef TAE_ASYNC
    if (parent_runtype == BATCH || parent_runtype == ASYNC)
    	{
    	tmmsg (PROCFAIL,
"Async jobs may request parms only if invoked interactively.", "TAE-ASYPARM");
    	return (DO_CHECK);
    	}
#endif
    if (run_type == BATCH)
	{
	tmmsg(PROCFAIL, "'GETPAR' not available in batch.",
			"TAE-NOBATCHCMD");
	return (DO_CHECK);
	}
    MOVE_STRUCT(*proc_ctx, paramctx);		/* use proc's ctx as basis	*/
    paramctx.parmst.link = NULL;		/* clear out symbol tables	*/
    paramctx.locst.link = NULL;
    paramctx.qualst.link = NULL;
    paramctx.numrefs = 0;
    for (i=0; i<MAXREF; i++)
    	paramctx.refs[i] = NULL;
    paramctx.icbptr = NULL;			/* icb pointer			*/
    paramctx.subptr = NULL;
    paramctx.int_procs = NULL;			/* internal proc block pointer	*/

/* move into parmst only those variables that are requested		*/

    ppt = lookex(&((*getpar_ctx).parmst), "PARMS");	/* find the request list	*/
    names = (TEXT **)(*ppt).v_cvp;	/* The name list			*/
    code = parm_select(proc_ctx, names, (*ppt).v_count, &paramctx.parmst);
    if (code != SUCCESS)
    	goto exit;
    code = parm_local(proc_ctx, &paramctx.locst);
    if (code != SUCCESS)
    	goto exit;
    code = plcini(&paramctx);
    if (code != SUCCESS)
	{
	overr();
	goto exit;
	}
    code = gp_ins (&prcfil, &paramctx);		/* installation exit */
    if (code != DP_NOTDONE)				
	;					/* dp_ins did the job*/
    else
	{
        preface = lookex(&((*getpar_ctx).parmst), "PREFACE");	
	if ((*preface).v_count == 0)	
	    preface = NULL;			/* no PREFACE	     */
    	if (run_type == ASYNC)			/* get params from remote monitor */
    	    {

#ifdef TAE_ASYNC
	    if (preface != NULL)
		{
		for (i = 0; i < (*preface).v_count; i++)
		    preftab[i] = SVAL(*preface,i);
		prefpt = preftab;
		prefcnt = (*preface).v_count;
		}
	    else
		{
		prefpt = NULL;
		prefcnt = 0;
		}
    	    code = get_parms_remote (proc_ctx, prefpt, &paramctx, prefcnt);
    	    if ( (code != SUCCESS) && (code != DO_EXIT) )
    		goto exit;
            if (code == SUCCESS)		/* don't run if dyn tut exit */
    	        code = DO_RUN;
#endif
    	    }
    	else
    	    {
	    code = opn_tutor(&prcfil, &paramctx, NOFORCE, preface, TRUE);
	    if (code != SUCCESS)
		goto exit;
	    paramctx.asydyntut = FALSE;		/* not tutoring for async job */
	    code = tutor (&prcfil, &paramctx);
	    cls_tutor (&prcfil, &paramctx);
    	    }
	}
    if (code == DO_RUN)
/* update the proc_ctx using the values from new latched ctx		*/
    	{
	for (p=paramctx.parmst.link; p!=NULL; p=(*p).v_link)
	    {				/* update requested variables	*/
	    v = search ((*p).v_name, proc_ctx);
    	    if (v == NULL) continue;	/* ignore variables not in original ctx */
	    code = set_value(v, (*p).v_cvp, (*p).v_count);
	    if (code != SUCCESS)
		overr();		/* message about alloc error	*/
	    for (p1=(*p).v_qualst.link; p1!=NULL; p1=(*p1).v_link)
		{			/* update any param qualifiers */
		v1 = lookex(&(*v).v_qualst, (*p1).v_name);
    	        if (v1 == NULL) continue;  /* ignore variables not in original ctx */
	        code = set_value(v1, (*p1).v_cvp, (*p1).v_count);
	        if (code != SUCCESS)
		   overr();		/* message about alloc error	*/
	        }
	    }
    	}
    else
        {
        IVAL(*sfi_gbl, 0) = -1;			/* indicate killed	*/
	set_string (skey_gbl, "TAE-EXIT");	/* set $SFI/$SKEY	*/
	}
exit:
    clsctx(&paramctx);
    return(DO_CHECK);
    }

#ifdef TAE_ASYNC
/*  get_parms_remote - Get parameters from parent monitor
 *
 *  returns SUCCESS/FAIL; issues err msg on FAIL
 *
 *  Strategy is to put the parameters in a save file, send a MON_MSG to the
 *  parent, and then get the updated parameters from the save file specified
 *  in the returned MON_MSG.
 */
    FUNCTION CODE get_parms_remote (proc_ctx, preface, parctx, vcount)

    struct CONTXT	*proc_ctx;	/* in: Context of executing proc    */
    TEXT		**preface;	/* in: preface for tutoring 	    */
    struct CONTXT	*parctx;	/* in/out: parameter context	    */
    FUNINT		vcount;		/* in: number of preface lines	    */

    {
    IMPORT TEXT		comm_file[];	/* async comm'n file		    */

    struct SFILE	sf;
    CODE		code;
    TEXT		recv_file[FSPECSIZ+1];	
    TEXT		procspec[FSPECSIZ+1];
    TEXT		*pt;
    CODE		send_stat, recv_stat;
    struct VARIABLE	*v, *lookex ();

    if (preface != NULL)
	code = cre_locstr (&(*parctx).parmst, "_PREFACE", vcount, MAXSTRSIZ, 
    				preface);

/*	Construct the name of the PROC with subcommand */

    v = lookex (&(*proc_ctx).locst, "_PROC");
    s_copy (SVAL(*v,0), procspec);
    v = lookex (&(*proc_ctx).locst, "_SUBCMD");
    if (!NULLSTR(SVAL(*v,0)))
	{
	s_append ("-", procspec);
	s_append (SVAL(*v,0), procspec);
	}
    pt = procspec;
    code = cre_locstr (&(*parctx).parmst, "_PROC", 1, FSPECSIZ+1, &pt);
    if (code != SUCCESS)
    	return (FAIL);
    code = f_opnspc (&sf, SAVELUN, comm_file, "", "", ASY_TYPE, F_WRITE);
    if (code != SUCCESS)
    	{
    	tmmsg(PROCFAIL,
"Error opening async communication file.  Host code = %s.","TAE-COMOPEN", sf.errmsg);
    	return(FAIL);
    	}
    code = save_pfile (&sf, M_SUBPDF, NULL, 0, 1, &(*parctx).parmst);	/* write parmst to file */
    if (code != SUCCESS)
    	{
    	tmmsg(PROCFAIL,
"Error writing to async communication file. Host code = %s.","TAE-COMWRT", sf.errmsg);
    	f_close(&sf, F_DELETE);
    	return(FAIL);
    	}
    f_close (&sf, F_KEEP);

    /** now send the parameter file to the parent monitor and receive reply   */
    code = get_parm_locpar(comm_file, recv_file, FALSE,
		&send_stat, &recv_stat);	/* talk to local parent	      */
	
    if (code == KILLED)
	return (DO_ABORT);			/* non_interactive proc exit  */
    else if (code == DO_EXIT)
        return (DO_EXIT);			/* non-interactive dyn tut exit */
    if (code != SUCCESS)
	{
    	if (send_stat != SUCCESS)		/* could not send to parent   */
	    tmmsg(PROCFAIL,
	     "Error sending message to parent monitor.  Host error code = %d.",
    	 	"TAE-ASYSND", send_stat);
	else					/* error in  msg reception    */
	    tmmsg(PROCFAIL,
	 "Error receiving message from parent monitor.  Host error code = %d.",
    		"TAE-RCVMON", recv_stat);
    	return(FAIL);
    	}
    code = get_syms (recv_file, parctx);
/* TBD:    GET RID OF _PROC AND _PREFACE FROM PARMST */
    return(code);
    }
#endif

/*
 *	global_do - perform GLOBAL statement (a PDF introductory statement).
 */

    FUNCTION CODE global_do (cpctx, npctx)

    struct CONTXT	*cpctx;		/* in/out: ctx for proc described by PDF*/
    struct CONTXT	*npctx;		/* in:  ctx for PROCEDURE sttmt		*/

    {
    struct VARIABLE	*v;
    CODE	code;

    if ((*cpctx).proctype == Y_UNKNOWN)
        {
	(*cpctx).proctype = Y_GLOBAL;
	(*cpctx).compiled = s_equal((*npctx).subcmd, "COMPILED")  ?  TRUE : FALSE;
	v = lookex(&((*npctx).parmst), "HELP");
	s_copy(SVAL(*v, 0), (*cpctx).help_spec);	/* save help filespec text	*/
	setOptions (cpctx, npctx);
	return(DO_SUCCESS);
        }

    /* here if GLOBAL seen internal to another proc */

    code = int_proc (cpctx, npctx);
    return (code);
    }


/*  int_proc - Handle internal procs
 *
 *  Save the name and position in an IPBLOCK and set search for END-PROC.
 *  Note that if we're nested in another internal proc, all we want to
 *  do is push onto the search stack.
 */
    FUNCTION CODE int_proc (cpctx, npctx)

    struct CONTXT	*cpctx;		/* in:  containing proc context		*/
    struct CONTXT	*npctx;		/* in:  proc ctx for PROC command	*/

    {
    struct VARIABLE	*v, *lookex();
    struct IPBLOCK	*ipbpt, *pt, *lastpt;
    struct SEARCH	*srch;
    CODE		code;
    BOOL		true_part();

    /* if not already searching, initialize search block to search for PROC or END-PROC */
    if ((*cpctx).srchblk == NULL)
    	{
	srch = (struct SEARCH *) tae_alloc (1, sizeof (struct SEARCH));
    	saved_search = SRCH_PROC;
	(*srch).type = SRCH_PROC;
	(*srch).cmd_line = (*cpctx).pdf_line;
	(*srch).onfailcmd = FALSE;
	stk_in ((*srch).srchstk, MAXNEST,
			    TYPE_NONE);	/* Initialize type stack */
	(*cpctx).srchblk = (GENPTR) srch;	/* Put search block pointer in ctx */
    	/* move required information into ipblock and link it in */
    	if (true_part(cpctx))			/* don't save pos'n if in false bracket  of IF */
    	    {
	    ipbpt = (struct IPBLOCK *) tae_alloc (1, sizeof(struct IPBLOCK));
	    v = lookex(&((*npctx).parmst), "NAME");		/* name of int'l proc */
	    if ((*v).v_count == 0)
		{
		tmmsg (PROCFAIL, "Internal proc encountered without a name.",
			 "TAE-NONAME");
		goto undo_both;
		}
	    s_copy (SVAL(*v,0), (*ipbpt).procname);		/* proc name into block	    */
	    f_movpos (&((*cpctx)).prcpos, &((*ipbpt).position));  /* save record pos'n    */
	    if ((*cpctx).int_procs == NULL)
		(*cpctx).int_procs = (GENPTR) ipbpt;	/* link new block	    */
	    else
		{
		for (pt = (struct IPBLOCK *)(*cpctx).int_procs; pt != NULL; pt = (*pt).flink)
		    {
		    lastpt = pt;				/* save last one	*/
		    if (s_equal((*pt).procname, (*ipbpt).procname))
			{
			tmmsg (PROCFAIL, "NAME '%s' for internal proc already in use.",
				    "TAE-DUBINTNAME", (*ipbpt).procname);
			return (DO_RETURN);
			}
		    }
		(*lastpt).flink = ipbpt;			/* link new		    */
		}
	    (*ipbpt).flink = NULL;
    	    }
    	}
    else				/* we're already searching */
    	{
	srch = (struct SEARCH *) (*cpctx).srchblk;
    	if ((*srch).type != SRCH_PROC)
    	    {
    	    saved_search = (*srch).type;
	    saved_line = (*srch).cmd_line;
	    saved_onfail = (*srch).onfailcmd;
    	    (*srch).type = SRCH_PROC;
	    (*srch).cmd_line = (*cpctx).pdf_line;
	    (*srch).onfailcmd = FALSE;
    	    }
	code = pushti ((*srch).srchstk, TYPE_PROC);
	if (code != SUCCESS)
    	    {
	    tmmsg (PROCFAIL, "More than %d nested PROCS.",
	    	   "TAE-MAXNEST",MAXNEST);
       	    goto undo_both;
    	    }
    	}
    return (DO_SUCCESS);


undo_both:
    	tae_free (ipbpt);
    	tae_free ((*cpctx).srchblk);
    	return (DO_RETURN);
    }


/*
 *	logoff_do - perform LOGOFF command.
 *
 *	NOTE:
 *	Since under UNIX the exit handler is not automatically triggered
 *	at normal termination (exit) time, we explicitly terminate the
 *	asynchronously running jobs here.
 */

    FUNCTION CODE logoff_do (cpctx, npctx)

    struct CONTXT	*cpctx;		/* in:  containing proc context		*/
    struct CONTXT	*npctx;		/* in:  proc ctx for LOGOFF command	*/
    {
    IMPORT CODE 	cmd_mode;	/* current command mode			*/
    IMPORT CODE		run_type;	/* ASYNC or BATCH or INTER		*/
    TEXT		cmdstr[CMDLINSIZ+1];
    struct VARIABLE 	*sfi;

    sfi = lookex (&((*npctx).parmst), "$SFI");
    if (cmd_mode != NORMCMD)		/* only allowed in normal cmd mode */
    	{
    	tmmsg(PROCFAIL, "'LOGOFF' not available in proc interrupt mode.",
    		"TAE-BADCMD");
    	return(DO_CHECK);
    	}
#ifdef TAE_ASYNC
    if (run_type == ASYNC)
    	async_exit();			/* local async job termination	*/
#endif

#ifdef UNIX
    term_async();			/* terminate async offsprings	*/
#endif
    s_copy("slogoff", cmdstr);	/* execute SLOGOFF at local node */
    prccmd(cmdstr, cpctx);		/* execute SLOGOFF proc		*/
    disable_recvar ();		/* DISABLE-RECVAR               */
    term_ins();			/* installation exit		*/
    if ((*sfi).v_default)		/* if nothing explicit:		*/
        exit(TM_LOGOFF);		/* normal logoff		*/
    else
	exit (IVAL(*sfi,0));	/* set explicit $SFI    status  */
    }

/*
 *    NOP command.
 *
 *	This command ignores everything else on the command
 *	line and returns normal completion status.   The idea
 *	is that you can DEFCMD any proc to be NOP and 
 *	all occurences of the proc in procedures will produce
 *	no syntax errors and do nothing. 
 *
 *	In the command table, NOP is defined as Y_ANYSUB,
 *	so that any subcommand can be used (and ignored).
 *	This is so that you only have to define the base
 *	proc name to be NOP and subcommands don't cause 
 *	difficulty.
 *
 */

    FUNCTION CODE nop_do (cpctx, npctx)

    struct CONTXT	*cpctx;		/* in:  containing proc context		*/
    struct CONTXT	*npctx;		/* in:  proc ctx for LOGOFF command	*/
    {
    return (DO_CHECK);
    }

#ifdef TCL78			/* TCL 78 was withdrawn 12/84	*/
/*  ok_runtype - Return TRUE if one of the values of the variable matches
 *		 the qualifier run type.
 *
 *  Note:  We check the qualifier run type against the proc's run type, as
 *         specified in the variable.  Note, however, that if the qualifier
 *	   is INTERACTIVE it really means "synchronous", whereas if the proc's
 *	   run type is interactive, it really means interactive.
 */
    FUNCTION BOOL ok_runtype (v, qualst)

    struct VARIABLE	*v;		/* in: variable with allowable run types */
    struct SYMTAB	*qualst;	/* in: qualifier symbol table		*/

    {
    IMPORT CODE		run_type;	/* current run type of TM	*/

    COUNT		i;
    CODE		qual_run_type;  /* run type from qualifiers	*/
    TEXT		errstr[STRINGSIZ+1];  /* for error string	*/

/* Check each value in the variable vector to see if any one of them is the proc's run type */

    qual_run_type = runtype (qualst);		/* get runtype of proc 	*/
    if (qual_run_type == FAIL)
    	return (TRUE);				/* just pass it; implies qualst not formatted */
    for (i=0; i < (*v).v_count; i++)
    	{
    	if (s_equal (SVAL(*v,i), "ALL"))
    	    return (TRUE);
    	else if (s_lseq (SVAL(*v,i), "ASYNC") && qual_run_type == ASYNC)
    	    return (TRUE);
    	else if (s_lseq (SVAL(*v,i), "ASYNC") && qual_run_type == INTER)
    	    {
    	    if (run_type == ASYNC)		/* sync in async job */
    		return (TRUE);
    	    }
    	else if (s_lseq (SVAL(*v,i), "BATCH") && qual_run_type == BATCH)
    	    return (TRUE);
    	else if (s_lseq (SVAL(*v,i), "BATCH") && qual_run_type == INTER)
    	    {
    	    if (run_type == BATCH)		/* sync in batch job */
    		return (TRUE);
    	    }
    	else if (s_lseq (SVAL(*v,i), "INTERACTIVE") && qual_run_type == INTER)
    	    {
    	    if (run_type == INTER)		
    	    	return (TRUE);
    	    }
    	}
    s_copy (SVAL(*v,0), errstr);		/* form error string	*/
    for (i=1; i < (*v).v_count; i++)
    	{
    	s_append(", ", errstr);
    	s_append(SVAL(*v,i), errstr);
    	}
    tmmsg(PROCFAIL, "Inappropriate run type for this proc. Allowed: %s.",
    		"TAE-INAPRN", errstr);
    return (FALSE);
    }
#endif

/*	parm_select	Build a parm symbol table based on a variable list
 *
 *	Returns SUCCESS/FAIL
 */
    FUNCTION CODE parm_select(ctx, names, count, st)

    struct CONTXT	*ctx;		/* in: context from which to get variables	*/
    TEXT		*names[];	/* in: list of variable names		*/
    COUNT		count;		/* in: number of names			*/
    struct SYMTAB	*st;		/* in/out: symbol table to build	*/

    {
    COUNT		i;
    struct VARIABLE	*vold, *vnew, *lookex(), *allvar();
    CODE		code;

    for (i=0; i<count; i++)
    	{
        vold = usearch (names[i], ctx);			/* get variable	   */
    	if (vold == NULL)
    	    {
    	    tmmsg(PROCFAIL, "Reference to undefined variable '%s'.",
    				"TAE-UNDEFVAR", names[i]);
    	    return(FAIL);
    	    }
    	if ((vnew = allvar(st)) ==NULL)		/* allocate and link new variable	*/
    	    {
    	    overr();				/* allocation failure		*/
    	    return(FAIL);
    	    }
    	code  = vcopy(vold, vnew);		/* copy the variable structure	*/
	if ((*vold).v_class != V_PARM)		/* if a local parameter		*/
	    {
	    (*vnew).v_class   = V_PARM;		/* force to be a PARM		*/
	    (*vnew).v_keyword = FALSE;
	    (*vnew).v_iparm   = FALSE;
	    (*vnew).v_default = FALSE;
	    }
	(*vnew).v_default = TRUE;		/* current is now default	*/
    	}
    return(code);
    }

/*	parm_local - Build a local symbol table with _VIEWPDF(if decalred)
 *
 *	Returns SUCCESS/FAIL
 */
    FUNCTION CODE parm_local (ctx, st)

    struct CONTXT	*ctx;		/* in: context from which to get variables	*/
    struct SYMTAB	*st;		/* in/out: symbol table to build	*/

    {
    COUNT		i;
    struct VARIABLE	*vold, *vnew, *lookex(), *allvar();
    CODE		code;

        vold = usearch ("_VIEWPDF", ctx);			/* get variable	   */
    	if (vold == NULL)
    	    return(SUCCESS);

    	if ((vnew = allvar(st)) ==NULL)		/* allocate and link new variable	*/
    	    {
    	    overr();				/* allocation failure		*/
    	    return(FAIL);
    	    }
    	code  = vcopy(vold, vnew);		/* copy the variable structure	*/
	if ((*vold).v_class != V_LOCAL)		/* if a local parameter		*/
	    {
	    (*vnew).v_class   = V_LOCAL;		/* force to be a PARM		*/
	    (*vnew).v_keyword = FALSE;
	    (*vnew).v_iparm   = FALSE;
	    (*vnew).v_default = FALSE;
	    }
	(*vnew).v_default = TRUE;		/* current is now default	*/
    return(code);
    }

/*	parmset_do.   Perform PARMSET statement.
 */

    FUNCTION CODE parmset_do(cpctx, npctx)

    struct CONTXT	*cpctx;		/* in/out: ctx for containing proc */
    struct CONTXT	*npctx;		/* in:  context for PROCEDURE sttmt*/

    {
    struct VARIABLE	*v;
    CODE		code;

    if ((*cpctx).proctype == Y_UNKNOWN)		/* beginning of PDF */
    	{
        /*  create a local variable named _ACTION holding the desired action */
        v = lookex(&((*npctx).parmst), "ACTION");	
        addstr(&((*cpctx).locst), "_ACTION", 1, 1, (*v).v_cvp, V_LOCAL);
	(*cpctx).proctype = Y_PARMSET;
	(*cpctx).compiled = s_equal((*npctx).subcmd,"COMPILED") ? TRUE : FALSE;
	v = lookex(&((*npctx).parmst), "HELP");
	s_copy(SVAL(*v, 0), (*cpctx).help_spec);	
	setOptions (cpctx, npctx);
        return(DO_SUCCESS);
    	}
    /* here if PARMSET seen internal to another proc */
    code = int_proc (cpctx, npctx);
    return (code);
    }

/*
 *	proced_do - perform PROCEDURE statement (a PDF introductory statement).
 *
 *	If this is an internal proc, save the name and address
 */

    FUNCTION CODE proced_do (cpctx, npctx)

    struct CONTXT	*cpctx;		/* in/out: ctx for containing proc */
    struct CONTXT	*npctx;		/* in:  context for PROCEDURE sttmt*/

    {
    struct VARIABLE	*v;
    CODE		code;

    if ((*cpctx).proctype == Y_UNKNOWN)		/* beginning of PDF */
    	{
	(*cpctx).proctype = Y_PROCEDURE;
	(*cpctx).compiled = s_equal((*npctx).subcmd,"COMPILED") ? TRUE : FALSE;
	v = lookex(&((*npctx).parmst), "HELP");
	s_copy(SVAL(*v, 0), (*cpctx).help_spec);	
	setOptions (cpctx, npctx);
	return(DO_SUCCESS);
    	}
    /* here if PROCEDURE seen internal to another proc */
    code = int_proc (cpctx, npctx);
    return (code);
    }

/*
 *	process_do - perform PROCESS statement (a PDF introductory statement).
 */

    FUNCTION CODE process_do (cpctx, npctx)

    struct CONTXT	*cpctx;		/* in/out: ctx for containing proc	*/
    struct CONTXT	*npctx;		/* in:  context for the PROCESS sttmt	*/

    {
    struct VARIABLE	*v, *ve;		/* dynamic in-memory variables		*/
    CODE		code;

    if ((*cpctx).proctype == Y_UNKNOWN)		/* beginning of PDF */
    	{
	(*cpctx).proctype = Y_PROCESS;
	(*cpctx).compiled = s_equal((*npctx).subcmd,"COMPILED") ? TRUE : FALSE;
	ve  = lookex(&((*npctx).parmst), "EXECUTE");	
	s_copy(SVAL(*ve,0), (*cpctx).exe_spec);		/* save EXECUTE file spec */
    	v = lookex (&((*npctx).parmst), "NAME");
    	if (((*v).v_count != 0) && (NULLSTR(SVAL(*ve,0))))
    	    s_copy(SVAL(*v,0), (*cpctx).exe_spec);	/* NAME, no EXE (for int'l procs) */
	v = lookex(&((*npctx).parmst), "HELP");
	s_copy(SVAL(*v,0), (*cpctx).help_spec);		/* save help filespec text*/
	setOptions (cpctx, npctx);
        return(DO_SUCCESS);
    	}

    /* here if PROCESS seen internal to another proc */
    code = int_proc (cpctx, npctx);
    return (code);

    }

#ifdef XXXXXX	/* READ command deleted in light of noscreen tutor  */

NOTE: the call to UPDTAB() is now incorrect (3/2/84)

{0, "READ",	"",  Y_GENINTER,	I_NPM(ptread), ptread, 	read_do    },

    static struct RESIDVAR ptread[] =	/* READ PDF			*/
    	{
/* name    type      k  m maxc               size     dc val      dvp*/

"VARIABLE",V_STRING, 0, 1, INT_MAXVAL, F_Q_NAMESIZ,   0, NULL,  NULL,
"PROMPT",  V_STRING, 0, 1, 1,     STRINGSIZ,  1, NULL,  (GENPTR)null_string
        };

/*
 *	read_do - perform READ statement.
 */

    FUNCTION CODE read_do (procctx, cmdctx)

    struct CONTXT	*procctx;		/* in/out: enclosing	*/
    struct CONTXT	*cmdctx;		/* in:  cmd context	*/

    {
    IMPORT struct CONTXT primctx;		/* context of level 0	*/
    IMPORT struct ECB ecbi;			/* operator attention	*/

    struct SYNBLK	sb;
    TEXT		cmdstr[CMDLINSIZ+1];
    struct SYMTAB	symtab;			/* for all output vars	*/
    struct VARIABLE	*v, *prompt, *tv;
    TEXT		**names;
    CODE		code;
    static TEXT *attn_key[] = {"TAE-ATTN"};
    static TAEINT minus_one[] = -1;

    v = lookex (&(*cmdctx).parmst, "VARIABLE");
    names = (TEXT **) (*v).v_cvp;
    symtab.link = NULL;					/* local symtab	    */
    prompt = lookex (&(*cmdctx).parmst, "PROMPT");
    while (FOREVER)
        {						/* start with fresh */
        deltab (&symtab);				/* table each time: */
	code = parm_select (procctx, names, (*v).v_count, &symtab);
	if (code != SUCCESS)
	    goto exit;
	ini_status ();					/* clear $sfi/$skey*/
	getcln (SVAL(*prompt, 0), cmdstr);		/* read cmd line   */
	if (e_occur (&ecbi))				/* give user a way */
	    {						/* out		   */
            tmmsg(PROCFAIL, "READ terminated by attention.", "TAE-ATTN");
	    e_clear (&ecbi);				/* prevent TAE's   */
            t_attn (&ecbi);				/* interrupt mode  */
	    goto exit;
	    }
	code = substitute (cmdstr, CMDLINSIZ, &primctx, TRUE);
	history_save (cmdstr);				/* add to $LASTCMD */
	if (code != SUCCESS)				/* bad substitute  */
    	    continue;
	initok (&sb, cmdstr);				/* init syntax	   */
	code = updtab (&symtab, &sb);			/* translate values*/
	if (code == SUCCESS)	
	    break;
	}
    for (v=symtab.link; v != NULL; v=(*v).v_link)	/* apply values	    */
        {
	tv = search ((*v).v_name, procctx);		/* find target	    */
	code = set_value (tv, (*v).v_cvp, (*v).v_count);
	if (code != SUCCESS)
	    overr();
	}
exit:
    deltab (&symtab);		/* delete symbol table	*/
    return (DO_CHECK);
    }
#endif

/*
 *	return_do - perform RETURN statement.
 */

    FUNCTION CODE return_do (cpctx, npctx)

    struct CONTXT	*cpctx;		/* in/out: context of containing proc*/
    struct CONTXT	*npctx;		/* in:  ctx from RETURN cmd (null)   */

    {
    IMPORT struct VARIABLE *sfi_gbl;	/* pointer to $SFI variable		*/
    IMPORT struct VARIABLE *skey_gbl;	/* pointer to $SKEY variable		*/

    struct VARIABLE	*v;


    v = lookex(&(*npctx).parmst, "$SFI");
    if (!(*v).v_default)			/* if $SFI= present	*/
        IVAL(*sfi_gbl,0) =  IVAL(*v,0);	/* copy sfi to global	*/
    v = lookex(&(*npctx).parmst, "$SKEY");
    if (!(*v).v_default)			/* if $SKEY= present	*/
        set_string (skey_gbl, SVAL(*v, 0));
    return(DO_RETURN);
    }

/*	set_sub - mark context with subcommand found, and save full
 *	subcommand spelling in .subcmd and _SUBCMD local variable.
 */

    FUNCTION static VOID set_sub (ctx, fullname)

    struct CONTXT	*ctx;		/* in/out: proc context		*/
    TEXT		fullname[];	/* in:  full name of subcommand	*/

    {
    struct VARIABLE	*v;

    (*ctx).subfnd = TRUE;
    (*ctx).subact = TRUE;
    s_copy(fullname, (*ctx).subcmd);
    v = lookex(&(*ctx).locst, "_SUBCMD");
    set_string(v, fullname);
    return;
    }

/*
 *	stop_do.   STOP command terminates all levels.
 */

    FUNCTION CODE stop_do(pctx, cmdctx)

    struct CONTXT *pctx;
    struct CONTXT *cmdctx;

    {
    return (DO_STOP);
    }

/*
 *	subcmd_do - perform SUBCMD command.
 */

    FUNCTION CODE subcmd_do (cpctx, npctx)

    struct CONTXT	*cpctx;		/* in/out: context of containing proc	*/
    struct CONTXT	*npctx;		/* in:  context for SUBCMD command	*/

    {
    struct VARIABLE	*v;		/* variable in symbol table		*/
    struct SUBCMD	*s;		/* SUBCMD struct in chain		*/
    TEXT		tmpsub[SUBCMDSIZ+1];	/* subcommand string typed in	*/

    (*cpctx).subs = TRUE;			/* we have seen a subcmd	*/
    v   = lookex(&(*npctx).parmst, "NAME");	/* point to name variab from SUBCMD sttmt*/
    s = allsub(&(*cpctx).subptr);		/* new SUBCMD struct into table	*/
    if (s == NULL)
	{
	tmmsg(PROCFAIL, "TAE Monitor internal memory overflow.", "TAE-MEMOVR");
	return(DO_RETURN);
	}
    s_copy(SVAL(*v, 0), (*s).name);		/* fill in new struct		*/
    if (s_equal((*npctx).subcmd, "DEFAULT"))
	(*s).deflt = TRUE;
    else
	(*s).deflt = FALSE;
    if ((*cpctx).subblk)			/* if we're already in a SUBCMD block*/
	{
	tmmsg(PROCFAIL, "Nesting of subcommands not allowed.", "TAE-SUBNST");
	return(DO_RETURN);
	}
    (*cpctx).subblk = TRUE;			/* now we're in a SUBCMD block	*/
    if ((*cpctx).special == COMPILING)		/* if we're compiling a PDF	*/
	(*(*cpctx).comp).cursub = s;		/* remember current subc tab entry*/
    else if ((*cpctx).special != SUB_SEARCH)
	{
	s_copy((*cpctx).subcmd, tmpsub);
	if ((*cpctx).subfnd)			/* if correct subc already found*/
	    tmpsub[(*cpctx).subsiz] = EOS;	/* shorten to orig typed length*/
	    					/* for ambiguity check		*/
	if (s_equal((*npctx).subcmd, "DEFAULT")  &&  NULLSTR(tmpsub))
	    {
	    if (!(*cpctx).subfnd)
		set_sub(cpctx, SVAL(*v, 0));		/* mark found & save full spelling*/
	    }
	else if (!NULLSTR(tmpsub)  &&  s_lseq(tmpsub, SVAL(*v, 0)))	/* if caller specd SUBCMD blk*/
	    {
	    if ((*cpctx).subfnd)			/* if we've found this one before*/
		{
		tmmsg(SUCCESS,
		    "Subcommand '%s' specified twice or '%s' ambiguous.",
		    "TAE-SUBTWC", SVAL(*v, 0), tmpsub);
		(*cpctx).special = SUB_SEARCH;		/* so we can complete SUBCMD table*/
		return(DO_SUCCESS);
		}
	    set_sub(cpctx, SVAL(*v, 0));
	    }
	else
	    (*cpctx).subact = FALSE;
	}
    return(DO_SUCCESS);
    }

/*	switch_do.   Set $SWITCH from hex string.
 */

    FUNCTION CODE switch_do (procctx, cmdctx)

    struct CONTXT	*procctx;	/* in: proc context	*/
    struct CONTXT	*cmdctx;	/* in: cmd context	*/

    {
    IMPORT struct VARIABLE *switch_gbl;	/* pointer to $SWITCH variable */

    TAEINT		value;
    struct VARIABLE	*v;
    CODE		code;

    v = lookex (&(*cmdctx).parmst, "VALUE");
    code = s_sh2i (SVAL(*v,0), &value);
    if (code != SUCCESS)
        {
	tmmsg(PROCFAIL, "Invalid hexadecimal value.", "TAE-INVHEX");
	return (DO_CHECK);
	}
    IVAL(*switch_gbl,0) = value;
    return (DO_SUCCESS);
    }

/*
 * 	cmdmatch. Check if the input command matches with the defined command 
 *	in the given command string.
 *	Return code:	
 *
 *	SUCCESS, if same as or a substring ,longer than abbr., of defined cmd.
 *	FAIL,    if no match 
 *
 */

    FUNCTION  CODE  cmdmatch (incmd , defv, index)


    TEXT	incmd[];		/* in: input command		    */
    struct VARIABLE **defv;		/* out: $DEFCMD VARIABLE of match   */
    COUNT	*index;			/* out: index to matching string    */


    {
    IMPORT struct SYMTAB  glbtab;	/* TCL global symbol table	*/
    IMPORT TEXT s_table[];	/* lower to upper conversion	*/

    struct VARIABLE  *dv;		/* pointer to $DEFCMDi var	*/
    COUNT	i,j,dn;
    TEXT	cmd[STRINGSIZ+1];	/* command in upper case (no *) */  
    COUNT	len_cmd;		/* command length excluding *   */
    FAST TEXT	*ptr_cmd;		/* pointer into cmd		*/
    TEXT	*defstr;		/* pointer to defcmd string	*/
    TEXT	defcmd[STRINGSIZ+1];	/* command name to match with	*/
    COUNT     	len_defcmd;		/* length of match command	*/
    FAST TEXT	*ptr_defcmd;		/* working pointer into defcmd	*/
    COUNT	abblen;			/* abbr length of match command */
    static TEXT dname[] = "$DEFCMDi";
    

/*	The logic here is critical to TAE performance, so there
 *	are attempts to avoid calling functions when the
 *	work can be done in-line.
 */

    *index = -1;
    *defv = NULL;
    for (ptr_cmd = cmd;   *incmd != EOS; )
	*ptr_cmd++ = s_table [*incmd++];	/* make upper case	*/
    *ptr_cmd = EOS;				/* copy EOS		*/
    len_cmd = ptr_cmd - cmd;			/* calculate length 	*/
    for (dn=0; dn <= 9; dn++)
        {
	dname[sizeof(dname)-2] = '0' + dn;	/* build $DEFCMDi name	*/
	dv = lookex (&glbtab, dname);		/* find $DEFCMDi 	*/
	if (dv == NULL)
	    return (FAIL);			/* cannot find	 	*/
	*defv = dv;				/* set for caller	*/
	for (*index = 0; *index < (*dv).v_count; (*index)++)	
	    {
    	    defstr = SVAL(*dv, *index);		/* ptr to defined cmd	*/
	    if (cmd[0] != s_table [*defstr])	/* for speed		*/
	        continue;

	    /* unpack command name from this $DEFCMDi component into defcmd */
    	    ptr_defcmd = defcmd;			/* working ptr	*/
    	    abblen = 0;					/* abbrev length*/
	    for (; *defstr != '*' && *defstr != '=' && *defstr != EOS; abblen++)
		*ptr_defcmd++ = s_table [*defstr++];	/* copy in uppercase to cmd */
	    len_defcmd = abblen;			/* assume no abbreviation */
	    if (*defstr == '*')				/* if abbreviated	*/
		for (defstr++; *defstr != '=' && *defstr != EOS; len_defcmd++)
		    *ptr_defcmd++ = s_table [*defstr++];/* copy the rest	*/ 
	    *ptr_defcmd = EOS;				/* supply EOS		*/

	    /* check for match between cmd and defcmd			*/
	    if (len_cmd > len_defcmd || len_cmd < abblen)
		continue;				/* too small or too big to match    */
	    ptr_cmd = cmd;
	    ptr_defcmd = defcmd;
	    for (;  *ptr_cmd != EOS; ptr_cmd++, ptr_defcmd++)
		if (*ptr_cmd != *ptr_defcmd)		/* compare 	*/
		    break;
	    if (*ptr_cmd == EOS)			/* full compare?*/
		return (SUCCESS);
	    }
        }
    return(FAIL);				/* no match found	*/
    }

/*  cre_locstr - Create a local string variable, add it to symbol table
 *
 *  Note that valids are not accommodated; minc & maxc are set to 'count'
 *
 *  Returns SUCCESS, or, on allocation failure, FAIL (msg already printed)
 */
    FUNCTION CODE cre_locstr (symtab, name, count, size, vp)

    struct SYMTAB	*symtab;	/* in: the symbol table we're adding to */
    TEXT		name[];		/* in: name of variable		    */
    FUNINT		count;		/* in: value count		    */
    FUNINT		size;		/* in: max string size		    */
    GENPTR		vp;		/* in: pointer to vector of values  */

    {
    struct VARIABLE	*v;		/* new variable			    */
    CODE		code;

    v = allvar (symtab);
    if (v == NULL) goto alloc_err;
    s_bcopy (name, (*v).v_name, NAMESIZ);
    (*v).v_type = V_STRING;
    (*v).v_class = V_LOCAL;
    (*v).v_minc = (*v).v_maxc = (*v).v_count = count;
    (*v).v_size = size;
    (*v).v_cvp = allval(v);
    if ((*v).v_cvp == NULL) goto alloc_err1;
    code = cpy_val (V_STRING, vp, (*v).v_cvp, count);
    if (code != SUCCESS) goto alloc_err1;
    return (code);


alloc_err1:
    tae_free (v);

alloc_err:
    overr();
    return(FAIL);
    }

