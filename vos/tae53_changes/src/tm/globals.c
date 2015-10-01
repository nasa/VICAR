/******************************************************************************
 *	Copyright (c) 1990, 1991, 1992,
 *	National Aeronautics and Space Administration
 *	ALL RIGHTS RESERVED
 *
 *	The software (programs, data bases and/or documentation) on or in
 *	any media can not be reproduced, disclosed or used except under
 *	terms of the license between COSMIC and your organization.
 *****************************************************************************/

/* Global variables for the TAE monitor (TM).
 * Declares and initializes all TM-wide C globals.
 *
 * CHANGE LOG:
 *
 *	11-jul-83	Purged change log, deleted checkout records,
 *			and audited global definitions...jtm
 *	12-sep-83	Added last_gbl...palm
 *	27-sep-83	Fix comment for no DYNFLATCH, etc...peb
 *	18-oct-83	Fix spelling of pstd_fil and std_fil...palm
 *	28-oct-83	char_gbl...palm
 *	30-oct-83	Add prim_act...peb
 *	09-feb-84	Change 'scnflg' to 'menu_screen'...dm
 *	14-feb-84	Change runtyp name and make CODE...palm
 *	15-feb-84	Add menu_gbl, delete mcb_head, mnnest...dm
 *	27-feb-84	Delete defcmd_gbl...palm
 *	02-mar-84	Add async_request...palm
 *	26-apr-84	Add $xLIB x_gbl, $RUNTYPE run_gbl,
 *			$LOG log_gbl, $MESSAGE msg_gbl, $SESSION sess_gbl
 *			variables...ces
 *	08-jun-84	New _gbl for $AECHO and $BECHO...palm
 *	24-jun-84	New ASYNCPORT globals...palm
 *	06-aug-84	New job_file...palm
 *	09-aug-84	Initialize parent_runtype to TYPE_NONE...palm
 *	24-aug-84	Add help_out_term...nhe
 *	11-oct-84	Declare save_pstd as GLOBAL (bug fix at UNIX 1.2)...dm
 *	25-oct-84	Add lasthelp...lia
 *	29-oct-84	Add 'async_type' and 'remote' for rcjm...dm
 *	07-nov-84	Change lasthelp to prevproc and add prevsub...lia
 *	06-dec-84	Add one_pass flag for remote job processing...dm
 *	08-dec-84	New _gbl for $DYNTUT...lia
 *	18-dec-84	Define 'remote'  outside RCJM-only area...dm
 *
 ***************************************************************************
 * CHANGES MADE IN THE RCJM TREE:
 *
 *	06-feb-85	Add ecb_link for rcjm link abort check...dm
 *
 ***************************************************************************
 *
 *	25-jul-85	Fix UNIX lint error...dm
 *	01-aug-85	Delete RCJM ecb_link (due to link_abort redesign)...dm
 *	17-feb-86	Add int_enabled and host_enabled....palm
 *	16-jul-86	New parent_job and job_name...palm
 *	16-jul-86	New job_gbl...palm
 *	22-sep-86	Change HNAMESIZ to FSPECSIZ...palm
 *	28-mar-87	Add menuopt_gbl...palm
 *	12-aug-87	Add filever_gbl...palm
 *	24-mar-88	Apollo requires braces on structure initializer...ljn
 *	24-mar-88	Delete TAE_RCJM conditional...ljn
 *      07-apr-88       Added $TUTSELE...tpl
 *      02-jun-88       Added $TUTOPT...tpl
 *	23-may-90	Removed RCJM stuff by referring to old TAE_RCJM...ljn
 */

#include	"taeconf.inp"		/* TAE configuration definitions*/
#include	"fileinc.inp"		/* f_ package defs & structs	*/
#include	"syninc.inc"		/* syntax pkg defs & structs	*/
#include	"eventinc.inp"		/* event pkg defs & structs	*/
#include	"tmhost.inp"		/* host-dependent defs		*/
#include	"tminc.inc"		/* TM-only host-independent defs*/
#include	"asyncinc.inc"		/* Async processing defs	*/
#include "taeintproto.h"



#ifdef SESSION_LOG
#include	"sessinc.inc"		/* session logging structs & defs*/
#endif


/* start with the traditional global variables.
 */

    GLOBAL struct SFILE  stdo_fil={0};	/* standard output file context		*/
    GLOBAL struct SFILE  *pstdo_fil=NULL;/* pointer to std out file context	*/
    GLOBAL struct SFILE  *saved_pstd = NULL;	/* pointer to saved stdout	*/
    GLOBAL struct SFILE	 prcfil={0};	/* proc file context (for f_ functions)	*/
    GLOBAL struct SFILE	 *scr_file=NULL;	/* pointer to script file context*/
    GLOBAL struct SFILE  job_file;	/* primary input for async/batch	*/
    GLOBAL COUNT	 scpcnt=1;	/* number of cycles for script file	*/
    GLOBAL BOOL		 incl_cmd=FALSE; /* commands from include file ? 	*/
    GLOBAL CODE		 run_type;	/* INTER, ASYNC, or BATCH		*/
    GLOBAL struct SYMTAB glbtab={NULL};	/* global symbol table			*/
    GLOBAL struct ECB	 ecbi;		/* event block for operator interrupts	*/
    GLOBAL TEXT		 job_name[JOBNAMESIZ+1] = {0}; /* of this job      */
    GLOBAL TEXT		 parent_job[JOBNAMESIZ+1] = {0}; /* name of parent */
    GLOBAL CODE		 usermode;	/* MENUMODE or CMDMODE			*/
    GLOBAL CODE		 prim_act;	/* TUTOR_ACTIVATION or CMD_ACTIVATION...*/
    					/* primary level proc activation method	*/
    GLOBAL CODE		 termtype;	/* terminal type			*/
    GLOBAL COUNT	 termlines;	/* # lines on terminal screen		*/
    GLOBAL COUNT	 termcols;
    GLOBAL BOOL		 help_to_term;	/* TRUE if help output is forced to terminal	*/
    GLOBAL BOOL		 full_scr_help;	/* TRUE if help is full screen		*/
    GLOBAL CODE		 menu_screen = UNDEFINE;
    GLOBAL CODE		 parmlatch = NOLATCH;	/* parm latch flag: NOLATCH, RUNLATCH*/
    GLOBAL CODE		 help_proc = NOPROC;	/* previous help command flag:
    						 * NOPROC, SAVEPROC		*/

    GLOBAL struct CONTXT latchctx;	/* latched param proc context		*/
    GLOBAL struct CONTXT *curproc;	/* pointer to proc context		*/
    GLOBAL struct VARIABLE *aecho_gbl;  /* pointer to $AECHO variable		*/
    GLOBAL struct VARIABLE *becho_gbl;  /* pointer to $BECHO variable		*/
    GLOBAL struct VARIABLE *echo_gbl;   /* pointer to $ECHO variable		*/
    GLOBAL struct VARIABLE *filever_gbl;/* pointer to $FILEVER variable */
    GLOBAL struct VARIABLE *tutsel_gbl;/* pointer to $TUTSELE variable */
    GLOBAL struct VARIABLE *job_gbl;	/* pointer to $JOB variable		*/
    GLOBAL struct VARIABLE *last_gbl;	/* pointer to $LASTCMD variable		*/
    GLOBAL struct VARIABLE *prompt_gbl;	/* pointer to $PROMPT variable		*/
    GLOBAL struct VARIABLE *parent_gbl;	/* pointer to $PARENT variable		*/
    GLOBAL struct VARIABLE *sfi_gbl;	/* pointer to $SFI variable		*/
    GLOBAL struct VARIABLE *skey_gbl;	/* pointer to $SKEY variable		*/
    GLOBAL struct VARIABLE *switch_gbl;	/* pointer to $SWITCH variable		*/
    GLOBAL struct VARIABLE *tutor_gbl;	/* pointer to $TUTOR variable		*/
    GLOBAL struct VARIABLE *dyntut_gbl; /* pointer to $DYNTUT variable		*/
    GLOBAL struct VARIABLE *char_gbl;	/* pointer to $SYSCHAR variable		*/
    GLOBAL struct VARIABLE *menu_gbl;	/* pointer to $MENUS variable		*/
    GLOBAL struct VARIABLE *menuopt_gbl;/* pointer to $MENUOPT variable		*/
    GLOBAL struct VARIABLE *tutoropt_gbl;/* pointer to $TUTOPT variable		*/
    GLOBAL struct VARIABLE *apl_gbl;	/* pointer to $APLIB variable		*/
    GLOBAL struct VARIABLE *usrl_gbl;	/* pointer to $USERLIB variable		*/
    GLOBAL struct VARIABLE *sysl_gbl;	/* pointer to $SYSLIB variable		*/
    GLOBAL struct VARIABLE *run_gbl;	/* pointer to $RUNTYPE  variable		*/
    GLOBAL struct VARIABLE *log_gbl;	/* pointer to $LOG variable		*/
    GLOBAL struct VARIABLE *msg_gbl;	/* pointer to $MESSAGE variable		*/
    GLOBAL struct VARIABLE *sess_gbl;	/* pointer to $SESSION variable		*/
    

    GLOBAL TEXT   lastkey[KEYSIZ+1] = {""};	/* most recent message key	*/
    GLOBAL TEXT	  prevproc[FSPECSIZ+1] = {""};	/* most recent command or proc
    						 * entered for HELP		*/
    GLOBAL TEXT	  prevsub[SUBCMDSIZ+1] = {""};	/* most recent subcommand
    						 * entered for HELP		*/
    GLOBAL CODE		 cmd_mode;	/* type of command mode: NORMCMD, or	*/
    					/* DYNMODE or INTMODE			*/
    GLOBAL BOOL		remote = FALSE;		/* local or remote execution */
    GLOBAL BOOL		int_enabled = TRUE;	/* intrpt mode enabled	     */
    GLOBAL BOOL		host_enabled = TRUE;	/* host cmds enabled	     */


#ifdef SESSION_LOG
    GLOBAL struct SESSLOG sesslog={0};	/* for session logging			*/
#endif
#ifdef SESSION_LOG2
    GLOBAL struct SFILE sess2file;
#endif

/********************* ASYNC globals *******************************/

#ifdef TAE_ASYNC
    GLOBAL struct ACB  	*acb_head = NULL;  	/* pointer to acb list	    */
    GLOBAL COUNT	acbcnt = 0;		/* ACB count		    */
    GLOBAL struct SYMTAB asy_parmst = {NULL}; 	/* parms for async init command */
    GLOBAL TEXT		comm_file[FSPECSIZ+1]; 	/* monitor-to-mon comm'n file name */
    GLOBAL CODE		parent_runtype = TYPE_NONE; /* run_type of parent mon   */
    GLOBAL BOOL		async_request = FALSE; 	/* flag indicating a request is waiting */
#endif
